;;; +init-check.el --- Check init.el in a throwaway daemon  -*- lexical-binding: t; -*-

;;; Commentary:

;; Starts a second, named Emacs daemon that loads init.el the same way
;; the primary daemon does, so a broken init.el can be caught before
;; restarting the primary. `+init-check' just reports errors,
;; `+init-check-interactive' opens a frame on the daemon for trying
;; things out.

;; The daemon is killed with SIGKILL rather than `kill-emacs', so
;; `kill-emacs-hook' doesn't get to overwrite savehist, recentf and
;; friends with the state of the throwaway instance.

;;; Code:

(require 'server)

(defvar +init-check-name "init-check"
  "Server name of the init check daemon.")

(defvar +init-check-timeout 180
  "Seconds to wait for the init check daemon to start up.")

(defconst +init-check--report-form
  '(list :done (or init-file-had-error
                   (and (bound-and-true-p elpaca-after-init-time) t))
         :init-error init-file-had-error
         :failed (and (fboundp 'elpaca--queued)
                      (cl-loop for (id . e) in (elpaca--queued)
                               when (eq (elpaca<-status e) 'failed) collect id))
         ;; Elpaca turns errors in package config into warnings.
         :warnings (when-let* ((buffer (get-buffer "*Warnings*")))
                     (with-current-buffer buffer (buffer-string))))
  "Form evaluated in the init check daemon to get its status.")

(defun +init-check--start ()
  "Start the init check daemon and return its process."
  (when (server-running-p +init-check-name)
    (user-error "An init check is already running"))
  (let ((buffer (with-current-buffer (get-buffer-create "*init-check*")
                  (erase-buffer)
                  (current-buffer))))
    (make-process
     :name "init-check"
     :buffer buffer
     :noquery t
     :connection-type 'pipe
     :command (list (expand-file-name invocation-name invocation-directory)
                    (concat "--fg-daemon=" +init-check-name)))))

(defun +init-check--wait (proc pred on-ready &optional deadline)
  "Poll PRED until it returns non-nil, then call ON-READY with the value.
Gives up if PROC dies or DEADLINE passes."
  (let ((deadline (or deadline (+ (float-time) +init-check-timeout)))
        (value (funcall pred)))
    (cond
     (value (funcall on-ready value))
     ((not (process-live-p proc))
      (+init-check--fail proc "daemon died"))
     ((> (float-time) deadline)
      (delete-process proc)
      (+init-check--fail proc "timed out"))
     (t (run-at-time 1 nil #'+init-check--wait proc pred on-ready deadline)))))

(defun +init-check--report ()
  "Return the status of the init check daemon once it's done starting."
  ;; The server isn't started until init.el has been loaded.
  (when-let* (((server-running-p +init-check-name))
              (report (ignore-errors
                        (server-eval-at +init-check-name +init-check--report-form)))
              ((plist-get report :done)))
    report))

(defun +init-check--fail (proc reason)
  "Show the output of PROC with REASON for failing."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert "\n--- init check failed: " reason "\n"))
  (display-buffer (process-buffer proc))
  (message "init.el check failed: %s" reason))

(defun +init-check--emacsclient ()
  "Return the emacsclient matching the running Emacs."
  (let ((file (expand-file-name "emacsclient" invocation-directory)))
    (cond
     ((file-executable-p file) file)
     ((executable-find "emacsclient"))
     (t (user-error "Can't find emacsclient")))))

(defun +init-check (&optional on-success)
  "Load init.el in a throwaway daemon and report any errors.
Calls ON-SUCCESS if init.el loaded without errors or warnings."
  (interactive)
  (let ((proc (+init-check--start)))
    (message "Checking init.el...")
    (+init-check--wait
     proc #'+init-check--report
     (lambda (report)
       (delete-process proc)
       (if (or (plist-get report :init-error)
               (plist-get report :failed)
               (plist-get report :warnings))
           (+init-check--fail proc (format "%S" report))
         (message "init.el OK")
         (when on-success
           (funcall on-success)))))))

(defun +init-check-interactive ()
  "Load init.el in a throwaway daemon and open a frame on it.
The daemon is killed when the frame is closed."
  (interactive)
  (unless (display-graphic-p)
    (user-error "Not in a graphical frame"))
  ;; The environment of the primary daemon is whatever systemd gave
  ;; it, which doesn't necessarily include a usable display. Frames
  ;; created by emacsclient carry the environment of the client, which
  ;; does, so use that for both daemon and client.
  (let* ((env (or (frame-parameter nil 'environment) process-environment))
         (proc (let ((process-environment env))
                 (+init-check--start))))
    (message "Starting init check daemon...")
    (+init-check--wait
     proc (lambda () (server-running-p +init-check-name))
     (lambda (_)
       ;; Without -n, emacsclient sticks around until the frame is
       ;; closed, which is our cue to kill the daemon.
       (let ((process-environment env))
         (make-process
          :name "init-check-client"
          :buffer (process-buffer proc)
          :noquery t
          :command (list (+init-check--emacsclient) "-s" +init-check-name "-c")
          :sentinel (lambda (client _event)
                      (unless (process-live-p client)
                        (delete-process proc)
                        (unless (zerop (process-exit-status client))
                          (+init-check--fail proc "emacsclient failed"))))))))))

(provide '+init-check)
;;; +init-check.el ends here
