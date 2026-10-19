;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require '+review)

(defvar +review-test-buffers nil
  "Buffers to kill after each test.")

(defun +review-test-file-buffer (file &optional content)
  "Return a buffer pretending to visit FILE, with CONTENT."
  (let ((buffer (generate-new-buffer (file-name-nondirectory file))))
    (push buffer +review-test-buffers)
    (with-current-buffer buffer
      (insert (or content ""))
      (setq buffer-file-name file
            default-directory (file-name-directory file))
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    buffer))

(defun +review-test-review-buffers ()
  "Return all review buffers."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (derived-mode-p '+review-comments-mode)))
              (buffer-list)))

(describe "+review-insert"
  (before-each
    (setq +review-test-buffers nil)
    ;; Root is the first two path components, i.e. /proj-a/.
    (spy-on 'project-current :and-call-fake
            (lambda (&rest _)
              (when (string-match "\\`/[^/]+/" default-directory)
                (cons 'transient (match-string 0 default-directory)))))
    (let ((ignore-window-parameters t))
      (delete-other-windows)))

  (after-each
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (mapc #'kill-buffer (+review-test-review-buffers))
    (mapc #'kill-buffer +review-test-buffers))

  (it "inserts a relative location and leaves point after it"
    (switch-to-buffer (+review-test-file-buffer "/proj-a/sub/foo.el" "one\ntwo\n"))
    (forward-line 1)
    (forward-char 2)
    (+review-insert)
    (expect (buffer-name) :to-equal "*proj-a review*")
    (expect major-mode :to-be '+review-comments-mode)
    (expect (buffer-string) :to-equal "sub/foo.el:2:3: ")
    (expect (point) :to-be (point-max)))

  (it "appends to the same buffer for files in the same project"
    (switch-to-buffer (+review-test-file-buffer "/proj-a/foo.el"))
    (+review-insert)
    (insert "first note")
    (other-window 1)
    (switch-to-buffer (+review-test-file-buffer "/proj-a/bar/baz.el"))
    (+review-insert)
    (expect (buffer-string) :to-equal "foo.el:1:1: first note\nbar/baz.el:1:1: ")
    (expect (length (+review-test-review-buffers)) :to-be 1))

  (it "uses separate buffers for separate projects"
    (switch-to-buffer (+review-test-file-buffer "/proj-a/foo.el"))
    (+review-insert)
    (let ((review-a (current-buffer)))
      (other-window 1)
      (switch-to-buffer (+review-test-file-buffer "/proj-b/foo.el"))
      (+review-insert)
      (expect (current-buffer) :not :to-be review-a)
      (expect (buffer-name) :to-equal "*proj-b review*")
      (expect (length (+review-test-review-buffers)) :to-be 2)))

  (it "creates a bottom window that survives delete-other-windows"
    (let ((code-window (selected-window)))
      (switch-to-buffer (+review-test-file-buffer "/proj-a/foo.el"))
      (+review-insert)
      (let ((review-window (selected-window)))
        (expect review-window :not :to-be code-window)
        (expect (window-parameter review-window 'no-delete-other-windows) :to-be t)
        (expect (window-in-direction 'above review-window) :to-be code-window)
        (select-window code-window)
        (delete-other-windows)
        (expect (window-live-p review-window) :to-be t))))

  (it "swaps the buffer of a visible review window from another project"
    (switch-to-buffer (+review-test-file-buffer "/proj-a/foo.el"))
    (+review-insert)
    (let ((review-window (selected-window))
          (window-count (count-windows)))
      (other-window 1)
      (switch-to-buffer (+review-test-file-buffer "/proj-b/foo.el"))
      (+review-insert)
      (expect (selected-window) :to-be review-window)
      (expect (count-windows) :to-be window-count)
      (expect (buffer-name) :to-equal "*proj-b review*")))

  (it "errors in buffers not visiting a file"
    (with-temp-buffer
      (expect (+review-insert) :to-throw 'user-error))))

(describe "+review-visit"
  :var (root review-buffer)
  (before-each
    (setq root (file-name-as-directory (make-temp-file "review" t)))
    (with-temp-file (expand-file-name "foo.el" root)
      (insert "one\ntwo three\n"))
    (setq review-buffer (generate-new-buffer "*review*"))
    (with-current-buffer review-buffer
      (+review-comments-mode)
      (setq +review--project-root root)
      (insert "foo.el:2:5: a note\n"))
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (split-window)
    (switch-to-buffer review-buffer)
    (goto-char (point-min)))

  (after-each
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (kill-buffer review-buffer)
    (when-let* ((buffer (find-buffer-visiting (expand-file-name "foo.el" root))))
      (kill-buffer buffer))
    (delete-directory root t))

  (it "binds RET to visit only on the location"
    (expect (key-binding (kbd "RET")) :to-be '+review-visit)
    (forward-char 10)
    (expect (key-binding (kbd "RET")) :to-be '+review-visit)
    (forward-char 2)
    (expect (key-binding (kbd "RET")) :not :to-be '+review-visit))

  (it "visits the location in the other window"
    (let ((review-window (selected-window)))
      (+review-visit)
      (expect (selected-window) :not :to-be review-window)
      (expect (window-buffer review-window) :to-be review-buffer)
      (expect (buffer-file-name) :to-equal (expand-file-name "foo.el" root))
      (expect (line-number-at-pos) :to-be 2)
      (expect (current-column) :to-be 4)
      (expect (count-windows) :to-be 2)))

  (it "errors when not on a location"
    (forward-char 14)
    (expect (+review-visit) :to-throw 'user-error)))

(describe "+review-comments-mode"
  (it "highlights locations like compilation-mode"
    (with-temp-buffer
      (+review-comments-mode)
      (insert "sub/foo.el:12:5: a note")
      (font-lock-ensure)
      (expect (get-text-property 1 'face) :to-be 'compilation-info)
      (expect (get-text-property 12 'face) :to-be 'compilation-line-number)
      (expect (get-text-property 15 'face) :to-be 'compilation-column-number)
      (expect (get-text-property 19 'face) :to-be nil))))
