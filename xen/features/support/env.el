(require 'f)

(defvar xen-support-path
  (f-dirname load-file-name))

(defvar xen-features-path
  (f-parent xen-support-path))

(defvar xen-root-path
  (f-parent xen-features-path))

(add-to-list 'load-path xen-root-path)

(require 'xen)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
