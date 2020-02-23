
(use-package elisp-def

  )

;;;###autoload
(defun +emacs-lisp-lookup-definition (thing)
  "Lookup definition of THING."
  (if-let (module (+emacs-lisp--module-at-point))
      (doom/help-modules (car module) (cadr module) 'visit-dir)
    (call-interactively #'elisp-def)))
