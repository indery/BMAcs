
(use-package helpful)

(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

  )


;;;###autoload
(defun +emacs-lisp-lookup-documentation (thing)
  "Lookup THING with `helpful-variable' if it's a variable, `helpful-callable'
if it's callable, `apropos' otherwise."
  (cond ((when-let (module (+emacs-lisp--module-at-point))
           (doom/help-modules (car module) (cadr module))
           (when (eq major-mode 'org-mode)
             (with-demoted-errors "%s"
               (re-search-forward
                (if (caddr module)
                    "\\* Module Flags$"
                  "\\* Description$"))
               (when (caddr module)
                 (re-search-forward (format "=\\%s=" (caddr module))
                                    nil t))
               (when (invisible-p (point))
                 (org-show-hidden-entry))))
           'deferred))
        (thing (helpful-symbol (intern thing)))
        ((call-interactively #'helpful-at-point))))
