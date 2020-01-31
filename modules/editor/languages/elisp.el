

(use-package 
  elisp-slime-nav 
  ;;:hook ((emacs-lisp-mode . elisp-slime-nav-mode) ) 
  :config
  (setq +nav-goto-def-func (lambda () 
			     (elisp-slime-nav-find-elisp-thing-at-point (symbol-name
									 (symbol-at-point))))) 
  (setq +doc-at-point-func (lambda () 
			     (elisp-slime-nav-describe-elisp-thing-at-point (symbol-name
									     (symbol-at-point))))))

(use-package 
  elisp-format)

;; the edebug eval list buffer uses comment lines to deliniate list entries.
;; aggressive indent messes with these lines and causes all list entries to be interpreted as one entry
(add-to-list
 'aggressive-indent-dont-indent-if
 '(derived-mode-p 'edebug-eval-mode)
 )

(with-eval-after-load 'general
  (general-add-hook 
   'emacs-lisp-mode-hook
   (list
    #'eldoc-mode
    #'company-mode
    #'lisp-butt-mode
    #'rainbow-delimiters-mode
    #'highlight-quoted-mode
    #'hasklig-mode
    #'aggressive-indent-mode
    #'evil-cleverparens-mode
    #'smartparens-strict-mode
    #'which-function-mode
    ))
  )
   
(setq +doc-at-point-func 'describe-function-in-popup)

(provide '+elisp)
