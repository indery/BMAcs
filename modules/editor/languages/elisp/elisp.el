
(with-eval-after-load 'general
  (general-add-hook 
   'emacs-lisp-mode-hook
   (list
    #'eldoc-mode
    #'company-mode
    #'rainbow-delimiters-mode
    #'highlight-quoted-mode
    #'aggressive-indent-mode
    #'evil-cleverparens-mode
    #'smartparens-strict-mode
    #'which-function-mode
    ))
  )
   
(setq +doc-at-point-func 'describe-function-in-popup)

(set-popup-rule!
  "^\\*edebug" :side 'right :size 0.25 :slot 1 :modeline t) 

(set-popup-rule!
  "^\\*scratch" :side 'right :size 0.25 :slot 2 :modeline t) 

(provide '+elisp)
