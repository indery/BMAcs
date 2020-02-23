(use-package 
  elisp-format)

;; the edebug eval list buffer uses comment lines to deliniate list entries.
;; aggressive indent messes with these lines and causes all list entries to be interpreted as one entry
(add-to-list
 'aggressive-indent-dont-indent-if
 '(derived-mode-p 'edebug-eval-mode)
 )
