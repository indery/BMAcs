(use-package company

  :init
  (setq company-idle-delay .6)   
  )

(with-eval-after-load 'doom/keymapper
  (map!
   :map'company-active-map
   "TAB" 'company-complete-selection
   "<tab>" 'company-complete-selection
   "RET" 'company-complete-selection
   )

  ) 

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode))
