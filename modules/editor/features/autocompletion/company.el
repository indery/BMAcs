(use-package company

  :init
  (setq company-idle-delay 0.1)   
  (setq company-minimum-prefix-length 1)

  :config
  
  (with-eval-after-load 'doom/keymapper
    ;; evil tries to over-write this - so RN we will just run this after evil loads
    (with-eval-after-load '+evil
      (map!
       :map 'company-active-map
       "TAB" 'company-complete-selection
       "<tab>" 'company-complete-selection
       "RET" 'company-complete-selection
       ))

    )

  )




(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode)) 
