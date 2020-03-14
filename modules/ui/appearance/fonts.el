
(use-package hasklig-mode
  :init
  (set-face-attribute 'default nil
		      :family "Hasklig"
		      :height 140
		      :weight 'normal
		      :width 'normal)



  (define-globalized-minor-mode global-hasklig-mode
    hasklig-mode
    hasklig-mode
    )

  :config
  (global-hasklig-mode)
  )
