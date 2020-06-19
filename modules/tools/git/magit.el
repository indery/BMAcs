
(use-package magit)

(use-package evil-magit
  :after +evil
  )


(use-package forge
  :after magit
  )

(use-package git-timemachine)


(use-package git-gutter
  :config
  (global-git-gutter-mode +1)

  )


(use-package magit-todos
  :straight (magit-todos :host github :repo "alphapapa/magit-todos")
  :config
  (setq magit-todos-section-heading "Inline TODOs")
  (setq magit-todos-fontify-keyword-headers t)
  (magit-todos-mode)
  )

(set-popup-rule!
  "^magit:"
  :side 'bottom
  :slot -1
  :size 0.50
  :select t
  ) 
