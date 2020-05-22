
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
  )

(use-package magit-org-todos
  :config
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-org-todos-insert-org-todos
   nil
   'append
   )
  )   

(set-popup-rule!
  "^magit:" :side 'right :slot -1 :size 0.40 :select t) 
