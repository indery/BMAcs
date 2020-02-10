
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


(set-popup-rule!
  "^magit:" :side 'right :slot -1 :size 0.40 :select t) 
