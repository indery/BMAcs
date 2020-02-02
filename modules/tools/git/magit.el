
(use-package magit)

(use-package evil-magit
  :after +evil
  )


(use-package forge
  :after magit
  )


(set-popup-rule!
  "^magit" :side 'right :slot -1 :size 0.35 :select t) 
