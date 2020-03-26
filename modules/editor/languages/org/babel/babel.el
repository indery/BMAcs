
;;(setq org-babel-confirm-evaluate nil)
(setq org-confirm-babel-evaluate nil)


(defun +org-insert-code-block ()
  (org-insert-structure-template)
  )

(use-package ob-async
  )
