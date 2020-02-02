

(defun +org-set-purchase-price ()
  (interactive)
  (org-set-property "PRICE" (read-string "Price for purchase: "))
  (org-set-tags '("purchase"))
  )



(defun +org-add-tag ()
  (interactive)
  (counsel-org-tag)
  )



(defhydra hydra-inbox-processing (
				  :color teal
				  :title "Inbox Processing Actions")


  ;; set state
  ("s" org-todo)

  ;; set resources/contexts

  ;; set blockers 
  
  ;;tag
  ("t" org-tag-set-tags)


  ;; refile

  )
