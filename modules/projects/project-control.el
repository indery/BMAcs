



;; TODO create new project
(defun +projectile-open-project-notes-file ()

  (interactive)
  
  (with-current-buffer (find-file-noselect (expand-file-name (concat  (projectile-project-root) "/notes.org")))
    (rename-buffer "*PROJECT NOTES*")
    (pop-to-buffer (buffer-name))
    )

  )

(set-popup-rule!
  "\*PROJECT NOTES\*"
  :side 'right
  :slot -1
  :size 0.30
  :select t
  :quit 'other
  )



;; TODO jump to project notes



;; TODO jump to project settings
