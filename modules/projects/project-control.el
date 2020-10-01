

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create New Project



(defun create-new-project (project-root-path)
  (progn
    (make-directory project-root-path t)
    (make-empty-file (expand-file-name (concat project-root-path "/project.el")))
    )

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Notes 
(defun +projectile-open-project-notes-file ()

  (interactive)
  
  (with-current-buffer (find-file-noselect (expand-file-name (concat  (projectile-project-root) "/notes.org")))
    (rename-buffer (generate-new-buffer-name "*PROJECT NOTES*"))
    (pop-to-buffer (buffer-name))
    )

  )

(set-popup-rule!
  "\*PROJECT NOTES\*"
  :side 'right
  :slot -1
  :size 0.30
  :select t
;;  :quit 'other
  :autosave t
  )



;; TODO jump to project notes



;; TODO jump to project settings
