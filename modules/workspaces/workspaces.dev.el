
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finding workspaces
(defvar +workspaces-directories)

(defun +workspaces-list-all-workspaces ()

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; workspace CRUD actions
(defun +workspaces-create-workspace ()
  ;; TODO: Workspace create
  (interactive)
  (progn
    (treemacs-create-workspace)
    ;;TODO create workspace directory
    ;;TODO populate workspace with repos...
    )

  )

(defun +workspaces-switch-workspace ()
  (interactive)
  ;; TODO: Workspace switch
  ;;; change projectile 
  )

;; TODO: Workspace delete

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; workspace manage projects 

;; TODO: Workspace add project

;; TODO: Workspace delete project

;; TODO: Workspace sync project


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save restore, and manage workspace state

;; TODO: Workspace save buffers

;; TODO: Workspace save layout

;; TODO: Workpsace load layout

;; TODO: Workspace load buffers


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; workspace-scoped shortcuts/actions

;; TODO: Workspace jump to settings

;; TODO: Workspace jump to notes

;; TODO: Workspace jump to scratch file

;; TODO: Workspace run commands (custom keybindings)

;; TODO: Workspace org-capture
