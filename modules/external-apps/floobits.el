
(use-package floobits
  :init
  (defvar floobits-workspaces nil)
  (defvar floobits-username nil)

  )



(defun floobits-join-existing-worksapce ()
  (interactive)

  (floobits-join-workspace
   (format "https://floobits.com/%s/%s/" 
	   floobits-username
	   (completing-read "Pick a Workspace..." floobits-workspaces))
   )

  )

