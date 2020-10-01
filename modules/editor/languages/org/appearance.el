
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  )

(use-package org-pretty-table
  :straight ( :host github :repo "fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode)
  )


(use-package mixed-pitch
  :straight ( :host gitlab :repo "jabranham/mixed-pitch")
  :after doom-themes
  :hook (org-mode . mixed-pitch-mode)

  :custom-face
					;  (variable-pitch ((t (:family "Adobe Garamond Pro"))))
					;  (org-document-title ((t (:weight bold :height 1.5))))
					;  (org-done ((t (:strike-through t :weight bold))))
					;  (org-headline-done ((t (:strike-through t))))
					;  (org-level-1 ((t (:weight bold :height 1.34))))
					;  (org-level-2 ((t (:weight normal :height 1.32))))
					;  (org-level-3 ((t (:weight normal :height 1.3))))
					;  (org-image-actual-width '(600))
					;  (org-todo ((t (:inherit 'fixed-pitch))))

  :config
  (setq
   org-hide-emphasis-markers t
   org-hidden-keywords '(title)
   )

  
  (setq org-todo-keyword-faces `(
				 ("TODO" . ,(doom-color 'red))
				 ("DONE" . ,(doom-color 'grey))
				 ("CANCELLED" . ,(doom-color 'grey))
				 ("WAITING" . ,(doom-color 'cyan))
				 ("BLOCKED" . ,(doom-color 'magenta))
				 )

	org-todo-keywords '(
                            (sequence "TODO(t)" "NEXT(n)" "BLOCKED(b)" "WAITING(w)" "|" "CANCELLED(c)" "DONE(d!)" )
                            (sequence "PROJECT(p)" "|" "PROJECT-COMPLETED(P)")
                            ))

  )

