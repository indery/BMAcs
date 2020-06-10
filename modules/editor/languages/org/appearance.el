
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  )

(use-package org-pretty-table
  :straight ( :host github :repo "fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode)
  )


(use-package mixed-pitch
  :straight ( :host gitlab :repo "jabranham/mixed-pitch")
  :hook (org-mode . mixed-pitch-mode)

  :custom-face
  (variable-pitch ((t (:family "EB Garamond"))))
  (org-document-title ((t (:weight bold :height 1.5))))
  (org-done ((t (:strike-through t :weight bold))))
  (org-headline-done ((t (:strike-through t))))
  (org-level-1 ((t (:weight bold :height 1.3))))
  (org-level-2 ((t (:weight normal :height 1.2))))
  (org-level-3 ((t (:weight normal :height 1.1))))
  (org-image-actual-width '(600))
  (org-todo ((t (:inherit 'fixed-pitch))))

  :config
  (setq
   org-hide-emphasis-markers t
   )
  )


(with-eval-after-load 'org
  ;; blurb to hide the "+#TITLE" keyword and some others
  
  (defvar my-org-hidden-keywords
    '(title author date email tags options))

  (defun org-hide-keywords ()
    (save-excursion
      (let (beg end ov)
	(goto-char (point-min))
	(while (re-search-forward
		(concat "\\(^[ \t]*#\\+\\)\\("
			(mapconcat (lambda (kw)
                                     (format "%s:\s"(symbol-name kw)))
                                   my-org-hidden-keywords "\\|")
			"\\)")
		nil t)
          (setq beg (match-beginning 1)
		end (match-end 2)
		ov  (make-overlay beg end))
	  (overlay-put ov 'invisible t)))))

  (add-hook 'org-mode-hook 'org-hide-keywords)
  )
