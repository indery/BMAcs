(use-package doom-themes
  
  :after all-the-icons 

  :init
  ;; TODO run 'all-the-icons-install-fonts if this is the first time this package has been loaded

					; setting reccomended from the doom-themes READE to prevent org-mode headline scaling from bleeding into the line numbers
  ;;(let ((height (face-attribute 'default :height)))
  ;;  ;; for all linum/nlinum users
  ;;  (set-face-attribute 'linum nil :height height)
  ;;  ;; only for `linum-relative' users:
  ;;  (set-face-attribute 'linum-relative-current-face nil :height height)
  ;;  ;; only for `nlinum-relative' users:
  ;;  (set-face-attribute 'nlinum-relative-current-face nil :height height))
  
  :config

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled


  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config) 

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  ;;(load-theme 'doom-one t)  
  ;;(load-theme 'doom-nord-light t)  
  (load-theme 'doom-nord t)  

  )

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg) 

  )


(use-package ns-auto-titlebar
  :straight (:host github :repo "purcell/ns-auto-titlebar")
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode))

  )

(use-package kurecolor
  
  )

(use-package rainbow-mode
  :config
  (rainbow-mode 1)
  )
