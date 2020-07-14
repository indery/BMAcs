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

  (treemacs-modify-theme "doom-colors"
    :config
    (progn
      (treemacs-create-icon :icon (format "  %s\t" (all-the-icons-fileicon "terraform" :height 0.9 :v-adjust -0.1 :face 'all-the-icons-purple-alt)) :extensions ("tf"))

      )
    
    )


  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  ;;(load-theme 'doom-one t)  
  ;;(load-theme 'doom-one-light t)  

  ;;(load-theme 'doom-nord-light t)  
  ;;(load-theme 'doom-vibrant t)  
  ;;(load-theme 'doom-wilmersdorf t)
  ;;(load-theme 'doom-palenight t)
  ;;(load-theme 'doom-rouge t)

  )

(use-package auto-dark-emacs
  :straight (:host github :repo "LionyxML/auto-dark-emacs")
  :init
  ;;(setq auto-dark-emacs/dark-theme 'doom-one)
  ;;(setq auto-dark-emacs/light-theme 'doom-one-light)

  ;;(setq auto-dark-emacs/dark-theme 'doom-nord)
  ;;(setq auto-dark-emacs/light-theme 'doom-nord-light)

  ;;(setq auto-dark-emacs/dark-theme 'doom-opera)
  ;;(setq auto-dark-emacs/light-theme 'doom-opera-light)

  (setq auto-dark-emacs/dark-theme 'doom-solarized-dark)
  (setq auto-dark-emacs/light-theme 'doom-solarized-light)

  ;;(setq auto-dark-emacs/dark-theme 'doom-tomorrow-night)
  ;;(setq auto-dark-emacs/light-theme 'doom-tomorrow-day)

  )

(use-package solaire-mode
  :disabled
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  ;;  (solaire-mode-swap-bg) 

  (defun solaire-swap-bg ()
    (interactive)
    (solaire-mode-swap-bg)

    )

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
