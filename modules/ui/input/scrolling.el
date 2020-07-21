;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
;;(setq scroll-step 1)


(global-font-lock-mode t)
(setq font-lock-maximum-decoration t
      font-lock-maximum-size nil)

(setq fast-lock-cache-directories `(,(concat emacs-config-root "font-lock-cache")))
