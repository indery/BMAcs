

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  )

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))
