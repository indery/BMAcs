(use-package plantuml-mode
  ;;:straight (:host github :repo "skuro/plantuml-mode")
  :init
  (setq plantuml-executable-path "/usr/local/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)

  (setq plantuml-output-type "utxt")

  )
