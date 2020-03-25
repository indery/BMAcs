(use-package plantuml-mode
  ;;:straight (:host github :repo "skuro/plantuml-mode")
  :init
  (setq plantuml-jar-path (expand-file-name (concat emacs-config-root "external_libraries/java/plantuml.jar")))

  (setq plantuml-default-exec-mode 'jar)

  (setq plantuml-output-type "utxt")

  )


(defun +plantuml-bootstrap ()
  (interactive)
  (plantuml-download-jar)

  )


(provide '+plantuml)
