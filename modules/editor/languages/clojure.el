


(use-package cider
  ;;  :ensure-system-package (
  ;;			  "adoptopenjdk8"
  ;;			  "clojure")

  :hook ( (clojure-mode . rainbow-delimiters-mode)
	  (clojure-mode . cider-mode)
	  (clojure-mode . company-mode)
	  )
  :init
  (setq cider-overlays-use-font-lock t)
  (setq cider-use-overlays t)


  )
