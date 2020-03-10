(use-package hl-todo
  :config

  (setq hl-todo-keyword-faces
	'(("HOLD" . "#d0bf8f")
	  ("TODO" . "#cc9393")
	  ("NEXT" . "#dca3a3")
	  ("THEM" . "#dc8cc3")
	  ("PROG" . "#7cb8bb")
	  ("OKAY" . "#7cb8bb")
	  ("MARK" . "#d0bf8f")
	  ("DONT" . "#5f7f5f")
	  ("FAIL" . "#8c5353")
	  ("DONE" . "#afd8af")
	  ("NOTE"   . "#d0bf8f")
	  ("KLUDGE" . "#d0bf8f")
	  ("HACK"   . "#d0bf8f")
	  ("TEMP"   . "#d0bf8f")
	  ("FIXME"  . "#cc9393")
	  ("XXX+"   . "#cc9393")))

  (global-hl-todo-mode 1))
