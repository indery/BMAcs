;; Indentation of softwraped code
(use-package adaptive-wrap
  :ensure t
  :init (defun my-activate-adaptive-wrap-prefix-mode ()
	  "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
	  (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))
