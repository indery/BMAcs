(setq python-bin "/usr/local/bin/python3")
(use-package python
  :init
  (setq python-shell-interpreter python-bin)

  :config

  )


(use-package lsp-python-ms
  :init
  (setq  lsp-python-ms-python-executable-cmd python-bin)
  )

(use-package virtualenvwrapper)

(with-eval-after-load 'general
  (general-add-hook 
   'python-mode-hook
   (list
    #'lsp-deferred
    #'fringe-mode
    ))

  )

(with-eval-after-load 'dap-mode
  (setq dap-python-executable python-bin)

  (require 'dap-python)

  )

(add-to-list 'display-buffer-alist
	     `("*Python: .*"
	       (display-buffer-at-bottom)
	       (window-height . 20)
	       ))


(setenv "VIRTUALENVWRAPPER_PYTHON" "/usr/local/bin/python3") 
