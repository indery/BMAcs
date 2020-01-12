(setq python-bin "/usr/local/bin/python3")
(use-package python
  :init
  (setq python-shell-interpreter python-bin)

  :config

  (defun +python/open-repl ()
    "Open the Python REPL."
    (interactive)
    (unless python-shell-interpreter
      (user-error "`python-shell-interpreter' isn't set"))
    (pop-to-buffer
     (process-buffer
      (if-let* ((pipenv (+python-executable-find "pipenv"))
		(pipenv-project (pipenv-project-p)))
          (let ((default-directory pipenv-project)
		(python-shell-interpreter-args
		 (format "run %s %s"
			 python-shell-interpreter
			 python-shell-interpreter-args))
		(python-shell-interpreter pipenv))
            (run-python nil nil t))
	(run-python nil nil t)))))

  (defvar +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info")
    "CLI arguments to initialize ipython with when `+python/open-ipython-repl' is
called.")

  (defvar +python-jupyter-repl-args '("--simple-prompt")
    "CLI arguments to initialize 'jupiter console %s' with when
`+python/open-ipython-repl' is called.")

;;;###autoload
  (defun +python/open-ipython-repl ()
    "Open an IPython REPL."
    (interactive)
    (let ((python-shell-interpreter (or (+python-executable-find "ipython") "ipython"))
          (python-shell-interpreter-args (string-join +python-ipython-repl-args " ")))
      (+python/open-repl)))

;;;###autoload
  (defun +python/open-jupyter-repl ()
    "Open a Jupyter console."
    (interactive)
    (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
    (let ((python-shell-interpreter (or (+python-executable-find "jupyter") "jupyter"))
          (python-shell-interpreter-args (format "console %s" (string-join +python-jupyter-repl-args " "))))
      (+python/open-repl)))

;;;###autoload
  (defun +python-executable-find (exe)
    "TODO"
    (if (file-name-absolute-p exe)
	(and (file-executable-p exe)
             exe)
      (let ((exe-root (format "bin/%s" exe)))
	(cond ((when python-shell-virtualenv-root
		 (let ((bin (expand-file-name exe-root python-shell-virtualenv-root)))
                   (if (file-exists-p bin) bin))))
              ((when (require 'conda nil t)
		 (let ((bin (expand-file-name (concat conda-env-current-name "/" exe-root)
                                              (conda-env-default-location))))
                   (if (file-executable-p bin) bin))))
              ((when-let (bin (projectile-locate-dominating-file default-directory "bin/python"))
		 (setq-local doom-modeline-python-executable (expand-file-name "bin/python" bin))))
              ((executable-find exe))))))

;;;###autoload
  (defun +python/optimize-imports ()
    "organize imports"
    (interactive)
    (pyimport-remove-unused)
    (py-isort-buffer))

  (set-repl-handler! 'python-mode #'+python/open-repl :persist t)
  ;;  (set-docsets! 'python-mode "Python 3" "NumPy" "SciPy")
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

  (general-define-key
   :states '(normal visual emacs movement)
   :prefix "SPC"
   "dd" 'hydra-debug-actions/body
   )

  )

(with-eval-after-load 'dap-mode
  (setq dap-python-executable python-bin)

  (require 'dap-python)



  )

;; EXAMPLE TEMPLATE
;; (dap-register-debug-template "Store Service Local"
;;			     (list
;;			      :name "Store Service Local"
;;			      :type "python"
;;			      :args "-i"
;;			      )
;;			     )


(add-to-list 'display-buffer-alist
	     `("*Python: .*"
	       (display-buffer-at-bottom)
	       (window-height . 20)
	       ))


(setenv "VIRTUALENVWRAPPER_PYTHON" "/usr/local/bin/python3") 
;;(use-package pip-requirements
;;  :hook (pip-requirements-mode . pip-requirements-auto-complete-setup)
;;  )


;;(use-package py-isort)

;;(use-package yapfify
;;  :after python
;;  :hook (python-mode . yapf-mode)
;;  )

;;(use-package pylookup)
