(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(add-to-exec-path (concat (getenv "GOPATH") "/bin"))

;;(setenv "GOROOT" "/usr/local/Cellar/go@1.12/1.12.17/libexec/")

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))


(use-package go-mode
  :after +env +lsp +flycheck
  :hook (
	 (go-mode . lsp-go-install-save-hooks)
	 (go-mode . lsp-deferred)
	 (go-mode . flycheck-mode)
	 (go-mode . company-mode)
	 (go-mode . yas-minor-mode)
	 )
  :init
  (setq
   lsp-before-save-edits t
   lsp-enable-on-type-formatting t
   lsp-enable-indentation t
   lsp-enable-snippet t
   lsp-prefer-flymake nil
   )

  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))

  )


(require 'dap-go)

(defun go-dev-env-mac-setup ()
  (progn
    (+eshell-run-command "brew install go")

    ;; LSP setup stuff
    (+eshell-run-command "go get -u github.com/golang/tools/gopls")

    ;; delve setup stuff
    (+eshell-run-command "brew install node") ;; dap mode needs this for some reason
    (+eshell-run-command "go get -u github.com/go-delve/delve/cmd/dlv")
    (+eshell-run-command "sudo /usr/sbin/DevToolsSecurity -enable")
    (dap-go-setup))
  )
