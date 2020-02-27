
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
