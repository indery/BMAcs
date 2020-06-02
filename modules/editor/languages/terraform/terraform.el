(use-package terraform-mode)


(add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("/path/to/terraform-lsp/terraform-lsp" "-enable-log-file"))
                  :major-modes '(terraform-mode)
                  :server-id 'terraform-ls))

(add-hook 'terraform-mode-hook #'lsp)
