;;; lsp.el --- support for language servers          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jeff Parsons

;; Author: Jeff Parsons <jeffp@MacBook-Pro-9.local>
;; Keywords: tools,

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq
   lsp-ui-sideline-show-symbol t
   lsp-ui-sideline-show-hover nil
   ) 

  (setq
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature t
   lsp-ui-doc-position 'top
   lsp-ui-doc-alignment 'window
   lsp-ui-doc-use-childframe t
   ) 

  )

(use-package company-lsp
  :after company
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  )

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)


(provide '+lsp)
