
(use-package dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)


  ;; open the DAP-mode hydra whenever we hit a breakpoint, etc
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  )


(provide '+dap)
