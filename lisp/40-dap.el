;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe
  :straight t)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :straight t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))
