;; (use-package nix-lsp
;;   :ensure lsp-mode
;;   :after (lsp-mode)
;;   :demand t
;;   :custom
;;   (lsp-nix-nil-formatter ["nixfmt"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :mode "\\.nix\\'")
