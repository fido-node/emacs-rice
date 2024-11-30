;; -*- lexical-binding: t; -*-

(use-package kubed
  :ensure t
  :config
  (keymap-global-set "C-c k" 'kubed-prefix-map))

(provide 'fn-ops)
