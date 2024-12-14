;; -*- lexical-binding: t; -*-

(use-package kubed
  :ensure t
  :init
  (keymap-global-set "C-c k" 'kubed-prefix-map))

(provide 'fn-ops)
