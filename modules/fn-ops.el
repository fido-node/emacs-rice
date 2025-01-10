;; -*- lexical-binding: t; -*-

(use-package kubed
  :ensure t
  :init
  (keymap-global-set "C-c k" 'kubed-prefix-map))

(use-package tramp
  :ensure t)

(provide 'fn-ops)
