(use-package editorconfig
  :ensure t
  :init
  (editorconfig-mode 1))

(use-package format-all
  :ensure t
  :init
  (format-all-mode))

(use-package envrc
  :hook (after-init . envrc-global-mode))
