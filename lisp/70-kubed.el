
(use-package kubed
  :straight t
  :ensure t
  :config
  (keymap-global-set "C-c k" 'kubed-prefix-map)
  )
