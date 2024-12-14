;; -*- lexical-binding: t; -*-

;; Integrate with the system clipboard
(unless (display-graphic-p)
  (use-package xclip
    :demand t
    :init
    (xclip-mode 1)))

(use-package bluetooth)

;; Control NetworkManager via nmcli
(use-package nm
  :vc (:url "https://github.com/Kodkollektivet/emacs-nm"
            :rev :newest))


(use-package wakatime-mode
  :init
  (global-wakatime-mode))


(provide 'fn-desktop)
