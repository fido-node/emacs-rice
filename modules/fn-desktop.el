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

(use-package desktop
  :defer
  ;; :hook (kill-emacs . desktop-save-in-desktop-dir)
  :config
  ;; (when (daemonp)
  ;;   (defun my/restore-desktop (frame)
  ;;     "Restores desktop and cancels hook after first frame opens."
  ;;     (with-selected-frame frame
  ;;       (desktop-save-mode 1)
  ;;       (desktop-read)
  ;;       (remove-hook 'after-make-frame-functions 'my/restore-desktop)))
  ;;   (add-hook 'after-make-frame-functions 'my/restore-desktop))
  (setq desktop-auto-save-timeout 300
        desktop-path `(,(dir-concat user-cache-directory "desktop"))
        desktop-dirname (dir-concat user-cache-directory "desktop")
        desktop-base-file-name "desktop"
        desktop-restore-forces-onscreen nil
        desktop-globals-to-clear nil
        desktop-load-locked-desktop t
        desktop-missing-file-warning nil
        desktop-restore-eager 20
        desktop-restore-frames t
        desktop-save 'ask-if-new))

(provide 'fn-desktop)
