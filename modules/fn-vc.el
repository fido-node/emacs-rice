;; -*- lexical-binding: t; -*-


(use-package editorconfig
  :ensure t
  :init
  (editorconfig-mode 1))

;; (use-package envrc)

(use-package direnv
  :config
  (direnv-mode))

(defvar user-cache-directory (concat user-emacs-directory "var/"))

(use-package projectile
  :demand t
  :init
  ;; ensure projectile saves its files in a nice location
  (setq projectile-cache-file
        (concat user-cache-directory "projectile.cache"))
  (setq projectile-known-projects-file
        (concat user-cache-directory "projectile-bookmarks.eld"))

  :config
  (projectile-mode 1)
  (setq projectile-globally-ignored-file-suffixes
        '(
          ;; unity stuff
          ".meta" ".unity" ".asset" ".mat" ".cginc" ".prefab"
          ".renderTexture" ".lighting" ".shadergraph" ".shadersubgraph"
          ".shader" ".sceneWithBuildSettings" ".hlsl" ".vfx"
          ;; images
          ".png" ".xcf" ".jpg" ".jpeg" ".tif"
          ;; fonts
          ".ttf"
          ;; misc
          ".pdf"
          ))
  (setq projectile-indexing-method 'hybrid)

  ;; :general
  ;; (leader-keys
  ;;  "p" 'projectile-command-map)
  )

(use-package treemacs-projectile)

(use-package magit)

(provide 'fn-vc)
