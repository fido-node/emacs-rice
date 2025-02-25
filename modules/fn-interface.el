;; -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :font "Iosevka Comfy Motion" :height 130 :weight 'regular)

(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  :config
  (buffer-terminator-mode 1))

(use-package aggressive-indent)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package dashboard
  :ensure t
  ;; :defer t
  :config
  (setq dashboard-startup-banner 3)
  (setq dashboard-items '((recents   . 5)
			                    (projects . 15)))
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; vertically center content
  (setq dashboard-vertically-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)

  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-items
                                    dashboard-insert-init-info
                                    ))
  ;; (add-to-list 'dashboard-items '(agenda) t)
  ;; (setq dashboard-week-agenda t)

  :init
  (dashboard-setup-startup-hook)
  )

(use-package which-key
  :init
  (which-key-mode 1))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask" )
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       :on-nil
          treemacs-workspace-switch-cleanup        nil
          treemacs-no-png-images                   t
          treemacs-indent-guide-style 'block
          )

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 44)

    (treemacs-git-mode 'deferred)
    (treemacs-indent-guide-mode t)
    (treemacs-git-commit-diff-mode t)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    ;; (pcase (cons (not (null (executable-find "git")))
    ;;              (not (null treemacs-python-executable)))
    ;;   (`(t . t)
    ;;    (treemacs-git-mode 'deferred))
    ;;   (`(t . _)
    ;;    (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package spacious-padding
  ;; :after modus-theme
  :custom
  (spacious-padding-subtle-mode-line t)
  (spacious-padding-subtle-mode-line
   `(
     :mode-line-active 'default
     :mode-line-inactive vertical-border))

  :init

  (spacious-padding-mode)
  ;; (add-hook 'modus-themes-after-load-theme-hook 'spacious-padding-mode)
  )


(use-package vundo)

(use-package buffer-name-relative
  :init
  (buffer-name-relative-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


(use-package sideline
  :hook ((flycheck-mode . sideline-mode))
  :init
  (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
        sideline-backends-right-skip-current-line t  ; don't display on current line (right)
        sideline-order-left 'down                    ; or 'up
        sideline-order-right 'up                     ; or 'down
        sideline-format-left "%s   "                 ; format for left aligment
        sideline-format-right "   %s"                ; format for right aligment
        sideline-priority 100                        ; overlays' priority
        sideline-display-backend-name t))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup))

(use-package awesome-tab
  :config
  (setq awesome-tab-display-icon nil)
  (awesome-tab-mode t))

(use-package savehist
  :init
  (savehist-mode))

(use-package ediff
  :demand t
  :ensure nil
  )

(provide 'fn-interface)
