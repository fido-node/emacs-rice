;; -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :font "BlexMono Nerd Font Mono" :height 130 :weight 'semi-light)

(use-package hydra)

(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit-input)
              :map minibuffer-local-map
              ("M-h" . vertico-directory-up))
  :custom
  (vertico-cycle t)

  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))

  :config
  (require 'vertico-directory)
  (vertico-mode))

(use-package corfu
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("TAB" . corfu-insert)
              ([tab] . corfu-insert)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)

  :config
  (global-corfu-mode 1)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :demand t
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        orderless-matching-styles '(orderless-literal orderless-regexp)
        completion-category-overrides
        '((file (styles partial-completion)))))

(use-package wgrep
  :after consult
  :hook (grep-mode . wgrep-setup))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         :map minibuffer-local-map
         ("C-r" . consult-history))

  :custom
  (consult-project-root-function #'fn/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)

  :config
  (defun fn/get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root))))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))

  :custom
  (consult-dir-project-list-function nil))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :config
  (marginalia-mode))

(use-package embark
  :after vertico
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         :map minibuffer-local-map
         ("C-d" . embark-act)
         :map embark-region-map
         ("D" . denote-region))

  :config
  ;; Remove the mixed indicator to prevent the popup from being displayed
  ;; automatically
  (delete #'embark-mixed-indicator embark-indicators)
  (add-to-list 'embark-indicators 'embark-minimal-indicator)

  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after embark)

(use-package aggressive-indent)


(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(use-package dashboard
  :ensure t
  ;; :defer t
  :init
  (setq dashboard-startup-banner 3)
  (setq dashboard-items '((recents   . 5)
			                    (projects . 5)))
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

  :config
  (dashboard-setup-startup-hook)
  )


(use-package which-key
  :config
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



(provide 'fn-interface)
