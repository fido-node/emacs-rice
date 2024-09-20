(use-package lsp-mode
  :straight t
  ;; Optional - enable lsp-mode automatically in scala files
  ;; You could also swap out lsp for lsp-deffered in order to defer loading
  :hook
  (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :init
   (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; Optionally configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    ;; Optionally configure the cape-capf-buster.
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider nil)
  (setq lsp-modeline-code-actions-segments '(count name))
  (setq lsp-headerline-breadcrumb-segments '(path symbols))
  (setq lsp-prefer-flymake t)
  ;; Makes LSP shutdown the metals server when all buffers in the project are closed.
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-keep-workspace-alive
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-auto-execute-action nil)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.bloop\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.metals\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.ammonite\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.ivy2\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.sbt\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]target\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]project/target\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]project/\\..+\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]dist\\'"))


  )

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-update-mode 'line)

  (setq lsp-ui-peek-enable t)

  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-side 'right)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-doc-delay 3)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse t)

  :straight t)


(use-package lsp-treemacs
  :straight t
  :init
  (lsp-treemacs-sync-mode 1)
  )

(use-package dap-mode
  :straight t)
