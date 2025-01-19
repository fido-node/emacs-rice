
;; -*- lexical-binding: t; -*-


(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))


(use-package apheleia
  :defer t
  :bind ("C-c t a" . apheleia-mode)
  :init (apheleia-global-mode)
  :config
  ;; Set custom formatting commands
  (dolist (formatter-cmd '(
                           (shfmt     . ("shfmt" "-i" "4" "-ci" "-kp" "-sr"))


                           ))
    (add-to-list #'apheleia-formatters formatter-cmd))

  ;; Set custom formatters for modes
  (dolist (formatter-mode '((emacs-lisp-mode . lisp-indent)
                            (clojure-mode    . lisp-indent)

                            ))
    (add-to-list #'apheleia-mode-alist formatter-mode)))


;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

(use-package ripgrep)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package lsp-mode
  :straight (:type built-in)
  ;; Optional - enable lsp-mode automatically in scala files
  ;; You could also swap out lsp for lsp-deffered in order to defer loading
  :hook
  (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :init


  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)


  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; Optionally configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    ;; Optionally configure the cape-capf-buster.

    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
    )

  :init
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :none)
  ;; (setq lsp-completion-enable nil)
  (setq lsp-modeline-code-actions-segments '(count name))
  (setq lsp-headerline-breadcrumb-segments '(path symbols))
  (setq lsp-prefer-flymake t)
  ;; Makes LSP shutdown the metals server when all buffers in the project are closed.
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-keep-workspace-alive
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-auto-execute-action nil)
  (setq lsp-nix-nil-formatter ["nixfmt"])
  (setq lsp-file-watch-threshold 5000)

  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.ammonite\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.ivy2\\'")
    ))

(use-package lsp-ui
  :init
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-update-mode 'line)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-side 'right)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-doc-delay 3)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse t))

(use-package lsp-treemacs
  :init
  (lsp-treemacs-sync-mode 1))

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package nix-lsp
  :straight (:type built-in)
  :after (lsp-mode)
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :mode "\\.nix\\'")

(use-package company-nixos-options
  :after (company-mode)
  :custom
  (add-to-list 'company-backends 'company-nixos-options))

;; Add metals backend for lsp-mode
(use-package
  lsp-metals
  ;; :straight (:type built-in)
  )

(use-package dockerfile-mode
  :init
  (require 'dockerfile-mode))

(use-package  tree-sitter
  :init
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))


(provide 'fn-coding)
