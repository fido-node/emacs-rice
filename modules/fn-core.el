;; -*- lexical-binding: t; -*-


(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))


(use-package no-littering
  :demand t
  :config
  ;; Set the custom-file to a file that won't be tracked by Git
  (setq custom-file (if (boundp 'server-socket-dir)
                        (expand-file-name "custom.el" server-socket-dir)
                      (no-littering-expand-etc-file-name "custom.el")))
  (when (file-exists-p custom-file)
    (load custom-file t))

  ;; Don't litter project folders with backup files
  (let ((backup-dir (no-littering-expand-var-file-name "backup/")))
    (make-directory backup-dir t)
    (setq backup-directory-alist
          `(("\\`/tmp/" . nil)
            ("\\`/dev/shm/" . nil)
            ("." . ,backup-dir))))

  (setq auto-save-default nil)

  ;; Tidy up auto-save files
  (setq auto-save-default nil)
  (let ((auto-save-dir (no-littering-expand-var-file-name "auto-save/")))
    (make-directory auto-save-dir t)
    (setq auto-save-file-name-transforms
          `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
             ,(concat temporary-file-directory "\\2") t)
            ("\\`\\(/tmp\\|/dev/shm\\)\\([^/]*/\\)*\\(.*\\)\\'" "\\3")
            ("." ,auto-save-dir t)))))



;;; -- Native Compilation -----

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))




(setq-default fill-column 120)

(use-package minions
  :init
  (minions-mode))


;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(use-package alert)

(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  ;; (add-to-list 'super-save-predicates (lambda ()
  ;;                                       (not (eq major-mode 'mu4e-compose-mode))))
  )

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)


(use-package popper
  :bind (("C-M-'" . popper-toggle-latest)
         ("M-'" . popper-cycle)
         ("C-M-\"" . popper-toggle-type))
  :custom
  (popper-window-height 12)
  (popper-reference-buffers '(eshell-mode
                              vterm-mode
                              geiser-repl-mode
                              help-mode
                              grep-mode
                              helpful-mode
                              compilation-mode))
  :config
  (require 'popper) ;; Needed because I disabled autoloads
  (popper-mode 1))



;;; ----- Dired -----
(use-package all-the-icons)
(use-package all-the-icons-dired)
(use-package dired-ranger)

(defun fn/dired-mode-hook ()
  (interactive)
  (dired-hide-details-mode 1)
  (unless (or fn/is-termux
              (string-equal "/nix/store/" (expand-file-name default-directory)))
    (all-the-icons-dired-mode 1))
  (hl-line-mode 1))

(use-package dired
  :straight (:type built-in)
  :bind (:map dired-mode-map
              ("b" . dired-up-directory)
              ("H" . dired-hide-details-mode))
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-dwim-target 'dired-dwim-target-next
        dired-hide-details-hide-symlink-targets nil
        dired-kill-when-opening-new-dired-buffer t
        delete-by-moving-to-trash t)

  (add-hook 'dired-mode-hook #'fn/dired-mode-hook))

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;;; -- Make Help More Helpful -----

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind (([remap describe-function] . helpful-function)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)))


(provide 'fn-core)
