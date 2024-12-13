;;; -*- lexical-binding: t; -*-

;; Some snake oil during the startup.  Probably won't hurt and is
;; reverted right after the Emacs initialization finishes.
(setq inhibit-startup-message t)
(global-display-line-numbers-mode 1)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold ,gc-cons-threshold))
          'append)
(setq gc-cons-threshold most-positive-fixnum)


(defun bootstrap-straight ()
  "Install and configure straight.el"
  (setq straight-vc-git-default-clone-depth
        (if-let ((depth (getenv "EMACS_STRAIGHT_DEPTH")))
            (string-to-number depth)
          'full))
  (setq straight-check-for-modifications '(check-on-save find-when-checking)
        straight-build-dir (format "build-%s" emacs-version)
        straight-use-package-by-default t)
  (setq straight-host-usernames '((github . "vifon")))
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))


(defun bootstrap-use-package ()
  "Install use-package.el"
  (straight-use-package 'org)
  (setq use-package-enable-imenu-support t)
  (straight-use-package 'use-package)
  (straight-use-package 'diminish)

  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
  ;; (use-package auto-package-update
  ;;   :config
  ;;   (setq auto-package-update-delete-old-versions t)
  ;;   (setq auto-package-update-hide-results t)
  ;;   (auto-package-update-maybe)
  ;;   )
  )



(bootstrap-straight)
(bootstrap-use-package)



;; This is not needed in general, but my specific init.el tries to
;; explicitly load early-init.el if it wasn't loaded yet.
;; This `provide' is used to load it only once.
(provide 'early-init)
