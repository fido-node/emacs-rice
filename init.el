;; Load early-init.el regardless of the way Emacs was started.
(require 'nano-defaults (expand-file-name "nano-defaults" user-emacs-directory))
(require 'nano-splash (expand-file-name "nano-splash" user-emacs-directory))
(require 'early-init (expand-file-name "early-init" user-emacs-directory))

;; Load no-littering.el before anything else to keep ~/.emacs.d/ tidy.
(use-package no-littering :straight t)


(setq create-lockfiles nil)
(no-littering-theme-backups)
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))


;; Expose the packages integrated into the config repository.
(add-to-list 'load-path (expand-file-name "vendor/" user-emacs-directory))

;; Load the machine-local custom.el, versioned separately.
;; Possibly absent.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Load the config management and maintenance helper library.
(require 'init-lib (expand-file-name "init-lib" user-emacs-directory))

;; Load all the config parts.
(load-numbered-parts (expand-file-name "lisp/" user-emacs-directory))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9a977ddae55e0e91c09952e96d614ae0be69727ea78ca145beea1aae01ac78d2"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
