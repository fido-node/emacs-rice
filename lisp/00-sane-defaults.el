;; -*- lexical-binding: t -*-
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq uniquify-buffer-name-style 'forward)

(save-place-mode 1)

(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 0)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(show-paren-mode t)

(defun mode-line-render (left right)
  "Function to render the modeline LEFT to RIGHT."
  (concat left
          (propertize " " 'display `(space :align-to (- right ,(length right))))
          right))
(setq-default mode-line-format
     '((:eval
       (mode-line-render
       (format-mode-line (list
         (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                         'help-echo "Mode(s) menu"
                         'mouse-face 'mode-line-highlight
                         'local-map   mode-line-major-mode-keymap)
         " %b "
         (if (and buffer-file-name (buffer-modified-p))
             (propertize "(modified)" 'face `(:inherit face-faded)))))
       (format-mode-line
        (propertize "%4l:%2c" 'face `(:inherit face-faded)))))))
;;; -------------------------------------------------------------------


;;; -------------------------------------------------------------------

              
;;; Vertical window divider
;;; -------------------------------------------------------------------
(setq window-divider-default-right-width 3)
(setq window-divider-default-places 'right-only)
(window-divider-mode)


(setq backup-directory-alist '(("." . "~/.backups"))
      make-backup-files t     ; backup of a file the first time it is saved.
      backup-by-copying t     ; don't clobber symlinks
      version-control t       ; version numbers for backup files
      delete-old-versions t   ; delete excess backup files silently
      kept-old-versions 6     ; oldest versions to keep when a new numbered
                              ;  backup is made (default: 2)
      kept-new-versions 9     ; newest versions to keep when a new numbered
                              ;  backup is made (default: 2)
      auto-save-default t     ; auto-save every buffer that visits a file
      auto-save-timeout 20    ; number of seconds idle time before auto-save
                              ;  (default: 30)
      auto-save-interval 200)  ; number of keystrokes between auto-saves
                              ;  (default: 300)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))



(when (getenv "WAYLAND_DISPLAY")
  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
        (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste)
  )
(pixel-scroll-precision-mode)
