;; -*- lexical-binding: t; -*-

;;; This file is generated from the Emacs.org file in my dotfiles repository!

;;; ----- Basic Configuration -----

;; Core settings
(setq ;; Flash the UI instead of beeping
 visible-bell nil

 ;; Yes, this is Emacs
 inhibit-startup-message t
 ;; inhibit-startup-screen t
 initial-scratch-message nil
 ;; initial-buffer-choice nil
 frame-title-format nil
 use-file-dialog nil

 indicate-empty-lines t

 ;; Instruct auto-save-mode to save to the current file, not a backup file
 auto-save-default nil
 auto-save-timeout 3    ; number of seconds idle time before auto-save
                                        ;  (default: 30)
 auto-save-interval 200   ; number of keystrokes between auto-saves
                                        ;  (default: 300)
 x-select-enable-primary t
 x-select-enable-clipboard t
 mouse-drag-copy-region t

 ;; No backup files, please
 make-backup-files nil

 ;; Make it easy to cycle through previous items in the mark ring
 set-mark-command-repeat-pop t

 ;; Don't warn on large files
 large-file-warning-threshold nil

 ;; Follow symlinks to VC-controlled files without warning
 ;; vc-follow-symlinks t

 ;; Don't warn on advice
 ad-redefinition-action 'accept

 ;; Revert Dired and other buffers
 global-auto-revert-non-file-buffers t

 ;; Silence compiler warnings as they can be pretty disruptive
 native-comp-async-report-warnings-errors nil)


;; No scroll bars
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

;; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(dolist (mode '(scroll-bar-mode
                horizontal-scroll-bar-mode
                menu-bar-mode
		            tooltip-mode
                tool-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))


;; Core modes
(repeat-mode 1)                ;; Enable repeating key maps
(menu-bar-mode 0)              ;; Hide the menu bar
(tool-bar-mode 0)              ;; Hide the tool bar
(savehist-mode 1)              ;; Save minibuffer history
(scroll-bar-mode 0)            ;; Hide the scroll bar
(xterm-mouse-mode 1)           ;; Enable mouse events in terminal Emacs
(display-time-mode 1)          ;; Display time in mode line / tab bar
(fido-vertical-mode 1)         ;; Improved vertical minibuffer completions
(column-number-mode 1)         ;; Show column number on mode line
(tab-bar-history-mode 1)       ;; Remember previous tab window configurations
(auto-save-visited-mode 1)     ;; Auto-save files at an interval
(global-visual-line-mode 1)    ;; Visually wrap long lines in all buffers
(global-auto-revert-mode 1)    ;; Refresh buffers with changed local files

(icomplete-mode 0)
;; (blink-cursor-mode 0)

(show-paren-mode t)

;; Tabs to spaces
(setq-default indent-tabs-mode nil
  	          tab-width 2)
(fset 'yes-or-no-p 'y-or-n-p)

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make icomplete slightly more convenient
;; (keymap-set icomplete-fido-mode-map "M-h" 'icomplete-fido-backward-updir)
;; (keymap-set icomplete-fido-mode-map "TAB" 'icomplete-force-complete)

;; Delete trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Move customization settings out of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t))

;; Match completion substrings that may be out of order
(defun fn/override-fido-completion-styles ()
  (setq-local completion-styles '(substring partial-completion emacs22)))

;; (add-hook 'icomplete-minibuffer-setup-hook 'fn/override-fido-completion-styles)


(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

(set-default 'cursor-type  '(hbar . 2))

;;; ----- System Identification -----

(defvar fn/is-termux
  (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(defvar fn/current-distro (or (and (eq system-type 'gnu/linux)
                                   (file-exists-p "/etc/os-release")
                                   (with-temp-buffer
                                     (insert-file-contents "/etc/os-release")
                                     (search-forward-regexp "^ID=\"?\\(.*\\)\"?$")
                                     (intern (or (match-string 1)
                                                 "unknown"))))
                              'unknown))

(defvar fn/is-guix-system (eql fn/current-distro 'guix))

;;; ----- Package Management -----

;; Automatically install packages (when not on Guix) but don't load
;; them until requested
(setq use-package-always-ensure (not fn/is-guix-system)
      use-package-always-defer t)

;;; ----- Configuration Management -----

(defvar fn/use-config-modules '()
  "A list of module symbols to load once init.el is finished.")

(defvar fn/common-config-modules '(
                                   fn-core
                                   fn-auth
                                   fn-present
                                   fn-writing
                                   fn-workflow
                                   fn-coding
                                   fn-interface
                                   fn-vc
                                   fn-bindings
                                   fn-social
                                   )
  "Configuration modules most commonly used across my machines.")



;; Add configuration modules to load path
(add-to-list 'load-path '"~/.config/emacs/modules")

;; Load system-specific configuration
(let ((config-path
       (format "~/.config/emacs/systems/%s.el" system-name)))
  (if (file-exists-p config-path)
      (load-file config-path)
    (message "No per-system configuration found for %s!" system-name)))




;;; ----- Appearance -----

(defun fn/set-terminal-title (title)
  (send-string-to-terminal (format "\e]0;%s\a" title)))

(defun fn/clear-background-color (&optional frame))
;; (interactive)
;; (or frame (setq frame (selected-frame)))
;; "unsets the background color in terminal mode"
;; (unless (display-graphic-p frame)
;; Set the terminal to a transparent version of the background color
;; (send-string-to-terminal
;; (format "\033]11;[90]%s\033\\"
;; (face-attribute 'default :background)))
;; (set-face-background 'default "unspecified-bg" frame)))

;; Clear the background color for transparent terminals
(unless (display-graphic-p)
  (add-hook 'after-make-frame-functions 'fn/clear-background-color)
  (add-hook 'window-setup-hook 'fn/clear-background-color)
  (add-hook 'ef-themes-post-load-hook 'fn/clear-background-color))

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-to-toggle '(modus-operandi-tinted))
  (modus-themes-common-palette-overrides
   `((bg-mode-line-active bg-lavender)
     (fg-mode-line-active fg-main)
     (border-mode-line-active bg-magenta-warmer)))
  :init
  (load-theme 'modus-operandi t)
  (add-hook 'modus-themes-after-load-theme-hook #'fn/clear-background-color))
;;
;; Make vertical window separators look nicer in terminal Emacs
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

;; Clean up the mode line
(setq-default mode-line-format
              '("%e" "  "
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                mode-line-format-right-align
                "  "
                (project-mode-line project-mode-line-format)
                " "
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                "  ")
              project-mode-line t
              mode-line-buffer-identification '(" %b")
              mode-line-position-column-line-format '(" %l:%c"))

;; Move global mode string to the tab-bar and hide tab close buttons
(setq tab-bar-close-button-show nil
      tab-bar-separator " "
      tab-bar-format '(tab-bar-format-menu-bar
                       tab-bar-format-tabs-groups
                       tab-bar-separator
                       tab-bar-format-align-right
                       tab-bar-format-global))

;; Turn on the tab-bar
(tab-bar-mode 1)

;; Customize time display
(setq display-time-load-average nil
      display-time-format "%H:%M %b %d W%U")

;; ----- Special Buffers as Popup Window -----

(setq display-buffer-alist
      '(("\\*\\(shell\\|.*term\\|.*eshell\\|help\\|compilation\\|Async Shell Command\\|Occur\\|xref\\).*\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)                  ; Popups go at the bottom
         (slot . 0)                       ; Use the first slot at the bottom
         (post-command-select-window . t) ; Select the window upon display
         (window-height . 0.3))))         ; 30% of the frame height

(defun fn/toggle-popup-window ()
  (interactive)
  (if-let ((popup-window
            (get-window-with-predicate
             (lambda (window)
               (eq (window-parameter window 'window-side)
                   'bottom)))))

      ;; Focus the window if it is not selected, otherwise close it
      (if (eq popup-window (selected-window))
          (delete-window popup-window)
        (select-window popup-window))

    ;; Find the most recent buffer that matches the rule and show it
    ;; NOTE: This logic is somewhat risky because it makes the assumption
    ;;       that the popup rule comes first in `display-buffer-alist'.
    ;;       I chose to do this because maintaining a separate variable
    ;;       for this rule meant I had to re-evaluate 2 different forms
    ;;       to update my rule list.
    (if-let ((popup-buffer
              (seq-find (lambda (buffer)
                          (buffer-match-p (caar display-buffer-alist)
                                          (buffer-name buffer)))
                        (if (project-current)
                            (project-buffers (project-current))
                          (buffer-list (selected-frame))))))
        (display-buffer popup-buffer (cdar display-buffer-alist))
      (message "No popup buffers found."))))

;; TODO: This binding may need to change
(keymap-global-set "C-c p" #'fn/toggle-popup-window)
(with-eval-after-load 'term
  (keymap-set term-raw-map "C-c p" #'fn/toggle-popup-window))

;;; ----- Essential Org Mode Configuration -----

(setq org-ellipsis " ▾"
      org-startup-folded 'content
      org-cycle-separator-lines 2
      org-fontify-quote-and-verse-blocks t)

;; Indent org-mode buffers for readability
(add-hook 'org-mode-hook #'org-indent-mode)

;; Set up Org Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

;; Use org-tempo
(use-package org-tempo
  :straight (:type built-in)
  :ensure nil
  :demand t
  :config
  (dolist (item '(("sh" . "src sh")
                  ("el" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("sc" . "src scheme")
                  ("ts" . "src typescript")
                  ("py" . "src python")
                  ("yaml" . "src yaml")
                  ("json" . "src json")
                  ("einit" . "src emacs-lisp :tangle emacs/init.el")
                  ("emodule" . "src emacs-lisp :tangle emacs/modules/dw-MODULE.el")))
    (add-to-list 'org-structure-template-alist item)))

;;; ----- Document Centering -----

(defvar center-document-desired-width 90
  "The desired width of a document centered in the window.")

(defun center-document--adjust-margins ()
  ;; Reset margins first before recalculating
  (set-window-parameter nil 'min-margins nil)
  (set-window-margins nil nil)

  ;; Adjust margins if the mode is on
  (when center-document-mode
    (let ((margin-width (max 0
			                       (truncate
			                        (/ (- (window-width)
				                            center-document-desired-width)
				                         2.0)))))
      (when (> margin-width 0)
	      (set-window-parameter nil 'min-margins '(0 . 0))
	      (set-window-margins nil margin-width margin-width)))))

(define-minor-mode center-document-mode
  "Toggle centered text layout in the current buffer."
  :lighter " Centered"
  :group 'editing
  (if center-document-mode
      (add-hook 'window-configuration-change-hook #'center-document--adjust-margins 'append 'local)
    (remove-hook 'window-configuration-change-hook #'center-document--adjust-margins 'local))
  (center-document--adjust-margins))

(add-hook 'org-mode-hook #'center-document-mode)

;; Make sure ripgrep is used everywhere
(setq xref-search-program 'ripgrep
      grep-command "rg -nS --noheading")

;; Load requested configuration modules
(dolist (module fn/use-config-modules)
  (require module))

(dashboard-open)
(treemacs-start-on-boot)
