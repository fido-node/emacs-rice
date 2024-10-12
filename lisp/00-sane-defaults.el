;; -*- lexical-binding: t -*-
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024) ;; 1mb

      uniquify-buffer-name-style 'forward
      visible-bell t
      ring-bell-function 'ignore
      window-divider-default-right-width 3
      window-divider-default-places 'right-only


      backup-directory-alist '(("." . "~/.backups"))
      make-backup-files t     ; backup of a file the first time it is saved.
      backup-by-copying t     ; don't clobber symlinks
      version-control t       ; version numbers for backup files
      delete-old-versions t   ; delete excess backup files silently
      kept-old-versions 6     ; oldest versions to keep when a new numbered
                                        ;  backup is made (default: 2)
      kept-new-versions 9     ; newest versions to keep when a new numbered
                                        ;  backup is made (default: 2)
      auto-save-default t     ; auto-save every buffer that visits a file
      auto-save-timeout 3    ; number of seconds idle time before auto-save
                                        ;  (default: 30)
      auto-save-interval 200   ; number of keystrokes between auto-saves
                                        ;  (default: 300)
      x-select-enable-primary t
      x-select-enable-clipboard t
      mouse-drag-copy-region t
      meow-cursor-type-default 'bar
      meow-cursor-type-motion 'bar
      meow-cursor-type-beacon 'bar
      meow-cursor-type-insert '(hbar . 2)
      )

;; Move global mode string to the tab-bar and hide tab close buttons
(setq tab-bar-close-button-show nil
      tab-bar-separator " "
      tab-bar-format '(tab-bar-format-menu-bar
                       tab-bar-format-tabs-groups
                       tab-bar-separator
                       tab-bar-format-align-right
                       tab-bar-format-global)
      )

;; Turn on the tab-bar
(tab-bar-mode 1)

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; No tabs
(setq-default indent-tabs-mode nil

              ;; Tab.space equivalence
              tab-width 4)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

(set-default 'cursor-type  '(hbar . 2))

(repeat-mode 1)                ;; Enable repeating key maps
(menu-bar-mode 0)              ;; Hide the menu bar
(tool-bar-mode 0)              ;; Hide the tool bar
(savehist-mode 1)              ;; Save minibuffer history
(scroll-bar-mode 0)            ;; Hide the scroll bar
(xterm-mouse-mode 1)           ;; Enable mouse events in terminal Emacs
(fido-vertical-mode 1)         ;; Improved vertical minibuffer completions
(column-number-mode 1)         ;; Show column number on mode line
(tab-bar-history-mode 1)       ;; Remember previous tab window configurations
(tool-bar-mode -1)
(auto-save-visited-mode 1)     ;; Auto-save files at an interval
(global-visual-line-mode 1)    ;; Visually wrap long lines in all buffers
(global-auto-revert-mode 1)    ;; Refresh buffers with changed local files
(blink-cursor-mode 0)
(show-paren-mode t)
(window-divider-mode)
(pixel-scroll-precision-mode)


;; Delete trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(setq display-buffer-alist
      '(("\\*\\(shell\\|.*term\\|.*eshell\\|help\\|compilation\\|Async Shell Command\\|Occur\\|xref\\).*\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)                  ; Popups go at the bottom
         (slot . 0)                       ; Use the first slot at the bottom
         (post-command-select-window . t) ; Select the window upon display
         (window-height . 0.3))))         ; 30% of the frame height
