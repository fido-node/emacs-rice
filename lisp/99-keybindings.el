

(use-package bind-key)

(require 'meow)
(require 'bind-key)

  ;; :bind
  ;; (("C-." . embark-act)         ;; pick some comfortable binding
  ;;  ("C-;" . embark-dwim)        ;; good alternative: M-.
  ;;  ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'



  ;; :bind (;; C-c bindings in `mode-specific-map'
  ;;        ("C-c M-x" . consult-mode-command)
  ;;        ("C-c h" . consult-history)
  ;;        ("C-c k" . consult-kmacro)
  ;;        ("C-c m" . consult-man)
  ;;        ("C-c i" . consult-info)
  ;;        ([remap Info-search] . consult-info)
  ;;        ;; C-x bindings in `ctl-x-map'
  ;;        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ;;        ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ;;        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ;;        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ;;        ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
  ;;        ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ;;        ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
  ;;        ;; Custom M-# bindings for fast register access
  ;;        ("M-#" . consult-register-load)
  ;;        ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ;;        ("C-M-#" . consult-register)
  ;;        ;; Other custom bindings
  ;;        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
  ;;        ;; M-g bindings in `goto-map'
  ;;        ("M-g e" . consult-compile-error)
  ;;        ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
  ;;        ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ;;        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ;;        ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ;;        ("M-g m" . consult-mark)
  ;;        ("M-g k" . consult-global-mark)
  ;;        ("M-g i" . consult-imenu)
  ;;        ("M-g I" . consult-imenu-multi)
  ;;        ;; M-s bindings in `search-map'
  ;;        ("M-s d" . consult-find)                  ;; Alternative: consult-fd
  ;;        ("M-s c" . consult-locate)
  ;;        ("M-s g" . consult-grep)
  ;;        ("M-s G" . consult-git-grep)
  ;;        ("M-s r" . consult-ripgrep)
  ;;        ("M-s l" . consult-line)
  ;;        ("M-s L" . consult-line-multi)
  ;;        ("M-s k" . consult-keep-lines)
  ;;        ("M-s u" . consult-focus-lines)
  ;;        ;; Isearch integration
  ;;        ("M-s e" . consult-isearch-history)
  ;;        :map isearch-mode-map
  ;;        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
  ;;        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
  ;;        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
  ;;        ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
  ;;        ;; Minibuffer history
  ;;        :map minibuffer-local-map
  ;;        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
  ;;        ("M-r" . consult-history))                ;; orig. previous-matching-history-element



  ;; :bind (
         ;; ("C-c p" . cape-prefix-map)
         ;; ) ;; Alternative keys: M-p, M-+, ...


  ;; :bind
  ;; ("C-<prior>" . centaur-tabs-backward)
  ;; ("C-<next>" . centaur-tabs-forward)


(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;; (meow-replace-state-name-list
;;  '(normal . "NORMAL")
;;  '(motion . "MOTION")
;;  '(keypad . "KEYPAD")
;;  '(insert . "INSERT")
;;  '(beacon . "BEACON"))
(meow-motion-overwrite-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 '("<escape>" . ignore))
(meow-leader-define-key
 ;; SPC j/k will run the original command in MOTION state.
 '("j" . "H-j")
 '("k" . "H-k")
 ;; Use SPC (0-9) for digit arguments.
 '("1" . meow-digit-argument)
 '("2" . meow-digit-argument)
 '("3" . meow-digit-argument)
 '("4" . meow-digit-argument)
 '("5" . meow-digit-argument)
 '("6" . meow-digit-argument)
 '("7" . meow-digit-argument)
 '("8" . meow-digit-argument)
 '("9" . meow-digit-argument)
 '("0" . meow-digit-argument)
 '("/" . meow-keypad-describe-key)
 '("?" . meow-cheatsheet))
(meow-normal-define-key
 '("0" . meow-expand-0)
 '("9" . meow-expand-9)
 '("8" . meow-expand-8)
 '("7" . meow-expand-7)
 '("6" . meow-expand-6)
 '("5" . meow-expand-5)
 '("4" . meow-expand-4)
 '("3" . meow-expand-3)
 '("2" . meow-expand-2)
 '("1" . meow-expand-1)
 '("-" . negative-argument)
 '(";" . meow-reverse)
 '("," . meow-inner-of-thing)
 '("." . meow-bounds-of-thing)
 '("[" . meow-beginning-of-thing)
 '("]" . meow-end-of-thing)
 '("a" . meow-append)
 '("A" . meow-open-below)
 '("b" . meow-back-word)
 '("B" . meow-back-symbol)
 '("c" . meow-change)
 '("d" . meow-delete)
 '("D" . meow-backward-delete)
 '("e" . meow-next-word)
 '("E" . meow-next-symbol)
 '("f" . meow-find)
 '("g" . meow-cancel-selection)
 '("G" . meow-grab)
 '("h" . meow-left)
 '("H" . meow-left-expand)
 '("i" . meow-insert)
 '("I" . meow-open-above)
 '("j" . meow-next)
 '("J" . meow-next-expand)
 '("k" . meow-prev)
 '("K" . meow-prev-expand)
 '("l" . meow-right)
 '("L" . meow-right-expand)
 '("m" . meow-join)
 '("n" . meow-search)
 '("o" . meow-block)
 '("O" . meow-to-block)
 '("p" . meow-yank)
 '("q" . meow-quit)
 '("Q" . meow-goto-line)
 '("r" . meow-replace)
 '("R" . meow-swap-grab)
 '("s" . meow-kill)
 '("t" . meow-till)
 '("u" . meow-undo)
 '("U" . meow-undo-in-selection)
 '("v" . meow-visit)
 '("w" . meow-mark-word)
 '("W" . meow-mark-symbol)
 '("x" . meow-line)
 '("X" . meow-goto-line)
 '("y" . meow-save)
 '("Y" . meow-sync-grab)
 '("z" . meow-pop-selection)
 '("'" . repeat)
 '("<escape>" . ignore))

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

 (defun consult-switch-buffer-kill ()
    "Kill candidate buffer at point within the minibuffer completion."
    (interactive)
    ; The vertico--candidate has a irregular char at the end.
    (let ((name  (substring (vertico--candidate) 0 -1)))
      (when (bufferp (get-buffer name))
        (kill-buffer name))))

(bind-keys :map minibuffer-local-map
  ("C-s" . consult-switch-buffer-kill)
  )

(bind-keys :map my-keys-minor-mode-map
  ("C-c C-f C-b" . consult-line)
  ("C-c C-f C-p" . consult-ripgrep)
  ("C-c C-t C-t" . treemacs-select-window)
  ("C-c C-t T"   . treemacs)
  ("C-c M-x" . consult-mode-command)
  ("C-c C-b C-b" . consult-buffer)
  ("C-c C-b C-s" . kill-buffer)
  ("C-c C-l C-s" . consult-lsp-symbols)
)


(my-keys-minor-mode 1)
