;; -*- lexical-binding: t; -*-


(use-package general
  :init
  (general-define-key
   ;; "C-f" 'consult-line
   ;; "C-M-f" 'consult-ripgrep
   ;; "C-/" 'comment-line
   ;; "C-x b" 'consult-buffer
   ;; "C-x C-b" 'consult-projectile
   "C-M-<left>" 'pop-global-mark
   ;; "C-;" 'embark-act
   )
  )


(provide 'fn-bindings)
