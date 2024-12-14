;; -*- lexical-binding: t; -*-


(use-package general
  :config
  (general-define-key
   "C-f" 'amx                             ; or 'smex
   "C-s" 'counsel-grep-or-swiper)

  )

(provide 'fn-bindings)
