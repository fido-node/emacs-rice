;; -*- lexical-binding: t; -*-


(use-package auth-source-1password
  :custom
  (auth-source-1password-vault "Private")
  :init
  (auth-source-1password-enable)
  )

(provide 'fn-auth)
