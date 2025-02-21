;; -*- lexical-binding: t; -*-

(setq tramp-remote-path
      '(tramp-own-remote-path tramp-default-remote-path "/bin" "/usr/bin"
                              "/sbin" "/usr/sbin" "/usr/local/bin"
                              "/usr/local/sbin" "/local/bin"
                              "/local/freeware/bin" "/local/gnu/bin"
                              "/usr/freeware/bin" "/usr/pkg/bin"
                              "/usr/contrib/bin" "/opt/bin" "/opt/sbin"
                              "/opt/local/bin" "/opt/homebrew/bin"
                              "/opt/homebrew/sbin"
                              "/run/current-system/profile/bin"))



(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-completion-reread-directory-timeout nil)
  (tramp-default-remote-shell "bash")
  (tramp-encoding-shell "bash"))

(provide 'fn-remote)
