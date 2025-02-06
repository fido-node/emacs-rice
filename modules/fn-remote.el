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



(provide 'fn-remote)
