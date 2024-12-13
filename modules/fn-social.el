;; -*- lexical-binding: t; -*-

(use-package tracking)

(use-package telega
  :commands telega
  :config
  (setq telega-use-tracking-for '(or unmuted mention)
        telega-completing-read-function #'completing-read
        telega-msg-rainbow-title t
        telega-chat-fill-column 75))

(provide 'fn-social)
