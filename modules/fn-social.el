;; -*- lexical-binding: t; -*-

(use-package tracking)

(use-package telega
  :commands telega
  :hook (
         (telega-mode . telega-notifications-mode)
         (telega-mode . telega-mode-line-mode)
         (telega-mode . telega-appindicator-mode)
         )
  :config
  (setq telega-use-tracking-for '(or unmuted mention)
        telega-completing-read-function #'completing-read
        telega-msg-rainbow-title t
        telega-use-docker t
        telega-chat-fill-column 75)
  (define-key global-map (kbd "C-c C-t") telega-prefix-map)
  (telega-notifications-mode 1)
  (telega-mode-line-mode 1)
  (telega-appindicator-mode 1)
  (telega-autoplay-mode 1)
  )

(use-package mastodon
  :config
  (setq mastodon-instance-url "https://mastodon.social"
        mastodon-active-user "fido_node")
  )

(provide 'fn-social)
