;; (use-package rg
;;   :config (rg-enable-default-bindings))

;; (use-package fzf
;;   :ensure t
;;   ;;:config
;;   ;; (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
;;   ;;       fzf/executable "fzf"
;;   ;;       fzf/git-grep-args "-i --line-number %s"
;;   ;;       ;; command used for `fzf-grep-*` functions
;;   ;;       ;; example usage for ripgrep:
;;   ;;       ;; fzf/grep-command "rg --no-heading -nH"
;;   ;;       fzf/grep-command "grep -nrH"
;;   ;;       ;; If nil, the fzf buffer will appear at the top of the window
;;   ;;       fzf/position-bottom t
;;   ;;       fzf/window-height 15)
;;   )


(use-package projectile
  :config
  (projectile-mode +1))

(use-package which-key
  :ensure t
  :config (which-key-mode))
