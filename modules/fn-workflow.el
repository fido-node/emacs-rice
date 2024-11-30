;; -*- lexical-binding: t; -*-

;;; ----- TODO Configuration -----

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d!)")))

(setq org-todo-keyword-faces
      '(("GOAL" . (:foreground "orange red" :weight bold))
        ("WAIT" . (:foreground "HotPink2" :weight bold))
        ("BACK" . (:foreground "MediumPurple3" :weight bold))))

;;; ----- Context Tags -----

(setq-default org-tag-alist
              '((:startgroup)
                ("Areas")
                (:grouptags)
                ("@home" . ?H)
                ("@work" . ?W)
                (:endgroup)

                (:startgrouptag . nil)
                ("Contexts")
                (:grouptags)
                ("@computer" . ?C)
                ("@mobile" . ?M)
                ("@calls" . ?A)
                ("@errands" . ?E)
                (:endgrouptag)

                ;; Task Types
                (:startgrouptag . nil)
                ("Types")
                (:grouptags)
                ("@easy" . ?e)
                ("@hacking" . ?h)
                ("@writing" . ?w)
                ("@creative" . ?v)
                ("@accounting" . ?a)
                ("@email" . ?m)
                ("@system" . ?s)
                (:endgrouptag)

                ;; Workflow states
                (:startgroup . nil)
                ("States")
                (:grouptags)
                ("@plan" . ?p)
                ("@review" . ?r)
                ("@followup" . ?f)
                (:endgroup)))


;; Only make context tags inheritable (what about noexport?)
(setq org-use-tag-inheritance "^@")

;;; ----- Time Tracking -----

;; Clock in on the current task when setting a timer
(add-hook 'org-timer-set-hook #'org-clock-in)

;; Clock out of the current task when the timer is complete
(add-hook 'org-timer-done-hook #'org-clock-out)

;;; ----- Agenda Configuration -----

(defvar fn/base-agenda-files '("Inbox.org" "Schedule.org")
  "The base agenda files that will always be included.")

(setq org-agenda-span 'day
      org-agenda-start-with-log-mode t
      org-agenda-files fn/base-agenda-files
      org-agenda-window-setup 'current-window)

;; Make done tasks show up in the agenda log
(setq org-log-done 'time
      org-log-into-drawer t)

;;; ----- Denote Integration -----

(defun fn/refresh-agenda-files ()
  (interactive)
  (setq org-agenda-files
        (append (denote-directory-files "_pra")
                fn/base-agenda-files)))

(defun fn/goto-weekly-note ()
  (interactive)
  (let* ((note-title (format-time-string "%Y-W%V"))
         (existing-notes
          (denote-directory-files (format "-%s" note-title) nil t)))
    (if existing-notes
        (find-file (car existing-notes))
      (denote note-title '("plw")))))

(with-eval-after-load 'denote
  ;; Quick access commands
  (keymap-set global-map "C-c n w" #'fn/goto-weekly-note)

  ;; Refresh agenda files the first time
  (fn/refresh-agenda-files)

  ;; Update agenda files after notes are created or renamed
  (add-hook 'denote-after-rename-file-hook #'fn/refresh-agenda-files)
  (add-hook 'denote-after-new-note-hook #'fn/refresh-agenda-files))


(use-package esxml  :ensure t)

(use-package ox-rss  :ensure t)

(provide 'fn-workflow)
