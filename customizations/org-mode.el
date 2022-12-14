;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;; Famous Org Mode      ;; ;;
;; ;; https://orgmode.org/ ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some helpers functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xzha/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Fixed Extended" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :ensure t
  :hook (org-mode . (lambda ()
                      (org-indent-mode 1)
                      (visual-line-mode 1)))
  :config
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

  (setq org-ellipsis " ⏎"
        org-directory (concat (getenv "HOME")  "/projects/org/")
        org-agenda-files `(,(concat org-directory "tasks.org")
                           ,(concat org-directory "meetings.org"))
        org-default-notes-file (concat org-directory "notes.org")
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WIP(w)"  "|" "DONE(d!)" "NONE(n!)")
          (sequence "REGULAR(r)" "ONETIME(o)"  "|" "ATTENDED (a!)")))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-refile-targets
        '(("archive-2022.org" :maxlevel . 1)
          ("tasks.org" :maxlevel . 1)))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-agenda-custom-commands
        '(("i" "In-progress tasks"
           ((todo "WIP"
                  ((org-agenda-overriding-header "Work-In-Progress Tasks")))))
          ("n" "No status tasks"
           ((todo "NONE"
                  ((org-agenda-overriding-header "Unknown status tasks")))))))

  (setq org-capture-templates
        '(("t" "Task add")
          ("tt" "Task" entry (file+olp "tasks.org" "Open")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("m" "Meeting add")
          ("mm" "Meeting" entry (file+olp "meetings.org" "Active")
           "* REGULAR %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal record add")
          ("jj" "Journal" entry
           (file+olp+datetree "journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)))

  (setq org-agenda-window-setup 'other-window)

  (xzha/org-font-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prettify org mode bullets ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prettify org files layout a litle bit ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package visual-fill-column
  :hook (org-mode . (lambda ()
                      (setq visual-fill-column-width 160
                            visual-fill-column-center-text t)
                      (visual-fill-column-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Roam                 ;;
;; https://www.orgroam.com/ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-roam
    :after org
    :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
    :custom
    (org-roam-directory (concat org-directory "/roam"))
    :config
    (org-roam-db-autosync-enable)
    :bind (("C-c n f" . org-roam-node-find)
           ("C-c n r" . org-roam-node-random)
           ("C-c n g" . org-roam-graph)
           (:map org-mode-map
                 (("C-c n i" . org-roam-node-insert)
                  ("C-c n o" . org-id-get-create)
                  ("C-c n t" . org-roam-tag-add)
                  ("C-c n a" . org-roam-alias-add)
                  ("C-c n l" . org-roam-buffer-toggle)))))
