;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;; ;; Customizations relating to editing a buffer. ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Key binding to use "hippie expand" for text autocompletion ;;
;; ;; http://www.emacswiki.org/emacs/HippieExpand                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-/") 'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp-friendly hippie expand ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlights matching parenthesis ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight current line ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-hl-line-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive search key bindings. By default, C-s runs ;;
;; isearch-forward, so this swaps the bindings.          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't use hard tabs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-q should fill at 80 chars, not 75 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq fill-column 80)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; When you visit a file, point goes to the last place where it ;;
;; ;; was when you previously visited the same file.               ;;
;; ;; http://www.emacswiki.org/emacs/SavePlace                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Emacs can automatically create backup files. This tells Emacs to           ;;
;; ;; put all backups in ~/.emacs.d/backups. More info:                          ;;
;; ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;;;;;;;;;;;;;;
;; comments ;;
;;;;;;;;;;;;;;
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use 2 spaces for tabs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show trailing spaces ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default show-trailing-whitespace t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show empty lines at the end of the buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indicate-empty-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix weird os x kill error ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable built-in indent mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq electric-indent-mode nil)

;;;;;;;;;;;;;;;;;;;
;; indent guides ;;
;;;;;;;;;;;;;;;;;;;
(use-package highlight-indent-guides
:ensure t
:config (setq highlight-indent-guides-method 'bitmap)
:hook (prog-mode-hook . highlight-indent-guides-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'highlight-indent-guides)                       ;;
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode) ;;
;; (setq highlight-indent-guides-method 'bitmap)            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package "yaml-mode"
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  :hook (yaml-mode-hook . (lambda ()
                            ('highlight-indent-guides-mod
                             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unlike python-mode, this mode follows the Emacs convention of not ;;
;; binding the ENTER key to `newline-and-indent'.  To get this       ;;
;; behavior, add the key definition to `yaml-mode-hook':             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'yaml-mode-hook                                                               ;;
;;           #'(lambda ()                                                                  ;;
;;              'highlight-indent-guides-mod ;; yaml-mode is not catched by prog-mode-hook ;;
;;              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package "dockerfile-mode"
  :ensure t
  :config  (setq dockerfile-mode-command "podman"))
