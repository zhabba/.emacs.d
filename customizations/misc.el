;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;; Unclassified settings ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These settings relate to how emacs interacts with your operating system ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;;(setq desktop-load-locked-desktop "ask") ;; sometimes desktop is locked, ask if we want to load it.
;;(desktop-save-mode 1) ;; auto-save buffer state on close for a later time.
(setq abbrev-file-name "~/.emacs.d/abbrev_defs") ;; where to save auto-replace maps

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No need for ~ files when editing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; displays the key bindings following your                   ;;
;; currently entered incomplete command (a prefix) in a popup ;;
;; https://github.com/justbur/emacs-which-key                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpful.el                         ;;
;; https://github.com/Wilfred/helpful ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helpful
  :ensure t
  :config
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)
  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  (global-set-key (kbd "C-h C") #'helpful-command))

;;;;;;;;;;;;;;;;;;;;;;;
;; https://magit.vc/ ;;
;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forge                          ;;
;; https://github.com/magit/forge ;;
;; https://magit.vc/manual/forge/ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package forge
  :ensure t
  :after magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programmable tab-completion ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This informs Emacs about the latest versions of all packages, and ;;
;; makes them available for download.                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (not package-archive-contents)
  (package-refresh-contents t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-update packages                               ;;
;; https://github.com/rranelli/auto-package-update.el ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))
