;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;; These customizations change the way emacs looks and disable/enable ;; ;;
;; ;; some user interface elements. Some useful customizations are       ;; ;;
;; ;; commented out, and begin with the line "CUSTOMIZE". These are more ;; ;;
;; ;; a matter of preference and may require some fiddling to match your ;; ;;
;; ;; preferences                                                        ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;
;; Simplify UI ;;
;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t)
(setq-default cursor-type 'box)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
;;(set-fring-mode 10)
(setq visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)

 ;;;;;;;;;;;;;;;;;;;;;;;
 ;; Show line numbers ;;
 ;;;;;;;;;;;;;;;;;;;;;;;
(column-number-mode)
(global-display-line-numbers-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show trailing spaces ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default show-trailing-whitespace t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show empty lines at the end of the buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indicate-empty-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable line numbers for specific modes  ;;
;; and don't highlight trailing whitespaces ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                shell-mode-hook
                dired-mode-hook
                ibuffer-mode-hook
                messages-buffer-mode-hook
                calendar-mode-hook
                org-agenda-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode -1)
                   (setq show-trailing-whitespace nil))))

;;;;;;;;;;;;;;;;;;
;; display time ;;
;;;;;;;;;;;;;;;;;;
;; (setq-default display-time-default-load-average nil)
;; (display-time-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set font and font size ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xzha/set-face-attribute ()
  (set-face-attribute 'default nil
                      :font "Iosevka Fixed Extended"
                      :height 190))


(add-hook 'server-after-make-frame-hook
              'xzha/set-face-attribute)

;; (if (daemonp)
;;     (add-hook 'server-after-make-frame-hook
;;               'xzha/set-face-attribute)
;;   (xzha/set-face-attribute))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation need by doom-modeline                                  ;;
;; https://github.com/domtronn/all-the-icons.el                        ;;
;; Note, if the fonts are weird, run `M-x all-the-icons-install-fonts` ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-height 28
        doom-modeline-vcs-max-length 16)
  :hook (after-init . doom-modeline-mode))

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-solarized-dark t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Underline position setting for X ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq x-underline-at-descent-line t)

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (add-to-list 'load-path "~/.emacs.d/themes")
;; (load-theme 'zenburn t)

;; increase font size for better readability
;; (set-face-attribute 'default nil :height 140)

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
;; (setq initial-frame-alist '((top . nil) (left . nil) (width . 177) (height . 53)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; No cursor blinking, it's distracting ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(blink-cursor-mode 0)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; full path in title bar ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default frame-title-format "%b (%f)")

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; don't pop up font menu ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "s-t") #'(lambda () (interactive)))

;; no bell
;;(setq ring-bell-function 'ignore)

;; Emacs has a few things that nearly everyone changes.
;; Minimize the chrome (emacs should be as command-line-like as possible),
;; lose the overly-verbose and annoying prompts, etc.
;; These are all documented inline.
;; (defalias 'qrr 'query-regexp-replace)
;; (setq initial-buffer-choice "~/Dropbox/docs/eng-log.org") ;; make the eng log the first file that's open.

(setq-default truncate-lines 1) ;; no wordwrap

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; colorize the output of the compilation mode. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  ;; mocha seems to output some non-standard control characters that
  ;; aren't recognized by ansi-color-apply-on-region, so we'll
  ;; manually convert these into the newlines they should be.
  (goto-char (point-min))
  (while (re-search-forward "\\[2K\\[0G" nil t)
    (progn
      (replace-match "
")))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
