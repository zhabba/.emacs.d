p;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the custom init file for the auto customization savings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define and initialise package repositories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("tromey" . "https://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; marmalaid is dead currently
;;(add-to-list 'package-archives
;;              '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("elpa" . "https://elpa.gnu.org/packages/") t)
(package-initialize)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package to simplify the config file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package system-packages
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In case if lot of packages need to be installed  ;;
;; (defvar my-packages                              ;;
;;   '(                                             ;;
;;     some-package                                 ;;
;;     ))                                           ;;
;;                                                  ;;
;; (dolist (p my-packages)                          ;;
;;   (when (not (package-installed-p p))            ;;
;;     (ignore-errors (package-install p))))        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On OS X, an Emacs instance started from the graphical user    ;;
;; interface will have a different environment than a shell in a ;;
;; terminal window, because OS X does not run a shell during the ;;
;; login. Obviously this will lead to unexpected results when    ;;
;; calling external utilities like make from Emacs.              ;;
;; This library works around this problem by copying important   ;;
;; environment variables from the user's shell.                  ;;
;; https://github.com/purcell/exec-path-from-shell               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :ensure t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some packages require specific emacs versions, so provide a helper function for later. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun emacs-version-gt-p (at-least-major at-least-minor)
  (let* ((version-arr (split-string emacs-version "\\."))
         (major (string-to-number (nth 0 version-arr)))
         (minor (string-to-number (nth 1 version-arr))))
    (if (> major at-least-major)
        t
      (if (and (equal major at-least-major) (>= minor at-least-minor))
          t
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       ;;
;; ;; This is currently fairly ubuntu-specific,                                    ;;       ;;
;; ;; as that's the only operating system I run today.                             ;;       ;;
;; ;; Most package updates should be handled by :ensure-system-package,            ;;       ;;
;; ;; but there are some things that aren't for particularly new modes or similar. ;;       ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       ;;
;; (defvar my-system-packages                                                               ;;
;;  '(xclip ; clipboard tooling                                                             ;;
;;    ;; autojump ; quickly navigate directories in terminal                                ;;
;;   source-highlight ; makes my less & diff color coded in terminals                       ;;
;;    git htop w3m tree ; misc utilities                                                    ;;
;;    ;; gnuplot-x11 ; for graphs (used in blog and elsewhere)                              ;;
;;    ;; shellcheck ; used in flymake for shell scripts                                     ;;
;;    ;; imapfilter offlineimap lua5.3 luarocks ; email                                     ;;
;;    ;; x11-utils wmctrl xdotool ; used by thyme                                           ;;
;;    ;; fonts                                                                              ;;
;;    ;; unifont ttf-ancient-fonts fonts-inconsolata xfonts-terminus                        ;;
;;    ;; ttf-cascadia-code ttf-anonymous-pro fonts-hack-ttf                                 ;;
;;    ))                                                                                    ;;
;; (require 'subr-x)                                                                        ;;
;; (system-packages-install (string-join (mapcar 'symbol-name my-system-packages) " ") nil) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able ;;
;; to load them.                                                          ;;
;;                                                                        ;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,        ;;
;; then you can add the following code to this file:                      ;;
;;                                                                        ;;
;; (require 'yaml-mode)                                                   ;;
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))                ;;
;;                                                                        ;;
;; Adding this code will make Emacs enter yaml mode whenever you open     ;;
;; a .yml file                                                            ;;
;; (add-to-list 'load-path "~/.emacs.d/vendor")                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization                                                   ;;
;; Add a directory to our load path so that when you `load` things ;;
;; below, Emacs knows where to look for the corresponding file.    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/customizations")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These customizations change the way emacs looks and disable/enable ;;
;; some user interface elements                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "ui.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These customizations make it easier for you to navigate files, ;;
;; switch buffers, and choose options from the minibuffer.        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "navigation.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These customizations make editing a bit nicer. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "editing.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets up exec-path-from-shell so that Emacs will use the correct ;;
;; environment variables                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "shell-integration.el")

;;;;;;;;;;;;;;;;;;;;;;;
;; For editing lisps ;;
;;;;;;;;;;;;;;;;;;;;;;;
(load "elisp-editing.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hard-to-categorize customizations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "misc.el")
