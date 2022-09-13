;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell scripts formatting ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets up exec-path-from shell                    ;;
;; https://github.com/purcell/exec-path-from-shell ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (memq window-system '(mac ns))
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
