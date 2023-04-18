;;; package --- Summary
;; Provides entry point when comfiguring in the ui
;;; Commentary:


;;; Code:

;;(setq inhibit-startup-message t)
(setq inhibit-splash-screen t) ;; emacs 22
(setq inhibit-startup-screen t)

(load "startup-message-ascii-art.el")

(defun choose-message () "Time to start coding ...")

(defun current-user ()
  "Work out who the curren user is."
  (car (split-string (shell-command-to-string "whoami") "\n")))
    
(setq initial-scratch-message
      "
;; Useful keyboard shortcuts:
;; C-h w command-name - tell you what a command is bound to keys
;; C-h k key-sequence - type a key and it tells you the command
;; M-m - back-to-indent - Takes you to the indented start of the line
;; C-M-v - Scroll OTHER window
")

(defun open-scratch-with (txt)
  "Open the scratch buffer with the specified text (as TXT)."
  (with-current-buffer "*scratch*"
    (insert txt)
    (goto-char (point-max))))

;;(open-scratch-with initial-scratch-message)

(provide 'startup-message)
;;; startup-message.el ends here
