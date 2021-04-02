;;; package --- Summary
;; Provides entry point when econfiguring in the terminal
;;; Commentary:

;;; Code:

(message "Running in terminal display mode")

(menu-bar-mode -1)

(show-paren-mode) ;; Highlight matching parentheses
(setq visible-bell 1) ;;To stop it making a bell noise...
(setq ring-bell-function 'ignore)

;; By default don't wrap lines
(setq-default truncate-lines t)

;; Display line and column numbers
(setq line-number-mode    t)
(setq column-number-mode  t)

;; Explicitly show the end of a buffer
(set-default 'indicate-empty-lines nil)

(global-font-lock-mode 1) ;; allows syntax highlighting to work
(load "headerline.el")
(load "terminal-modeline.el")
(setq resize-mini-windows nil) ;; Stop the minibuffer from resizing all the time

(provide 'terminal-display)
;;; terminal-display.el  ends here
