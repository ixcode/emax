;;; package --- Summary
;; Provides entry point when econfiguring in the terminal
;;; Commentary:

;;; Code:

(message "Configuring EMAX in Terminal mode")

(load "startup-message.el")
(load "keyboard-terminal.el")

(menu-bar-mode -1)

(provide 'init-for-terminal)
;;; init-for-terminal.el ends here
