;;; package --- Summary
;; Provides entry point when econfiguring in the terminal
;;; Commentary:

;;; Code:

(load "startup-message.el")
(load "themes.el")
(load "terminal-display.el")
(load "keyboard-mac-terminal.el")

;;(load "terminal-foundations.el")

(require 'auto-complete)
(require 'auto-complete-config)
(load "ido-conf.el")

(require `yasnippet)
(yas-global-mode 1)

(ac-config-default)
(global-auto-complete-mode t)

(require 'magit)

(provide 'terminal-config-stable)
;;; terminal-config-stable.el  ends here
