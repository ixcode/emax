;;; package --- Summary
;; Provides entry point when econfiguring in the terminal
;;; Commentary:

;;; Code:

(load "startup-message.el")
(load "themes.el")
(load "terminal-display.el")
(load "keyboard-mac-terminal.el")

;;(load "terminal-foundations.el")


(when (not (package-installed-p 'auto-complete))
  (package-install 'auto-complete))

(require 'auto-complete)
(require 'auto-complete-config)
(load "ido-conf.el")

(when (not (package-installed-p 'yasnippet))
  (package-install 'yasnippet))

(require 'yasnippet)
(yas-global-mode 1)

(ac-config-default)
(global-auto-complete-mode t)

(when (not (package-installed-p 'magit))
  (package-install 'magit))

(require 'magit)

(provide 'terminal-config-stable)
;;; terminal-config-stable.el  ends here
