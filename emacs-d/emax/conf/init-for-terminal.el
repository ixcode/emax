;;; package --- Summary
;; Provides entry point when econfiguring in the terminal
;;; Commentary:

;;; Code:

(message "Configuring EMAX in Terminal mode")


(load "startup-message.el")
(load "keyboard-terminal.el")

(load "themes.el")

(menu-bar-mode -1)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'exec-path-from-shell))
  (package-install 'json-mode))

(require 'json-mode)

(when (not (package-installed-p 'auto-complete))
  (package-install 'auto-complete))
(require 'auto-complete)

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; Keep track of files we've been editing
(require 'recentf)
(recentf-mode 1)


(provide 'init-for-terminal)
;;; init-for-terminal.el ends here
