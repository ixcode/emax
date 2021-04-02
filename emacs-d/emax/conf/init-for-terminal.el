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
(load "ido-conf.el")

(require 'cl) ;; TO help auto-complete work, common lispt
(when (not (package-installed-p 'auto-complete))
  (package-install 'auto-complete))
(require 'auto-complete)

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;;https://github.com/byuksel/Emacs-as-a-C-Cplusplus-Editor-IDE-settings/blob/master/.emacs


;; Keep track of files we've been editing
(require 'recentf)
(recentf-mode 1)

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


(require 'uniquify)


(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(global-font-lock-mode 1) ;; allows syntax highlighting to work
(load "headerline.el")
(load "modeline.el")
(load "popwin-conf.el")
(require 'recentf)
(recentf-mode 1)
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq resize-mini-windows nil) ;; Stop the minibuffer from resizing all the time
(load "ido-conf.el")

(when (not (package-installed-p 'expand-region))
  (package-install 'expand-region))
(require 'expand-region)
(pending-delete-mode t)




(provide 'init-for-terminal)
;;; init-for-terminal.el ends here
