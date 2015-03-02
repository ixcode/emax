

(tool-bar-mode -1) ;; hide the toolbar
(scroll-bar-mode -1)

;; Only want to do this if running in the terminal (menu-bar-mode -1) ;; hide the menubar

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

(setq frame-title-format
  '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))


(global-font-lock-mode 1) ;; allows syntax highlighting to work


;; See here http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
(add-to-list 'custom-theme-load-path "~/.emacs.d/emax/colour-themes")
;;(load-theme 'minamin t)
(load-theme 'zenburn t)


;;(load-theme 'minamindark t) 

(when (eq system-type 'darwin)
  (set-default-font "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"))


(if window-system
    (progn
      (set-frame-size (selected-frame) 179 47)
      (set-frame-position (selected-frame) 0 0)))

(require 'linum)
;;(global-linum-mode 1)
(setq linum-format " %4d ")

(when (fboundp 'fringe-mode)
  (fringe-mode '(1 . 0))) ;; Fringe widths (left . right) should be in pixels

;;(set-face-attribute 'fringe nil :background "#3F3F3F" :foreground "#2E2920")
;;(set-face-attribute 'linum nil :background "#4e4e4e")

(load "headerline.el")
(load "modeline.el")
(load "popwin-conf.el")
(global-hl-line-mode)


(global-visual-line-mode t)
