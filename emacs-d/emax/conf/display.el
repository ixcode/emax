
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

;;https://stackoverflow.com/questions/5795451/how-to-detect-that-emacs-is-in-terminal-mode

       


(when (fboundp 'fringe-mode)
  (fringe-mode '(1 . 0))) ;; Fringe widths (left . right) should be in pixels

(set-face-attribute 'fringe nil :background "#fffff5" :foreground "#2E2920")
;;(set-face-attribute 'linum nil :background "#4e4e4e")

(load "headerline.el")
(load "modeline.el")
(load "popwin-conf.el")




(if (display-graphic-p)
    (load "gui-display.el")
  (load "terminal-display.el"))
