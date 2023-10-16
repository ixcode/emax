;;; package --- Summary
;; Provides entry point when comfiguring in the ui
;;; Commentary:


;;; Code:
(message "Running in GUI display mode")

;;(load "themes.el") - should be able to load this in terminal and app

(set-frame-font "Menlo 18")

(load "themes.el")

(tool-bar-mode -1) ;; hide the toolbar
(scroll-bar-mode -1) ;; hide scrollbars
(fringe-mode 0)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(when (not (package-installed-p 'rainbow-mode))
  (package-install 'rainbow-mode))


(require 'rainbow-mode)
(rainbow-mode 1)


;;(require 'linum)
;;(setq linum-format " %4d ")

;;(global-linum-mode 1)

;;(load "headerline.el")


(global-hl-line-mode)
(set-cursor-color 'magenta)

;;
(load "modeline.el")





