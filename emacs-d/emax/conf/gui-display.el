;;; package --- Summary
;; Provides entry point when comfiguring in the ui
;;; Commentary:


;;; Code:
(message "Running in GUI display mode")

;;(load "themes.el") - should be able to load this in terminal and app

(setq-default line-spacing 4)
;;(setq-default cursor-type '(hbar . 20))
(defconst default-font-size 13)

(defun set-global-font-size (font-size)
  "Set the frame font to (as FONT-SIZE)."
  (set-frame-font (format "Menlo %d" font-size)))

(set-global-font-size default-font-size)

(defvar presentation-toggle nil)

(defun emax-toggle-presentation-mode ()
  "Toggle bigger fonts everywhere."
  (interactive)
  (if presentation-toggle
      (progn (set-global-font-size default-font-size)
             (setq presentation-toggle nil))
    
    (progn (set-global-font-size 20)
           (setq presentation-toggle t))))

(load "themes.el")

(tool-bar-mode -1) ;; hide the toolbar
(scroll-bar-mode -1) ;; hide scrollbars
(setq fringe-mode 'left-only)

(use-package vi-tilde-fringe
  :ensure t)
(add-hook 'prog-mode-hook 'vi-tilde-fringe-mode)

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
(set-default 'truncate-lines t)
(set-face-bold 'bold nil)
;;
(load "modeline.el")

(set-cursor-color 'magenta)

(defun my-display-numbers-hook ()
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'my-display-numbers-hook)
(add-hook 'lisp-mode-hook 'my-display-numbers-hook)

(setq-default display-line-numbers-width 4)

