(message "Running in GUI display mode")

(load "themes.el")

(if window-system
    (progn
      (set-frame-size (selected-frame) 179 47)
      (set-frame-position (selected-frame) 0 0)))

(require 'linum)
;;(global-linum-mode 1)
(setq linum-format " %4d ")

(when (display-graphic-p)
  (tool-bar-mode -1) ;; hide the toolbar
  (scroll-bar-mode -1))

(global-hl-line-mode)

(global-visual-line-mode t)
