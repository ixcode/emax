;;; package --- Summary
;; Provides entry point when comfiguring in the ui
;;; Commentary:


;;; Code:
(message "Running in GUI display mode")

(load "themes.el")

(set-default-font "Menlo 15")

;; (defvar after-make-console-frame-hooks '()
;;   "Hooks to run after creating a new TTY frame")

;; (defvar after-make-window-system-frame-hooks '((set-default-font "Menlo 15"))
;;   "Hooks to run after creating a new window-system frame")

;; (defun run-after-make-frame-hooks (frame)
;;   "Run configured hooks in response to the newly-created FRAME.
;; Selectively runs either `after-make-console-frame-hooks' or
;; `after-make-window-system-frame-hooks'"
;;   (with-selected-frame frame
;;     (run-hooks (if window-system
;;                    'after-make-window-system-frame-hooks
;;                  'after-make-console-frame-hooks))))

;; (add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)


(if window-system
    (progn
      (set-frame-size (selected-frame) 130 47)
      (set-frame-position (selected-frame) 0 0)))

(set-mouse-color "white")
(require 'linum)
;;(global-linum-mode 1)
(setq linum-format " %4d ")

(when (display-graphic-p)
  (tool-bar-mode -1) ;; hide the toolbar
  (scroll-bar-mode -1))

;;(load "eaf/eaf.el") ;; eaf doesnt work on a mac

;;(global-hl-line-mode)

(global-visual-line-mode t)

;;(load "powerline.el")


