;;;; Package -- Summary

;; This keyboard layout is focused on the core of editing on a mac
;; Therefore it prioritises mac familiar key shortcuts.

;; The goal here is to try and preserve the "OSX" experience so that you don't have to remap the world.

;; The next most useful key in emacs is the META key, so we bind that to the right option key
;; It's the right because most common is M-x and that allows you to type it with opposite hands

;; By default on os x, 'super' is mapped to copy, paste etc, like s-x, s-c, s-v, s-z
;; modifier settings can be: [control | meta | alt | super | hyper]

(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"



;; https://www.emacswiki.org/emacs/EmacsForMacOS
;; sets up some os x specific mappings
(if (eq system-type 'darwin)
    (load "keyboard-mac-ui.el"))


;; To allow window switching easy:
(defun select-next-window ()
  "Switch to the next window."
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window."
  (interactive)
  (select-window (previous-window)))

(global-unset-key (kbd "<s-right>")) ;; Was 'ns-next-frame
(global-unset-key (kbd "<s-left>"))  ;; Was 'ns-previous-frame

(global-set-key (kbd "<s-right>") 'select-next-window)
(global-set-key (kbd "<s-left>")  'select-previous-window)

(add-hook 'shell-mode
          (lambda ()
            (local-set-key (kbd "A-p") 'comint-previous-input)
            (local-set-key (kbd "A-n") 'comint-next-input)
            ))

(global-set-key (kbd "<f5>") 'magit-status)

(global-unset-key (kbd "s-P")) ;; was print buffer
(global-set-key (kbd "s-P") 'magit-push)


(global-unset-key (kbd "M-f")) 
(global-unset-key (kbd "M-b"))

(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)

