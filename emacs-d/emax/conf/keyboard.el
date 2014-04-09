;; This keyboard layout is focused on the core of editing on a mac
;; Therefore it prioritises mac familiar key shortcuts.

;; The goal here is to try and preserve the "OSX" experience so that you don't have to remap the world.

;; The next most useful key in emacs is the META key, so we bind that to the right option key
;; It's the right because most common is M-x and that allows you to type it with opposite hands

;; By default on os x, 'super' is mapped to copy, paste etc, like s-x, s-c, s-v, s-z
;; modifier settings can be: [control | meta | alt | super | hyper]

(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"

;; To allow window switching easy:
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))


(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil)
  (setq mac-option-modifier 'alt)
  (setq mac-right-option-modifier 'meta)

  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-right-command-modifier 'super)

  (defun insert-pound ()
    (interactive)
    (insert "#"))

  (global-unset-key (kbd "<s-right>")) ;; Was 'ns-next-frame
  (global-unset-key (kbd "<s-left>"))  ;; Was 'ns-previous-frame

  (global-set-key (kbd "<s-right>") 'select-next-window)
  (global-set-key (kbd "<s-left>")  'select-previous-window)


  (global-set-key (kbd "A-3") 'insert-pound) ;; On a uk keyboard, you need to press alt and we have disabled it

  (global-unset-key (kbd "M-3")) ;; Was 'digit-argument but I don't often use it whereas pound is always used. An alternative would be M-£ but then it wouldn't be the same in other apps
  (global-set-key (kbd "M-3") 'insert-pound) ;; On a uk keyboard, you need to press alt and we have disabled it

  ;; These rebind the cmd key where possible - they usually use meta but its easier to hit cmd
  (global-set-key (kbd "s-<") 'beginning-of-buffer)
  (global-set-key (kbd "s->") 'end-of-buffer)

  (global-set-key (kbd "s-/") 'dabbrev-expand)

  (global-set-key (kbd "C-,") 'scroll-down-line)
  (global-set-key (kbd "C-.") 'scroll-up-line)

  (global-set-key (kbd "s-]") 'next-buffer)
  (global-set-key (kbd "s-[") 'previous-buffer)


  (global-set-key (kbd "s-r") 'move-to-window-line-top-bottom)

  (global-set-key (kbd "C-s-/") 'indent-region)


  (setq org-support-shift-select t))

  (add-hook 'shell-mode
          (lambda ()
            (local-set-key (kbd "A-p") 'comint-previous-input)
            (local-set-key (kbd "A-n") 'comint-next-input)
            ))

(global-set-key (kbd "<f5>") 'magit-status)

(global-unset-key (kbd "s-P")) ;; was print buffer
(global-set-key (kbd "s-P") 'magit-push)
