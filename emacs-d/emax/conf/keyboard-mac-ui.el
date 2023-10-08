;;; package --- Summary
;;; Commentary:
;;; Code:


;;  (setq mac-option-key-is-meta t)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)

;;  (setq mac-command-key-is-meta nil)
(setq mac-command-modifier 'super)
(setq mac-right-command-modifier 'super)

(defun insert-pound ()
  "Put the pound(#) key in the buffer."
  (interactive)
  (insert "#"))

(defun insert-gbp ()
  "Put the british pound (£) key in the buffer."
  (interactive)
  (insert "£"))


;; Might need to change this part depending if you remap the £ or # keys

(global-set-key (kbd "M-3") 'insert-pound) ;; On a uk keyboard, you need to press alt and we have disabled it

(global-unset-key (kbd "M-3")) ;; Was 'digit-argument but I don't often use it whereas pound is always used. An alternative would be M-£ but then it wouldn't be the same in other apps
;;(global-set-key (kbd "M-3") 'insert-gbp) ;; On a uk keyboard, you need to press alt and we have disabled it

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


(setq org-support-shift-select t)

(provide 'keyboard-mac-ui)
;;; keyboard-mac-ui.el ends here
