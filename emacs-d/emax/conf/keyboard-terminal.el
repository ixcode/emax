;;; package --- Summary
;;; Commentary:
;;; Code:

(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"

(if (eq system-type 'darwin)
    (load "keyboard-mac-terminal.el"))

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


(provide 'keyboard-terminal)
;;; keyboard-terminal.el ends here
