;; POPWIN - Controls size of popup windows
;; https://github.com/m2ym/popwin-el



(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-height 0.2)

(setq display-buffer-function 'popwin:display-buffer)

;; (setq popwin:special-display-config
;;       '(("*Help*"  :height 30 :stick t)
;;         ("*Completions*" :noselect t)
;;         ("*compilation*" :noselect t)
;;         ("*Messages*" :height 30)
;;         ("*Occur*" :noselect t)
;;         ("\\*Slime Description.*" :noselect t :regexp t :height 30)
;;         ("*magit-commit*" :noselect t :height 40 :width 80)
;;         ("*magit-diff*" :noselect t :height 40 :width 80)
;;         ("*magit-edit-log*" :noselect t :height 15 :width 80)
;;         ("\\*Slime Inspector.*" :regexp t :height 30)
;;         ("*Ido Completions*" :noselect t :height 30)
;;         ("*eshell*" :height 30)
;;         ("\\*ansi-term\\*.*" :regexp t :height 30)
;;         ("*shell*" :height 30)
;;         (".*overtone.log" :regexp t :height 30)
;;         ("*gists*" :height 30)
;;         ("*sldb.*":regexp t :height 30)
;;         ("*nREPL error*" :height 30)
;;         ("*nREPL doc*" :height 30)
;;         ("*Kill Ring*" :height 30)))

;; (defun live-show-messages ()
;;   (interactive)
;;   (popwin:display-buffer "*Messages*"))


(push '("^\\*magit.*\\*$"  :regexp) popwin:special-display-config)
(push "*Backtrace*" popwin:special-display-config)
;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)

;; M-!
(push "*Shell Command Output*" popwin:special-display-config)

;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)

;; slime
(push "*slime-apropos*" popwin:special-display-config)
(push "*slime-macroexpansion*" popwin:special-display-config)
(push "*slime-description*" popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push "*slime-xref*" popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push 'slime-repl-mode popwin:special-display-config)
(push 'slime-connection-list-mode popwin:special-display-config)

;; vc
(push "*vc-diff*" popwin:special-display-config)
(push "*vc-change-log*" popwin:special-display-config)

;; undo-tree
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)

(push "*Buffer List*" popwin:special-display-config)
