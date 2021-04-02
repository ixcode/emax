;;; package --- Summary
;;; Commentary:


;;; Code:

;; If you already set your terminal to map option to meta then you don't need this
;; but it will work anyway so its ok to have here just in case.
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)

;; This allows us to bind activities to the command key.
(setq mac-command-modifier 'super)
(setq mac-right-command-modifier 'super)

(setq-default indent-tabs-mode nil)

;; Configure the away the keyboard works for various programming languages
;;http://www.xemacs.org/Documentation/html/cc-mode_19.html

(defconst jims-c-style
  '((c-basic-offset             . 4)
    (c-hanging-braces-alist     . ((substatement-open . (after))
                                   (func-decl-cont . (after)))))
  "Jims C Programming Style")

;; offset customizations not in my-c-style
;;(setq c-offsets-alist '((member-init-intro . ++)))

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  (message "Configuring Jims c-mode")
  (c-add-style "PERSONAL" jims-c-style t)
  (setq indent-tabs-mode nil)
  (electric-pair-mode))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


(provide 'keyboard-mac-terminal)
;;; keyboard-mac-terminal.el ends here

 
