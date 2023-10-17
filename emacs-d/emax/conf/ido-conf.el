;;; ido-conf.el --- Sets up useful things for programming

;;; Commentary:

;;; Code:

(use-package ido
  :ensure t)
(use-package ido-completing-read+
  :ensure t)
(use-package amx ;; Provides keybindings when looking things up
  :ensure t)


(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1) ; completing-read
(icomplete-mode 1)

(amx-mode 1)

(setq
 ido-enable-prefix nil
 ido-enable-flex-matching t
 ido-create-new-buffer 'always
 ido-use-filename-at-point 'guess
 ido-max-prospects 12
 ido-auto-merge-work-directories-length -1
 ido-ignore-extensions t ; uses completion-ignored-extensions
 ido-use-virtual-buffers t
 ido-max-directory-size nil
 ido-max-work-file-list 50
 suggest-key-bindings t
 enable-recursive-minibuffers t
 ido-ignore-buffers '("^ " "\.deft" "^\\*temp-shell*")
 ido-ignore-directories '("\\`\\.\\./" "\\`\\./")
 ido-ignore-files '("\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`__pycache__/")
 ido-file-extensions-order '(".el" ".hs" ".py")
 minibuffer-prompt-properties  '(read-only t
                                 point-entered minibuffer-avoid-prompt
                                 face minibuffer-prompt))



(provide 'ido-conf)
;;; ido-conf.el ends here

;; For virtual buffers
;;(setq recentf-max-saved-items 50)

;; Put ido completions on their own lines.
;; (setq ido-decorations
;;       '("\n → "
;;         ""
;;         "\n    "
;;         "\n    ..."
;;         "[" "]"
;;         " [No match]"
;;         " [Matched]"
;;         " [Not readable]"
;;         " [Too big]"
;;         " [Confirm]"))

;; (defun setup-ido ()
;;   (define-many-keys ido-completion-map
;;     `(("C- m" ido-merge-work-directories)
;;       ("C- <return>" ido-select-text)
;;       ("C- f" nil)
;;       ("C- x f" nil)
;;       ("C- x d" nil)
;;       ("C- b" nil)))

;;   (define-many-keys ido-file-dir-completion-map
;;     '(("+" ido-make-directory))))

;; (add-hook 'ido-minibuffer-setup-hook 'setup-ido)
;;(require 'idomenu)



;;ido-save-directory-list-file "~/Dropbox/emacs/ido.last"
