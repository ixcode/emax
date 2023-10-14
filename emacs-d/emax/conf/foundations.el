;;; foundations.el --- Sets up useful things for programming

;;; Commentary:

;;; Code:
(message "Initialising foundations")



(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
		         ("gnu" . "http://elpa.gnu.org/packages/")
		         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
; fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))


;; copies path environment from the shell into emacs
(when (not (package-installed-p 'exec-path-from-shell))
  (package-install 'exec-path-from-shell))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(load "temp-files-conf.el")

;; Keep track of files we've been editing
(require 'recentf)
(recentf-mode 1)


(setq exec-path (append exec-path '("/usr/local/bin")))

(defun setenv-from-shell (varname)
  (setenv varname (env-var-from-shell varname)))

;; from http://stackoverflow.com/questions/6411121/how-to-make-emacs-to-use-my-bashrc-file
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))


(setq resize-mini-windows nil) ;; Stop the minibuffer from resizing all the time

;;http://emacsformacosx.com/tips
;;(x-focus-frame nil)


;;(server-start)

(load "ido-conf.el")
;;(load "ivy-conf.el")

;; Use spaces instead of tab
(setq-default indent-tabs-mode nil) ;; Can put nil or t if want spaces instead of tabs
(setq tab-width 4)
;; need to work out how to turn off tabs mode for everything except tsv files
(autoload 'tsv-mode "tsv-mode" "A mode to edit table like file" t)
(autoload 'tsv-normal-mode "tsv-mode" "A minor mode to edit table like file" t)

(when (not (package-installed-p 'expand-region))
  (package-install 'expand-region))
(require 'expand-region)
(pending-delete-mode t)

(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
;;(evil-mode 1)

(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(require 'projectile)

(global-unset-key (kbd "<s-p>"))
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

(setq projectile-project-search-path '("~/Code/"))

(projectile-mode +1)

(unless (package-installed-p 'dumb-jump)
  (package-install 'dumb-jump))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(require 'expand-region)
(global-set-key (kbd "s-<up>") 'er/expand-region)
(global-set-key (kbd "s-<down>") 'er/contract-b)

(unless (package-installed-p 'iedit)
  (package-install 'iedit))

(require 'iedit)

(global-set-key (kbd "S-<f6>") 'iedit-mode)

(provide 'foundations)
;;; foundations.el ends here
