


(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
		 ("gnu" . "http://elpa.gnu.org/packages/")
		 ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))


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

;; Use spaces instead of tab
(setq-default indent-tabs-mode nil) ;; Can put nil or t if want spaces instead of tabs
(setq tab-width 4)
;; need to work out how to turn off tabs mode for everything except tsv files
(autoload 'tsv-mode "tsv-mode" "A mode to edit table like file" t)
(autoload 'tsv-normal-mode "tsv-mode" "A minor mode to edit table like file" t)


