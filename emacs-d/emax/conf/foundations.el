(require 'package)

(package-initialize)

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))


(load "temp-files-conf.el")

;; Keep track of files we've been editing
(require 'recentf)
(recentf-mode 1)

;; Use spaces instead of tab
(setq-default indent-tabs-mode nil)

(setq exec-path (append exec-path '("/usr/local/bin")))

(server-start)

(load "ido-conf.el")



