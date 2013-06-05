(message "[emax] : This is the start of something beautiful...")

(add-to-list 'load-path "~/.emacs.d/emax/lib")
(add-to-list 'load-path "~/.emacs.d/emax/conf")

(load "startup-message.el")
(load "keyboard.el")
(load "display.el")

(load "foundations.el")
(load "shells.el")
(load "languages.el")


(message "[emax] : Emax is happily configured, enjoy.")









