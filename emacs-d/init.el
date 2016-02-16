(message "[emax] : This is the start of something beautiful...")

(add-to-list 'load-path "~/.emacs.d/emax/lib")
(add-to-list 'load-path "~/.emacs.d/emax/conf")

(load "startup-message.el")
(load "keyboard.el")
(load "display.el")

(load "foundations.el")
(message "Foundations loaded ok.")
(load "shells.el")
(message "Shells loaded ok.")
(load "languages.el")
(message "Languages loaded ok.")
(load "journal.el")
(message "Journal loaded ok.")

(put 'narrow-to-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "[emax] : Emax is happily configured, enjoy.")
;;(start-process-shell-command "startup-voice" "startup" "sleep 1;say \"Hello Jim, time to code!\"")

