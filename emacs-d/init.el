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









(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e4e97731f52a5237f37ceb2423cb327778c7d3af7dc831788473d4a76bcc9760" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
