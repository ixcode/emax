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
(load "writing.el")
(load "irc.el")
(load "web.el")

(put 'narrow-to-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "[emax] : Emax is happily configured, enjoy.")
;;(start-process-shell-command "startup-voice" "startup" "sleep 1;say \"Hello Jim, time to code!\"")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fffff5" "#cc0000" "#5F7F5F" "yellow" "blue" "#6622CC" "#0c00cc" "#333c4e"])
 '(custom-safe-themes
   (quote
    ("bd3a84a9cd58d5d5c9f7f71bc0317634a7d4fe17b5523403203ac4894a105e72" default)))
 '(fci-rule-color "#ecece0")
 '(vc-annotate-background "#fffff5")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#cc0000")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#00a300")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#0c00cc")
     (280 . "#0e00e5")
     (300 . "#271aff")
     (320 . "#574dff")
     (340 . "#1a87ff")
     (360 . "#6622CC"))))
 '(vc-annotate-very-old-color "#6622CC"))
