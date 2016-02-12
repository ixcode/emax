(message "[emax] : This is the start of something beautiful...")

(add-to-list 'load-path "~/.emacs.d/emax/lib")
(add-to-list 'load-path "~/.emacs.d/emax/conf")

(load "startup-message.el")
(load "keyboard.el")
(load "display.el")

(load "foundations.el")
(load "shells.el")
(load "languages.el")
(load "journal.el")

(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("acef1c62c1af60ff67aac0638fd77bf65d3f6329a3045e6b456bf7ecf35fb48b" "d87be6635cb19f8c8d1b9d030bf42a2ed3be81d457f7842b5276d3f71eb7a402" "c9b2cef2db2b0214528931b6e5757b97d65b3aa4f35bb455f471df0216f76fc0" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "e4e97731f52a5237f37ceb2423cb327778c7d3af7dc831788473d4a76bcc9760" default)))
 '(markdown-command "multimarkdown")
 '(markdown-enable-math t)
 '(safe-local-variable-values
   (quote
    ((eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(message "[emax] : Emax is happily configured, enjoy.")
;;(start-process-shell-command "startup-voice" "startup" "sleep 1;say \"Hello Jim, time to code!\"")

