;;; package --- Summary
;; Provides entry point to the emax config system
;;; Commentary:

;;; Code:
(message "[emax] : This is the start of something beautiful...")

;; If having trouble loading - can do this open -a /Applications/Emacs.app --args --debug-init


(add-to-list 'load-path "~/.emacs.d/emax/lib")
(add-to-list 'load-path "~/.emacs.d/emax/conf")

(if (display-graphic-p)
  (load "init-for-ui")
  (load "init-for-terminal"))

;;(set-default 'explicit-shell-file-name "/usr/local/bin/zsh")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets magit ido-completing-read+ google-this google-maps google-c-style google auto-complete-nxml auto-complete-exuberant-ctags auto-complete-clang auto-complete-chunk auto-complete-c-headers auto-complete-auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
