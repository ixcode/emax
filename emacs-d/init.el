;;; package --- Summary
;; Provides entry point to the emax config system
;;; Commentary:

;;; Code:
(message "[emax] : A beginning is a delicate time...")

;; Enable quelpa package manager
(use-package quelpa :ensure t)
(use-package quelpa-use-package :ensure t)


;; If having trouble loading - can do this open -a /Applications/Emacs.app --args --debug-init

(add-to-list 'load-path "~/.emacs.d/emax/lib")
(add-to-list 'load-path "~/.emacs.d/emax/conf")

(load "emax-core")

(if (display-graphic-p)
   (load "init-for-ui")
   (load "init-for-terminal"))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(chatgpt polymode quelpa-use-package quelpa emmet-mode auto-complete-config fuzzy amx web-mode mini-modeline command-log-mode lsp-java lsp-mode iedit projectile nim-mode elpy flycheck go-mode markdown-mode sass-mode lua-mode slim-mode haml-mode nav paredit cider clojure-mode rainbow-mode rainbow-delimiters yaml-mode find-file-in-project fish-mode csv-mode vagrant-tramp expand-region yasnippet-snippets magit ido-completing-read+ google-this google-maps google-c-style google auto-complete-nxml auto-complete-exuberant-ctags auto-complete-clang auto-complete-chunk auto-complete-c-headers auto-complete-auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
