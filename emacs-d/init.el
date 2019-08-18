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
 '(ansi-color-names-vector
   ["#fffff5" "#cc0000" "#5F7F5F" "yellow" "blue" "#6622CC" "#0c00cc" "#333c4e"])
 '(custom-safe-themes
   (quote
    ("bd3a84a9cd58d5d5c9f7f71bc0317634a7d4fe17b5523403203ac4894a105e72" default)))
 '(explicit-shell-file-name "/usr/local/bin/zsh")
 '(fci-rule-color "#ecece0")
 '(package-selected-packages
   (quote
    (expand-region nim-mode exec-path-from-shell fish-mode ido-completing-read+ yaml-mode web-mode vagrant-tramp slim-mode sass-mode rainbow-mode rainbow-delimiters paredit nav markdown-mode magit lua-mode graphviz-dot-mode google-this go-mode flycheck elpy ebib csv-mode cider auto-complete)))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-black ((t (:background "black" :foreground "#3B425" :underline nil :weight normal))))
 '(term-color-blue ((t (:background "#80A0C2" :foreground "#80A0C2" :underline nil :weight normal))))
 '(term-color-cyan ((t (:background "#86C0D1" :foreground "#86C0D1" :underline nil :weight normal))))
 '(term-color-green ((t (:background "#A2BF8A" :foreground "#A2BF8A" :underline nil :weight normal))))
 '(term-color-magenta ((t (:background "#B58DAE" :foreground "#B58DAE" :underline nil :weight normal))))
 '(term-color-red ((t (:background "#C16069" :foreground "#C16069" :underline nil :weight normal))))
 '(term-color-white ((t (:background "#f0f0f0" :foreground "#f0f0f0" :underline nil :weight normal))))
 '(term-color-yellow ((t (:background "#ECCC87" :foreground "#ECCC87" :underline nil :weight normal)))))
