;;; package --- Summary

;;; Commentary:

;;; Code:

;; (when (not (package-installed-p 'color-theme-solarized))
;;   (package-install 'color-theme-solarized))

;;(require 'color-theme-solarized)

;; See here http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
(add-to-list 'custom-theme-load-path "~/.emacs.d/emax/colour-themes")
;;(load-theme 'minamin t)
(load-theme 'minamindark t)

(set-cursor-color "#ffffff")


;;(set-face-attribute 'fringe nil :background "#fffff5" :foreground "#2E2920")



;; (when (eq system-type 'darwin)
;;   (set-default-font "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"))

;;(setq-default line-spacing 4)

;; (when (not (package-installed-p 'doom-themes))
;;   (package-install 'doom-themes))

;;(require 'doom-themes)
;;(load-theme 'doom-nord t)

;;(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;     doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Enable flashing mode-line on errors
;;(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
;;(doom-themes-neotree-config)
;; or for treemacs users
;;(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
;;(doom-themes-org-config)

;; (setq ansi-color-names-vector
;;    ["black" "red" "green" "#813a01" "PaleBlue" "magenta" "cyan" "white"])
