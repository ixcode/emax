;; (when (not (package-installed-p 'color-theme-solarized))
;;   (package-install 'color-theme-solarized))

;;(require 'color-theme-solarized)

;; See here http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
(add-to-list 'custom-theme-load-path "~/.emacs.d/emax/colour-themes")
(load-theme 'minamin t)
;;(load-theme 'zenburn t)
;;(load-theme 'solarized t)

;;(load-theme 'minamindark t) 

(when (eq system-type 'darwin)
  (set-default-font "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"))

