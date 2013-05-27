

(tool-bar-mode -1) ;; hide the toolbar
;; Only want to do this if running in the terminal (menu-bar-mode -1) ;; hide the menubar

(show-paren-mode) ;; Highlight matching parentheses
(setq visible-bell 1) ;;To stop it making a bell noise...
(setq ring-bell-function 'ignore)

;; By default don't wrap lines
(setq-default truncate-lines t)


(global-font-lock-mode 1) ;; allows syntax highlighting to work

(set-default-font "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; See here http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
(add-to-list 'custom-theme-load-path "~/.emacs.d/emax/colour-themes")
(load-theme 'minamin t) 


(if window-system
    (progn
      (set-frame-size (selected-frame) 176 47)
      (set-frame-position (selected-frame) 0 0)))
