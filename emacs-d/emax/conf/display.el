(set-frame-position (selected-frame) 0 0)

(tool-bar-mode -1) ;; hide the toolbar
;; Only want to do this if running in the terminal (menu-bar-mode -1) ;; hide the menubar

(show-paren-mode) ;; Highlight matching parentheses
(setq visible-bell 1) ;;To stop it making a bell noise...
(setq ring-bell-function 'ignore)

;; By default don't wrap lines
(setq-default truncate-lines t)

(if window-system
    (progn
      (set-frame-size (selected-frame) 200 60)
      (set-frame-position (selected-frame) 0 0)))


