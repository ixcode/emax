(message "Running in terminal display mode")

(when (not (display-graphic-p))
  (menu-bar-mode -1))
