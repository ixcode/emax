(defun my-magit-mode-hook ()
  "Custom `magit-mode' behaviours."
  (setq left-fringe-width 0
        right-fringe-width 0))

(add-hook 'magit-mode-hook 'my-magit-mode-hook)
