;;; package --- Summary
;; Provides entry point when comfiguring in the ui
;;; Commentary:
;;Ivy is an alternative to ido-mode for narrowing lists.

;;; Code:

(message "Initialising Ivy")

(unless (package-installed-p 'counsel)
  (package-install 'counsel))

(use-package ivy :demand
      :config
      (setq ivy-use-virtual-buffers t
            ivy-count-format "%d/%d "))

(ivy-mode 1)

(provide 'ivy-conf)
