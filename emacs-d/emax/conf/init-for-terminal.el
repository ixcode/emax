;;; package --- Summary
;; Provides entry point when econfiguring in the terminal
;;; Commentary:

;;; Code:

(message "Configuring EMAX in Terminal mode")


(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))


(require 'terminal-config-stable)

(provide 'init-for-terminal)
;;; init-for-terminal.el ends here
