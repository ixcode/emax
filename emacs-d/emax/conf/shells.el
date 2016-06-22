(load "ssh-shell.el")

(when (not (package-installed-p 'vagrant-tramp))
  (package-install 'vagrant-tramp))

(eval-after-load 'tramp
  '(vagrant-tramp-enable))

 (require 'multi-term)

;;(when (not (package-installed-p 'top-mode))
;;  (package-install 'top-mode))
