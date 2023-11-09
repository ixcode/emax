
(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(use-package ag
  :ensure t)

(require 'projectile)

(global-unset-key (kbd "<s-p>"))
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

(setq projectile-project-search-path '("~/Code/"))

(projectile-mode +1)
