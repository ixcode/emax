
(when (not (package-installed-p 'rainbow-delimiters))
  (package-install 'rainbow-delimiters))

(global-rainbow-delimiters-mode)

(when (not (package-installed-p 'rainbow-mode))
  (package-install 'rainbow-mode))

(require 'rainbow-mode)
(rainbow-mode)
