(when (not (package-installed-p 'ebib))
  (package-install 'ebib))



(global-set-key "\C-ce" 'ebib)


