;; Enable LSP 

(use-package lsp-mode
  :ensure t
  :custom (lsp-html-visual-mode t)
  :hook ((html-mode . lsp)
         (web-mode  . lsp)))

(use-package lsp-java
  :ensure t
  :hook (java-mode-hook . lsp))


