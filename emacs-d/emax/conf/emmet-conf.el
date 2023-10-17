
(use-package emmet-mode
  :ensure t
  :bind (("C-<tab>" . emmet-expand-line))
  :custom ((emmet-move-cursor-between-quotes t "Puts the cursor into eg attributes"))
  :hook ((web-mode-hook  . emmet-mode)
         (html-mode-hook . emmet-mode)
         (sgml-mode-hook . emmet-mode)
         (css-mode-hook  . emmet-mode)))

;; (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;; (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;; (setq emmet-move-cursor-between-quotes t) ;; default nil
