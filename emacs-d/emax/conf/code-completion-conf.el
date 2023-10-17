;;; code-completion.el - provides ability to pop up code completions

(use-package popup  :ensure t)
(use-package fuzzy  :ensure t)
(use-package auto-complete :ensure t)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")

(ac-config-default)

(setq ac-auto-show-menu t
      ac-dwim t
      ac-use-menu-map t
      ac-use-quick-help t
      ac-quick-help-delay 1
      ac-quick-help-height 60
      ac-disable-inline t
      ac-show-menu-immediately-on-auto-complete t
      ac-auto-start 2
      ac-candidate-menu-min 0)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic
               ac-source-yasnippet))
(setq completion-fail-discreetly t)
(global-auto-complete-mode t)
