;;; coding.el - Provides support for various coding languages

(use-package magit :ensure t)

(emax-conf '((electric-pair     .  t)
             (paredit           .  t)
             (code-completion   .  t)
             (clojure           .  t)
             (incanter          .  nil)
             (emmet             .  t)
             (web-mode          .  t)
             (lsp               .  t)
             (nav               .  t)
             (yasnippet         .  t)
             (flycheck          .  t)))

