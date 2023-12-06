;;; package --- Summary
;;coding.el - Provides support for various coding languages
;;; Commentary:
;;; Code:
(require 'emax-core)

(use-package magit :ensure t)

(emax-conf '((electric-pair     .  t)
             (paredit           .  t)
             (code-completion   .  t)
             (clojure           .  t)
             (swift             .  t)
             (incanter          .  nil)
             (emmet             .  t)
             (web-mode          .  t)
             (impatient-mode    .  t)
             (lsp               .  t)
             (nav               .  t)
             (yasnippet         .  t)
             (projectile        .  t)
             (flycheck          .  t)))

(provide 'coding)
;;; coding.el ends here

