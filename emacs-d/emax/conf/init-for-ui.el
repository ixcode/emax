;;; package --- Summary
;;coding.el - Provides support for various coding languages
;;; Commentary:
;;; Code:
(require 'emax-core)

(message "[emax] Configuring EMAX in UI mode")


(emax-load '((gui-display         t)
	     (startup-message     t)
	     (keyboard            t)
             (foundations         t)
             (auto-completion     t) 	     
	     (shells              t)
	     (coding              t)
	     (journal             nil)))

(emax-message-line "Emax is happily configured, enjoy.")


(provide 'init-for-ui)
;;; init-for-ui.el ends here
