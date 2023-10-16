;;; package --- Summary
;; Provides entry point when comfiguring in the ui
;;; Commentary:
;;; Code:
(message "[emax] Configuring EMAX in UI mode")


(emax-load '((startup-message     t)
             (keyboard            t)
	     (gui-display         t)
	     (keyboard            t)
	     (gui-display         t)
	     (foundations         t)
	     (shells              t)
	     (languages           nil)
	     (journal             nil)))

(emax-message-line "Emax is happily configured, enjoy.")

(provide 'init-for-ui)
;;; init-for-ui.el ends here
