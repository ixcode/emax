;;; init-for-ui.el - Configure everything in UI mode

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


;;; init-for-ui.el ends here
