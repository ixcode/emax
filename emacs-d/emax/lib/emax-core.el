;;; package --- Summary
;;  emax-core.el - Provides support for various coding languages
;;; Commentary:
;;; Code:
(defun emax-log (log-message &rest params)
  "Prints a formatted (as LOG-MESSAGE) with (as PARAMS) to the *Messages* buffer."
  (apply #'message (concat "[emax] - " log-message) params))

(defun emax-message-line (message)
  "Prints a (as MESSAGE) surrounded with a kind of box."
  (emax-log "--------------------------------------------------------------------------------------------")
  (emax-log message)
  (emax-log "--------------------------------------------------------------------------------------------")
  nil)

(defun emax-make-module-file-name (symbol)
  "Add '.el' to (as SYMBOL)."
  (concat (symbol-name symbol) ".el"))


(defun emax-load-module (module)
  "Load a (as MODULE) from emax which is basically just an '.el' file."
  (interactive)
  (let ((module-file-name (emax-make-module-file-name module)))
    (emax-log "--------------------------------------------------------------------------------------------")
    (emax-log "Loading module \"%s\"" module-file-name)
    (condition-case err
	(progn
	  (load module-file-name)
	  (emax-log "Module loaded, no errors."))
      (error
       (emax-log "\nERROR: Failed to load module \"%s\": %s\n" module-file-name err) )))
  (emax-log "--------------------------------------------------------------------------------------------\n"))

(defun emax-conf-sections (module-list)
  "Load the configuration sections (as MODULE-LIST)."
  (emax-log "Loading configuration sections.")
  (dolist (item module-list)
    (let ((module (car item))
	  (enabled (cdr item)))
      (if enabled
	  (emax-load-module module)
	(emax-log "Module \"%s\" - not enabled" module)))))

(defun emax-make-conf-file-name (symbol)
  "Add '-conf.el' to the name of (as SYMBOL)."
  (concat (symbol-name symbol) "-conf.el"))


(defun emax-conf-module (module)
  "Configure a (as MODULE)."
  (let ((module-conf-name (emax-make-conf-file-name module)))
    (emax-log "Configuring  module \"%s\"" module-conf-name)
    (condition-case err
	(progn
	  (load module-conf-name)
	  (emax-log "Module configured, no errors."))
      (error
       (emax-log "\nERROR: Failed to load module \"%s\": %s\n" module-conf-name err) ))))

(defun emax-conf (module-list)
  "Go through the list of modules (as MODULE-LIST) and configures."
  (emax-log "Configuring modules...")
  (dolist (item module-list)
    (let ((module  (car item))
	  (enabled (cdr item)))
      (if enabled
	  (emax-conf-module module)
	(emax-log "Module \"%s\" - not enabled" module)))))

(provide 'emax-core)
;;; emax-core.el ends here
