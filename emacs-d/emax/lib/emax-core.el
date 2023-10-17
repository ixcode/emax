;;; emacs-core.el
;; Core utility functions of emax


(defun emax-message (message &rest params)
       (message "[emax] %s" message params))

(defun emax-message-line (message)
  (emax-message "--------------------------------------------------------------------------------------------")
  (emax-message message)
  (emax-message "--------------------------------------------------------------------------------------------")
  nil)

(defun emax-make-module-file-name (symbol)
  (concat (symbol-name symbol) ".el"))


(defun emax-load-module (module)
  "Loads a module from emax which is basically just an '.el' file"
  (interactive)
  (let ((module-file-name (emax-make-module-file-name module)))
    (emax-message "--------------------------------------------------------------------------------------------")
    (emax-message (format "Loading module \"%s\"" module-file-name))
    (condition-case err
	(progn
	  (load module-file-name)
	  (emax-message "Module loaded, no errors."))
      (error
       (emax-message (format "\nERROR: Failed to load module \"%s\": %s\n" module-file-name err)) )))
  (emax-message "--------------------------------------------------------------------------------------------\n"))

(defun emax-load (module-list)
  "Goes through the list of modules and loads them, each module item should be a pair with the module name and t for wether to load it or nil if not"
  (emax-message "Loading modules from \"init-for-ui\"")
  (dolist (item module-list)
    (let ((module (car item))
	  (enabled (car (cdr item))))
      (if enabled
	  (emax-load-module module)
	(emax-message (format "Module \"%s\" - not enabled" module))))))

(defun emax-make-conf-file-name (symbol)
  (concat (symbol-name symbol) "-conf.el"))


(defun emax-conf-module (module)
  "Configures a module"
  (interactive)
  (let ((module-conf-name (emax-make-conf-file-name module)))
    (emax-message (format "Configuring  module \"%s\"" module-conf-name))
    (condition-case err
	(progn
	  (load module-conf-name)
	  (emax-message "Module configured, no errors."))
      (error
       (emax-message (format "\nERROR: Failed to load module \"%s\": %s\n" module-conf-name err)) ))))

(defun emax-conf (module-list)
  "Go through the list of modules (as MODULE-LIST) and configures."
  (emax-message "Configuring modules...")
  (dolist (item module-list)
    (let ((module  (car item))
	  (enabled (cdr item)))
      (if enabled
	  (emax-conf-module module)
	(emax-message (format "Module \"%s\" - not enabled" module))))))

(provide 'emax-core)
;;; emax-core.el ends here
