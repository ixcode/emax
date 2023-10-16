
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
