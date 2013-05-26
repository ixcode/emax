(setq inhibit-startup-message t)

(load "startup-message-ascii-art.el")

(defun choose-message () 
  "Time to start coding ...")

(defun current-user ()
  (car (split-string (shell-command-to-string "whoami") "\n")))

     
(setq initial-scratch-message      
      (concat *emax*
(car (split-string (version) "\n"))
"
;;
;; Hello " (current-user) ". " (choose-message)
"

"))
