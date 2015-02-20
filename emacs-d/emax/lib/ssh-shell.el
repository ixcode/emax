;; Provides some functions for making remote shells easier 
;; A bit beta right now so theres no packaging or anything

(defun first (list)
  (car list))

(defun current-hostname ()
  (first (split-string (shell-command-to-string "hostname") "\n")))

(defun current-user ()
  (first (split-string (shell-command-to-string "whoami") "\n")))


(defun rename-shell-to-host ()
  (interactive)
  (let ((buffer-name (concat "*shell:" (current-user) "@" (current-hostname) "*"))) 
    (message (concat "Renaming current buffer to " buffer-name))
    (rename-buffer buffer-name)))

(defun rename-shell (name)
  "…"
  (interactive "sEnter name for this shell: ")
  (let ((buffer-name (concat "*shell:" (current-hostname) ":" name "*"))) 
    (message (concat "Renaming current buffer to " buffer-name))
    (rename-buffer buffer-name)))

; turn off shell command echo
(defun my-comint-init () 
  (setq comint-process-echoes t)) 

;;(shell "/ssh:root@5.79.7.4:")



(defun ssh-shell (input)
  "You must specify the right string, like user@host: in your input. Notice the colon at the end! thats so if you want to specify a dir you can"
  (interactive
   (let ((ssh-path (read-string "SSH To: " nil 'my-history)))     
                 (let ((remote-dir (concat "/ssh:" ssh-path))) 
                   (message (concat "Opening shell at " remote-dir))
                   (dired remote-dir)
                   (shell (concat "*shell:" (current-user) "@" (current-hostname) "*"))))))



(defun clear-shell ()
  (interactive)  
  (mark-whole-buffer)
  (delete-backward-char)
  (comint-send-input))



