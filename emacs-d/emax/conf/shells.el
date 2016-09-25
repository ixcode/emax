(load "ssh-shell.el")

(when (not (package-installed-p 'vagrant-tramp))
  (package-install 'vagrant-tramp))

(eval-after-load 'tramp
  '(vagrant-tramp-enable))

 (require 'multi-term)

;;(when (not (package-installed-p 'top-mode))
;;  (package-install 'top-mode))


(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; http://stackoverflow.com/questions/13763912/emacs-how-to-change-some-colors-in-m-x-shell

;;(ansi-color-names-vector [,minamin-bg-light ,minamin-red , minamin-green-1 ,minamin-yellow-1 ,"blue" ,minamin-magenta ,minamin-blue+1 ,minamin-fg])

;; Need to work out how to do this in the theme
(defun refresh-shell-colors ()
  (setq ansi-color-names-vector
   ["#fffff5" "#cc0000" "#8FB28F" "#813a01" "#271aff" "#6622CC" "#1a87ff" "#333c4e"])
  (setq ansi-color-map (ansi-color-make-color-map))
  (message "Have reset ansi colors"))

(add-hook 'shell-mode-hook 'refresh-shell-colors)
;;(setq ansi-color-map (ansi-color-make-color-map))

;; Filter out dogy characters
;; https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/
(defun preamble-regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(setq non-sgr-control-sequence-regexp
      (preamble-regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         ;; From git log
         ">"
         "="
         )))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)


