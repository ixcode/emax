;;; foundations.el --- Sets up useful things for programming

;;; Commentary:

;;; https://www.google.com/search?q=make+fish+default+shell&ie=utf-8&oe=utf-8&client=firefox-b-ab
;;; Code:

(setq explicit-shell-file-name "/bin/zsh")

(use-package eterm-256color
  :ensure t)

(require 'vterm)

(defun emax-insert-pound-vterm ()
  "Insert the # character into a vterm."
  (interactive)
  (vterm-send-string "#"))

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("s-v" . vterm-yank)
              ("M-3" . emax-insert-pound-vterm)))

(add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

(setq eterm-256color-disable-bold t)
(add-hook 'term-mode-hook #'eterm-256color-mode)


;; TOdo - add keybinding to map vterm-yank to s-V


;; Disable all bold fonts
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

(load "ssh-shell.el")

;;(setq explicit-shell-file-name "/bin/bash")
;;(setq explicit-shell-file-name "/usr/local/bin/fish")
(set-default 'system-uses-terminfo nil)



(when (not (package-installed-p 'vagrant-tramp))
  (package-install 'vagrant-tramp))

(eval-after-load 'tramp
  '(vagrant-tramp-enable))


;;(when (not (package-installed-p 'top-mode))
;;  (package-install 'top-mode))


(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


(ansi-color-for-comint-mode-on)

;; http://stackoverflow.com/questions/13763912/emacs-how-to-change-some-colors-in-m-x-shell

;;(ansi-color-names-vector [,minamin-bg-light ,minamin-red , minamin-green-1 ,minamin-yellow-1 ,"blue" ,minamin-magenta ,minamin-blue+1 ,minamin-fg])

;; Need to work out how to do this in the theme

;; Also need to change it based on the theme
;; ["#fffff5" "#cc0000" "#8FB28F" "#813a01" "#271aff" "#6622CC" "#1a87ff" "#333c4e"] ;; this is for light mode
;; ["black" "red" "green" "yellow" "PaleBlue" "magenta" "cyan" "white"] ;; This is the default
(defun refresh-shell-colors ()
  (interactive)
  (setq ansi-color-names-vector
   ["#3B425" "#C16069" "#A2BF8A" "#ECCC87" "#80A0C2" "#B58DAE" "#86C0D1" "#f0f0f0"]) ;; these are nord colors
  (setq ansi-color-map (ansi-color-make-color-map))
  (message "Have reset ansi colors"))

(defun refresh-term-colors ()
   `(term-color-black ((t (:foreground , "#3B425"
                                       :background ,"black"))))
   `(term-color-red ((t (:foreground , "#C16069"
                                       :background ,"black"))))
   `(term-color-green ((t (:foreground ,"#A2BF8A"
                                       :background ,"black"))))
   `(term-color-yellow ((t (:foreground ,"#ECCC87"
                                       :background ,"black"))))
   `(term-color-blue ((t (:foreground ,"#80A0C2"
                                      :background ,"black"))))
   `(term-color-magenta ((t (:foreground ,"#B58DAE"
                                         :background ,"black"))))
   `(term-color-cyan ((t (:foreground ,"#86C0D1"
                                       :background ,"black"))))
   `(term-color-white ((t (:foreground ,"#f0f0f0"
                                       :background ,"black"))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
   (message "Have reset ansi colors for term"))

(defun disable-bold-fonts()
  (interactive)
  (mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal :underline nil))
  (face-list))
  (message "Disabled bold fonts in "))

(add-hook 'shell-mode-hook 'refresh-shell-colors)
(add-hook 'term-mode-hook 'disable-bold-fonts)
;;(setq ansi-color-map (ansi-color-make-color-map))

;; Filter out dogy characters
;; https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/
(defun preamble-regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

;; https://www.reddit.com/r/emacs/comments/88yzp4/better_way_to_run_terminals_in_emacs/
;; http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/

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


(load "multi-term.el")

;;(require 'multi-term)
;;(setq multi-term-program "/usr/local/bin/fish")
;;(setq multi-shell-use-ansi-color t) ;; sets hook to use ansi color for comint mode

;;(add-hook 'term-mode-hook 'refresh-shell-colors)
(defun fish ()
  "Shortcut for launching multiterm with fish in it."
    (interactive)
  (multi-term))

(eval-after-load "term"
  '(define-key term-raw-map (kbd "s-v") 'term-paste))

(set-default 'explicit-shell-file-name "/bin/zsh")


;; (add-hook
;;      'term-mode-hook
;;      (lambda ()
;;        (message "Trying to override paste function")
;;        (local-unset-key (kbd "s-v"))
;;        (local-set-key (kbd "s-v") 'term-paste)))
;; https://github.com/fish-shell/fish-shell/issues/2983
;; https://github.com/fish-shell/fish-shell/issues/2441
;; https://emacs.stackexchange.com/questions/7372/stray-trailing-4m-before-prompt-with-zsh-in-m-x-ansi-term
;;  https://emacs.stackexchange.com/questions/20545/emacs-colors-being-set-differently-when-term-is-screen-256color-and-xterm-256
;; https://fishshell.com/docs/current/index.html#variables-color

;;https://www.emacswiki.org/emacs/AnsiTermHints
;;https://finiteheap.com/emacs/2015/12/20/ansi-remote-directory-tracking.html
;;https://github.com/dieggsy/esh-autosuggest
(provide 'shells)
;;; shells.el ends here
