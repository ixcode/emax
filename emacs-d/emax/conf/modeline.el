;;; languages.el --- Sets up useful things for programming

;;; Commentary:

;;; Code:

;; Settings for the mode line

;; The display settings for the modeline should be set in the color theme (see my zenburn version for example)





;; Display the time in the mode bar.
;; http://www.emacswiki.org/emacs/DisplayTime
;; Use this to play with the format string...
;; (format-time-string "%R %Y-%m-%d [%Z]" (current-time))

;; Display the time in the mode line:
(setq display-time-format "%Y-%m-%d %R [%Z] ")
(setq display-time-string-forms
     '((format-time-string display-time-format (current-time))))

(setq display-time-and-date t)
(display-time-mode 1)

(display-battery-mode)

;; Making all the modes show up as symbols
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas-minor-mode . " γ")
    (paredit-mode . " Φ")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . " τ")
    (volatile-highlights-mode . " υ")
    (elisp-slime-nav-mode . " δ")
    (nrepl-mode . " ηζ")
    (nrepl-interaction-mode . " ηζ")
    ;; Major modes
    (lisp-interaction-mode . "ξlisp")
    (clojure-mode . "Cloλure")
    (EL-mode . "λ")
    (lambda-mode . "")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "ξlisp")
    (markdown-mode . "md"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")
 
 
(defun clean-mode-line ()
  "Cleans up the modeline."
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))
 
 
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

 
 
;;; Greek letters - C-u C-\ greek ;; C-\ to revert to default
;;; ς ε ρ τ υ θ ι ο π α σ δ φ γ η ξ κ λ ζ χ ψ ω β ν μ

;; from http://amitp.blogspot.co.uk/2011/08/emacs-custom-mode-line.html


;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-unmodified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

;; (set-face-attribute 'mode-line nil
;;     :foreground "gray60" :background "gray40"
;;     :inverse-video nil
;;     :box '(:line-width 6 :color "gray40" :style nil))
;; (set-face-attribute 'mode-line-inactive nil
;;     :foreground "gray80" :background "gray30"
;;     :inverse-video nil
;;     :box '(:line-width 6 :color "gray30" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    ;;:background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-unmodified-face nil
    :inherit 'mode-line-face
    
    )
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo"
    :foreground "green"
    :height 130)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray90")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray70"
    :height 130)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")



(setq-default
 mode-line-format
 '(
   "%e "
   (:propertize "%b "    face mode-line-filename-face)
   (:propertize "%4l " face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   " [%p] "
   emacsclient
   mode-line-misc-info
   " [" (:propertize mode-name face mode-line-mode-face) 
   (:eval (propertize (format-mode-line minor-mode-alist) 'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process face mode-line-process-face) "]"
   (vc-mode vc-mode) " "
   
   (:eval
    (cond (buffer-read-only     (propertize "R" 'face 'mode-line-read-only-face))
          ((buffer-modified-p)  (propertize "+" 'face 'mode-line-modified-face))
          (t                    (propertize " " 'face 'mode-line-unmodified-face))))   
   mode-line-end-spaces))





;;   mode-line-front-space   
;;   mode-line-mule-info
;;   mode-line-client
;;   mode-line-modified
;;   mode-line-remote    
   ;;   mode-line-frame-identification
   ;;   mode-line-buffer-identification "   "





(provide 'modeline)
;;; modeline.el ends here
