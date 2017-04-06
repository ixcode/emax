;;; web.el --- Sets up irc chat


;;; Commentary:
;; Some useful connectivity to the web

;;; Code:

(when (not (package-installed-p 'google-this))
  (package-install 'google-this))

(require 'google-this)
(google-this-mode 1)


;;; web.el ends here
