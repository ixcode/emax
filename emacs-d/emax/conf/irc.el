;;; irc.el --- Sets up irc chat


;;; Commentary:
;; http://emacs-fu.blogspot.co.uk/2009/06/erc-emacs-irc-client.html
;; http://www.irchelp.org/
;;; Code:


;; (when (not (package-installed-p 'weechat))
;;   (package-install 'weechat))

(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-save-buffer-on-part t)

(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
  '((".*\\.freenode.net" "#emacs" "#thoughtworks")
     ("irc.dyne.org" "#decode")))

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun erc-go ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?

    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "jmdb" :full-name "Jim Barritt")
      (erc :server "irc.dyne.org" :port 6667 :nick "jmdb" :full-name "Jim Barritt"))))




;;; irc.el ends here
