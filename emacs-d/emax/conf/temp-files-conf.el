;; Make sure all backup files only live in one place
;; The symbolic links which begin with .# are not temp files, but locking files (http://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html#Interlocking)
;; You can make them go away by setting create-lockfiles to nil
;; Save all tempfiles in $TMPDIR/emacs$UID/                                                        

(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs" (user-uid)))

(setq backup-directory-alist `((".*" . ,emacs-tmp-dir))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

(message (format "[emax] All temporary files are being stored to %s" emacs-tmp-dir))
