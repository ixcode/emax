;;; journal.el --- Provides functions that create journal entries for a diary like journal

;;; Commentary:

;;

;;; Code:

(defun journal-entry-filename ()
  "The format of a journal entry."
  (format-time-string "JOURNAL-ENTRY-%Y-%m-%m-%H:%M.md"))

(defun journal-entry-home ()
  "Create a journal entry in your home dir."
  (interactive)
  (find-file (concat "~/Journal/" (journal-entry-filename))))

(defun journal-entry ()
  "Create a journal entry in the current dir."
  (interactive)
  (find-file (journal-entry-filename)))

(provide 'journal)

;;; journal.el ends here


