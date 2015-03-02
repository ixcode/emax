;; Provides functions that create journal entries for a diary like journal.

(defun journal-entry-filename ()  
  (format-time-string "JOURNAL-ENTRY-%Y-%m-%m-%H:%M.md"))

(defun journal-entry ()
  (interactive)
  (find-file (concat "/Users/jmdb/Journal/" (journal-entry-filename))))



