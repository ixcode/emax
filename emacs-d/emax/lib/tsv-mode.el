;;; tsv-mode.el --- Major mode for editing tab separated values  -*- lexical-binding: t -*-

;; Copyright (C) 2003, 2004, 2012, 2013, 2016  Free Software Foundation, Inc

;; Original-Author: Francis J. Wright <F.J.Wright at qmul.ac.uk>
;; Author: Jim Barritt <jim at planet-ix.com>
;; Time-stamp: <16 September 2016>
;; URL: http://centaur.maths.qmul.ac.uk/Emacs/
;; Version: 1.2
;; Keywords: convenience

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements TSV mode, a major mode for editing records
;; in a generalized TSV (tab-separated values) format.  It binds
;; finds with prefix ".tsv" to `tsv-mode' in `auto-mode-alist'.

;; In CSV mode, the following commands are available:

;; - C-c C-s (`tsv-sort-fields') and C-c C-n (`tsv-sort-numeric-fields')
;;   respectively sort lexicographically and numerically on a
;;   specified field or column.

;; - C-c C-r (`tsv-reverse-region') reverses the order.  (These
;;   commands are based closely on, and use, code in `sort.el'.)

;; - C-c C-k (`tsv-kill-fields') and C-c C-y (`tsv-yank-fields') kill
;;   and yank fields or columns, although they do not use the normal
;;   kill ring.  C-c C-k can kill more than one field at once, but
;;   multiple killed fields can be yanked only as a fixed group
;;   equivalent to a single field.

;; - C-c C-a (`tsv-align-fields') aligns fields into columns

;; - C-c C-u (`tsv-unalign-fields') undoes such alignment; separators
;;   can be hidden within aligned records.

;; - C-c C-t (`tsv-transpose') interchanges rows and columns.  For
;;   details, see the documentation for the individual commands.

;; CSV mode can recognize fields separated by any of several single
;; characters, specified by the value of the customizable user option
;; `tsv-separators'.  CSV data fields can be delimited by quote
;; characters (and must if they contain separator characters).  This
;; implementation supports quoted fields, where the quote characters
;; allowed are specified by the value of the customizable user option
;; `tsv-field-quotes'.  By default, the only separator is a comma and
;; the only field quote is a double quote.  These user options can be
;; changed ONLY by customizing them, e.g. via M-x customize-variable.

;; TSV mode commands ignore blank lines and comment lines beginning
;; with the value of the buffer local variable `tsv-comment-start',
;; which by default is #.  The user interface is similar to that of
;; the standard commands `sort-fields' and `sort-numeric-fields', but
;; see the major mode documentation below.

;; The global minor mode `tsv-field-index-mode' provides display of
;; the current field index in the mode line, cf. `line-number-mode'
;; and `column-number-mode'.  It is on by default.

;;; Installation:

;; Put this file somewhere that Emacs can find it (i.e. in one of the
;; directories in your `load-path' such as `site-lisp'), optionally
;; byte-compile it (recommended), and put this in your .emacs file:
;;
;; (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . tsv-mode))
;; (autoload 'tsv-mode "tsv-mode"
;;   "Major mode for editing comma-separated value files." t)

;;; History:

;; Begun on 15 November 2003 to provide lexicographic sorting of
;; simple TSV data by field and released as csv.el.  Facilities to
;; kill multiple fields and customize separator added on 9 April 2004.
;; Converted to a major mode and renamed tsv-mode.el on 10 April 2004,
;; partly at the suggestion of Stefan Monnier <monnier at
;; IRO.UMontreal.CA> to avoid conflict with csv.el by Ulf Jasper.
;; Field alignment, comment support and TSV mode customization group
;; added on 1 May 2004.  Support for index ranges added on 6 June
;; 2004.  Multiple field separators added on 12 June 2004.
;; Transposition added on 22 June 2004.  Separator invisibility added
;; on 23 June 2004.

;;; See also:

;; the standard GNU Emacs 21 packages align.el, which will align
;; columns within a region, and delim-col.el, which helps to prettify
;; columns in a text region or rectangle;

;; csv.el by Ulf Jasper <ulf.jasper at web.de>, which provides
;; functions for reading/parsing comma-separated value files and is
;; available at http://de.geocities.com/ulf_jasper/emacs.html (and in
;; the gnu.emacs.sources archives).

;;; To do (maybe):

;; Make separators and quotes buffer-local and locally settable.
;; Support (La)TeX tables: set separator and comment; support record
;; end string.
;; Convert comma-separated to space- or tab-separated.

;;; Code:

(defgroup TSV nil
  "Major mode for editing files of comma-separated value type."
  :group 'convenience)

(defvar tsv-separator-chars nil
  "Field separators as a list of character.
Set by customizing `tsv-separators' -- do not set directly!")

(defvar tsv-separator-regexp nil
  "Regexp to match a field separator.
Set by customizing `tsv-separators' -- do not set directly!")

(defvar tsv--skip-regexp nil
  "Regexp used by `skip-chars-forward' etc. to skip fields. Set by customizing `tsv-separators' -- do not set directly!")

(defvar tsv-font-lock-keywords nil
  "Font lock keywords to highlight the field separators in TSV mode.
Set by customizing `tsv-separators' -- do not set directly!")

(defcustom tsv-separators '("\t")
  "Field separators: a list of *single-character* strings.
For example: (\",\"), the default, or (\",\" \";\" \":\").
Neighbouring fields may be separated by any one of these characters.
The first is used when inserting a field separator into the buffer.
All must be different from the field quote characters, `tsv-field-quotes'."
  ;; Suggested by Eckhard Neber <neber@mwt.e-technik.uni-ulm.de>
  :type '(repeat string)
  ;; FIXME: Character would be better, but in Emacs 21.3 does not display
  ;; correctly in a customization buffer.
  :set (lambda (variable value)
	 (mapc (lambda (x)
		 (if (/= (length x) 1)
		     (error "Non-single-char string %S" x))
                 (if (and (boundp 'tsv-field-quotes)
                          (member x tsv-field-quotes))
                     (error "%S is already a quote" x)))
	       value)
	 (custom-set-default variable value)
	 (setq tsv-separator-chars (mapcar 'string-to-char value)
	       tsv--skip-regexp (apply 'concat "^\n" tsv-separators)
	       tsv-separator-regexp (apply 'concat `("[" ,@value "]"))
	       tsv-font-lock-keywords
	       ;; NB: tsv-separator-face variable evaluates to itself.
	       `((,tsv-separator-regexp (0 'tsv-separator-face))))))

(defcustom tsv-field-quotes '("\"")
  "Field quotes: a list of *single-character* strings.
For example: (\"\\\"\"), the default, or (\"\\\"\" \"'\" \"`\").
A field can be delimited by a pair of any of these characters.
All must be different from the field separators, `tsv-separators'."
  :type '(repeat string)
  ;; Character would be better, but in Emacs 21 does not display
  ;; correctly in a customization buffer.
  :set (lambda (variable value)
	 (mapc (lambda (x)
		 (if (/= (length x) 1)
		     (error "Non-single-char string %S" x))
		 (if (member x tsv-separators)
		     (error "%S is already a separator" x)))
	       value)
	 (when (boundp 'tsv-mode-syntax-table)
	   ;; FIRST remove old quote syntax:
	   (with-syntax-table text-mode-syntax-table
	     (mapc (lambda (x)
		     (modify-syntax-entry
		      (string-to-char x)
		      (string (char-syntax (string-to-char x)))
		      ;; symbol-value to avoid compiler warning:
		      (symbol-value 'tsv-mode-syntax-table)))
		   tsv-field-quotes))
	   ;; THEN set new quote syntax:
	   (tsv-set-quote-syntax value))
	 ;; BEFORE setting new value of `tsv-field-quotes':
	 (custom-set-default variable value)))

(defun tsv-set-quote-syntax (field-quotes)
  "Set syntax for field quote characters FIELD-QUOTES to be \"string\".
FIELD-QUOTES should be a list of single-character strings."
  (mapc (lambda (x)
	  (modify-syntax-entry
	   (string-to-char x) "\""
	   ;; symbol-value to avoid compiler warning:
	   (symbol-value 'tsv-mode-syntax-table)))
	field-quotes))

(defvar tsv-comment-start nil
  "String that starts a comment line, or nil if no comment syntax.
Such comment lines are ignored by TSV mode commands.
This variable is buffer local\; its default value is that of
`tsv-comment-start-default'.  It is set by the function
`tsv-set-comment-start' -- do not set it directly!")

(make-variable-buffer-local 'tsv-comment-start)

(defcustom tsv-comment-start-default "#"
  "String that starts a comment line, or nil if no comment syntax.
Such comment lines are ignored by TSV mode commands.
Default value of buffer-local variable `tsv-comment-start'.
Changing this variable does not affect any existing TSV mode buffer."
  :type '(choice (const :tag "None" nil) string)
  :set (lambda (variable value)
	 (custom-set-default variable value)
	 (set-default 'tsv-comment-start value)))

(defcustom tsv-align-style 'left
  "Aligned field style: one of 'left, 'centre, 'right or 'auto.
Alignment style used by `tsv-align-fields'.
Auto-alignment means left align text and right align numbers."
  :type '(choice (const left) (const centre)
		 (const right) (const auto)))

(defcustom tsv-align-padding 1
  "Aligned field spacing: must be a positive integer.
Number of spaces used by `tsv-align-fields' after separators."
  :type 'integer)

(defcustom tsv-header-lines 0
  "Header lines to skip when setting region automatically."
  :type 'integer)

(defcustom tsv-invisibility-default t
  "If non-nil, make separators in aligned records invisible."
  :type 'boolean)

(defface tsv-separator-face
  '((t :inherit escape-glyph))
  "TSV mode face used to highlight separators.")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Mode definition, key bindings and menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tsv-mode-line-help-echo
  ;; See bindings.el for details of `mode-line-format' construction.
  (get-text-property 0 'help-echo (car (default-value 'mode-line-format)))
  "Primary default mode line help echo text.")

;; Strangely the (last) function syntax seems incorrect... I had to swap the list and the number
(defconst tsv-mode-line-format
  ;; See bindings.el for details of `mode-line-format' construction.
  (let* ((ml (copy-sequence (default-value 'mode-line-format)))
         (x (or (memq 'mode-line-position ml) (last ml 3))))
    (when x
      (setcdr x (cons
                 `(tsv-field-index-string
                   ("" tsv-field-index-string))
                 (cdr x))))
    ml)
  "Mode line format string for TSV mode.")


(defvar tsv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?v)] 'tsv-toggle-invisibility)
    (define-key map [(control ?c) (control ?t)] 'tsv-transpose)
    (define-key map [(control ?c) (control ?c)] 'tsv-set-comment-start)
    (define-key map [(control ?c) (control ?u)] 'tsv-unalign-fields)
    (define-key map [(control ?c) (control ?a)] 'tsv-align-fields)
    (define-key map [(control ?c) (control ?z)] 'tsv-yank-as-new-table)
    (define-key map [(control ?c) (control ?y)] 'tsv-yank-fields)
    (define-key map [(control ?c) (control ?k)] 'tsv-kill-fields)
    (define-key map [(control ?c) (control ?d)] 'tsv-toggle-descending)
    (define-key map [(control ?c) (control ?r)] 'tsv-reverse-region)
    (define-key map [(control ?c) (control ?n)] 'tsv-sort-numeric-fields)
    (define-key map [(control ?c) (control ?s)] 'tsv-sort-fields)
    map))

;;;###autoload
(define-derived-mode tsv-mode text-mode "TSV"
  "Major mode for editing files of comma-separated value type.

TSV mode is derived from `text-mode', and runs `text-mode-hook' before
running `tsv-mode-hook'.  It turns `auto-fill-mode' off by default.
TSV mode can be customized by user options in the TSV customization
group.  The separators are specified by the value of `tsv-separators'.

TSV mode commands ignore blank lines and comment lines beginning with
the value of `tsv-comment-start', which delimit \"paragraphs\".
\"Sexp\" is re-interpreted to mean \"field\", so that `forward-sexp'
\(\\[forward-sexp]), `kill-sexp' (\\[kill-sexp]), etc. all apply to fields.
Standard comment commands apply, such as `comment-dwim' (\\[comment-dwim]).

If `font-lock-mode' is enabled then separators, quoted values and
comment lines are highlighted using respectively `tsv-separator-face',
`font-lock-string-face' and `font-lock-comment-face'.

The user interface (UI) for TSV mode commands is similar to that of
the standard commands `sort-fields' and `sort-numeric-fields', except
that if there is no prefix argument then the UI prompts for the field
index or indices.  In `transient-mark-mode' only: if the region is not
set then the UI attempts to set it to include all consecutive TSV
records around point, and prompts for confirmation; if there is no
prefix argument then the UI prompts for it, offering as a default the
index of the field containing point if the region was not set
explicitly.  The region set automatically is delimited by blank lines
and comment lines, and the number of header lines at the beginning of
the region given by the value of `tsv-header-lines' are skipped.

Sort order is controlled by `tsv-descending'.

TSV mode provides the following specific keyboard key bindings:

\\{tsv-mode-map}"  
  (turn-off-auto-fill)
  ;; Set syntax for field quotes:
  (tsv-set-quote-syntax tsv-field-quotes)
  ;; Make sexp functions apply to fields:
  (set (make-local-variable 'forward-sexp-function) 'tsv-forward-field)
  (tsv-set-comment-start tsv-comment-start)
  (setq
   ;; Font locking -- separator plus syntactic:
   font-lock-defaults '(tsv-font-lock-keywords)
   buffer-invisibility-spec tsv-invisibility-default
   ;; Mode line to support `tsv-field-index-mode':
   mode-line-format tsv-mode-line-format)
  (set (make-local-variable 'truncate-lines) t)
  ;; Enable or disable `tsv-field-index-mode' (could probably do this
  ;; a bit more efficiently):
  (tsv-field-index-mode (symbol-value 'tsv-field-index-mode)))

(defun tsv-set-comment-start (string)
  "Set comment start for this TSV mode buffer to STRING.
It must be either a string or nil."
  (interactive
   (list (edit-and-eval-command
	  "Comment start (string or nil): " tsv-comment-start)))
  ;; Paragraph means a group of contiguous records:
  (setq tsv-comment-start string)
  (set (make-local-variable 'paragraph-separate) "[:space:]*$") ; White space.
  (set (make-local-variable 'paragraph-start) "\n");Must include \n explicitly!
  (if string
      (progn
	(setq paragraph-separate (concat paragraph-separate "\\|" string)
	      paragraph-start (concat paragraph-start "\\|" string))
        (set (make-local-variable 'comment-start) string)
	(modify-syntax-entry
	 (string-to-char string) "<" tsv-mode-syntax-table)
	(modify-syntax-entry ?\n ">" tsv-mode-syntax-table))
    (with-syntax-table text-mode-syntax-table
      (modify-syntax-entry (string-to-char string)
			   (string (char-syntax (string-to-char string)))
			   tsv-mode-syntax-table)
      (modify-syntax-entry ?\n
			   (string (char-syntax ?\n))
			   tsv-mode-syntax-table))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[Tc][Ss][Vv]\\'" . tsv-mode))

(defvar tsv-descending nil
  "If non-nil, TSV mode sort functions sort in order of descending sort key.
Usually they sort in order of ascending sort key.")

(defun tsv-toggle-descending ()
  "Toggle `tsv-descending'."
  (interactive)
  (setq tsv-descending (not tsv-descending))
  (message "Sort order is %sscending" (if tsv-descending "de" "a")))

(defun tsv-toggle-invisibility ()
  "Toggle `buffer-invisibility-spec'."
  (interactive)
  (setq buffer-invisibility-spec (not buffer-invisibility-spec))
  (message "Separators in aligned records will be %svisible \
\(after re-aligning if soft\)"
	   (if buffer-invisibility-spec "in" ""))
  (redraw-frame (selected-frame)))

(easy-menu-define
  tsv-menu
  tsv-mode-map
  "TSV major mode menu keymap"
  '("TSV"
    ["Sort By Field Lexicographically" tsv-sort-fields :active t
     :help "Sort lines in region lexicographically by the specified field"]
    ["Sort By Field Numerically" tsv-sort-numeric-fields :active t
     :help "Sort lines in region numerically by the specified field"]
    ["Reverse Order of Lines" tsv-reverse-region :active t
     :help "Reverse the order of the lines in the region"]
    ["Use Descending Sort Order" tsv-toggle-descending :active t
     :style toggle :selected tsv-descending
     :help "If selected, use descending order when sorting"]
    "--"
    ["Kill Fields (Columns)" tsv-kill-fields :active t
     :help "Kill specified fields of each line in the region"]
    ["Yank Fields (Columns)" tsv-yank-fields :active t
     :help "Yank killed fields as specified field of each line in region"]
    ["Yank As New Table" tsv-yank-as-new-table :active t
     :help "Yank killed fields as a new table at point"]
    ["Align Fields into Columns" tsv-align-fields :active t
     :help "Align the start of every field of each line in the region"]
    ["Unalign Columns into Fields" tsv-unalign-fields :active t
     :help "Undo soft alignment and optionally remove redundant white space"]
    ["Transpose Rows and Columns" tsv-transpose :active t
     :help "Rewrite rows (which may have different lengths) as columns"]
    "--"
    ["Forward Field" forward-sexp :active t
     :help "Move forward across one field\; with ARG, do it that many times"]
    ["Backward Field" backward-sexp :active t
     :help "Move backward across one field\; with ARG, do it that many times"]
    ["Kill Field Forward" kill-sexp :active t
     :help "Kill field following cursor\; with ARG, do it that many times"]
    ["Kill Field Backward" backward-kill-sexp :active t
     :help "Kill field preceding cursor\; with ARG, do it that many times"]
    "--"
    ("Alignment Style"
     ["Left" (setq tsv-align-style 'left) :active t
      :style radio :selected (eq tsv-align-style 'left)
      :help "If selected, `tsv-align-fields' left aligns fields"]
     ["Centre" (setq tsv-align-style 'centre) :active t
      :style radio :selected (eq tsv-align-style 'centre)
      :help "If selected, `tsv-align-fields' centres fields"]
     ["Right" (setq tsv-align-style 'right) :active t
      :style radio :selected (eq tsv-align-style 'right)
      :help "If selected, `tsv-align-fields' right aligns fields"]
     ["Auto" (setq tsv-align-style 'auto) :active t
      :style radio :selected (eq tsv-align-style 'auto)
      :help "\
If selected, `tsv-align-fields' left aligns text and right aligns numbers"]
     )
    ["Show Current Field Index" tsv-field-index-mode :active t
     :style toggle :selected tsv-field-index-mode
     :help "If selected, display current field index in mode line"]
    ["Make Separators Invisible" tsv-toggle-invisibility :active t
     :style toggle :selected buffer-invisibility-spec
     :help "If selected, separators in aligned records are invisible"]
    ["Set Buffer's Comment Start" tsv-set-comment-start :active t
     :help "Set comment start string for this buffer"]
    ["Customize TSV Mode" (customize-group 'TSV) :active t
     :help "Open a customization buffer to change TSV mode options"]
    ))

(require 'sort)

(defsubst tsv-not-looking-at-record ()
  "Return t if looking at blank or comment line, nil otherwise.
Assumes point is at beginning of line."
  (looking-at paragraph-separate))

(defun tsv-interactive-args (&optional type)
  "Get arg or field(s) and region interactively, offering sensible defaults.
Signal an error if the buffer is read-only.
If TYPE is noarg then return a list `(beg end)'.
Otherwise, return a list `(arg beg end)', where arg is:
  the raw prefix argument by default\;
  a single field index if TYPE is single\;
  a list of field indices or index ranges if TYPE is multiple.
Field defaults to the current prefix arg\; if not set, prompt user.

A field index list consists of positive or negative integers or ranges,
separated by any non-integer characters.  A range has the form m-n,
where m and n are positive or negative integers, m < n, and n defaults
to the last field index if omitted.

In transient mark mode, if the mark is not active then automatically
select and highlight TSV records around point, and query user.
The default field when read interactively is the current field."
  ;; Must be run interactively to activate mark!
  (let* ((arg current-prefix-arg) (default-field 1)
	 (region
	  (if (not (use-region-p))
	      ;; Set region automatically:
	      (save-excursion
                (if arg
                    (beginning-of-line)
                  (let ((lbp (line-beginning-position)))
                    (while (re-search-backward tsv-separator-regexp lbp 1)
                      ;; Move as far as possible, i.e. to beginning of line.
                      (setq default-field (1+ default-field)))))
                (if (tsv-not-looking-at-record)
                    (error "Point must be within TSV records"))
		(let ((startline (point)))
		  ;; Set mark at beginning of region:
		  (while (not (or (bobp) (tsv-not-looking-at-record)))
		    (forward-line -1))
		  (if (tsv-not-looking-at-record) (forward-line 1))
		  ;; Skip header lines:
		  (forward-line tsv-header-lines)
		  (set-mark (point))	; OK since in save-excursion
		  ;; Move point to end of region:
		  (goto-char startline)
		  (beginning-of-line)
		  (while (not (or (eobp) (tsv-not-looking-at-record)))
		    (forward-line 1))
		  ;; Show mark briefly if necessary:
		  (unless (and (pos-visible-in-window-p)
			       (pos-visible-in-window-p (mark)))
		    (exchange-point-and-mark)
		    (sit-for 1)
		    (exchange-point-and-mark))
		  (or (y-or-n-p "Region OK? ")
		      (error "Action aborted by user"))
		  (message nil)		; clear y-or-n-p message
		  (list (region-beginning) (region-end))))
	    ;; Use region set by user:
	    (list (region-beginning) (region-end)))))
    (setq default-field (number-to-string default-field))
    (cond
     ((eq type 'multiple)
      (if arg
	  ;; Ensure that field is a list:
	  (or (consp arg)
	      (setq arg (list (prefix-numeric-value arg))))
	;; Read field interactively, ignoring non-integers:
	(setq arg
	      (mapcar
	       (lambda (x)
		 (if (string-match "-" x 1) ; not first character
		     ;; Return a range as a pair - the cdr may be nil:
		     (let ((m (substring x 0 (match-beginning 0)))
			   (n (substring x (match-end 0))))
		       (cons (car (read-from-string m))
			     (and (not (string= n ""))
				  (car (read-from-string n)))))
		   ;; Return a number as a number:
		   (car (read-from-string x))))
	       (split-string
		(read-string
		 "Fields (sequence of integers or ranges): " default-field)
		"[^-+0-9]+")))))
     ((eq type 'single)
      (if arg
	  (setq arg (prefix-numeric-value arg))
	(while (not (integerp arg))
	  (setq arg (eval-minibuffer "Field (integer): " default-field))))))
    (if (eq type 'noarg) region (cons arg region))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Sorting by field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tsv-nextrecfun ()
  "Called by `tsv-sort-fields-1' with point at end of previous record.
It moves point to the start of the next record.
It should move point to the end of the buffer if there are no more records."
  (forward-line)
  (while (and (not (eobp)) (tsv-not-looking-at-record))
    (forward-line)))

(defun tsv-sort-fields-1 (field beg end startkeyfun endkeyfun)
  "Modified version of `sort-fields-1' that skips blank or comment lines.

FIELD is a single field index, and BEG and END specify the region to
sort.

STARTKEYFUN moves from the start of the record to the start of the key.
It may return either a non-nil value to be used as the key, or
else the key is the substring between the values of point after
STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
starts at the beginning of the record.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN."
  (let ((tbl (syntax-table)))
    (if (zerop field) (setq field 1))
    (unwind-protect
	(save-excursion
	  (save-restriction
	    (narrow-to-region beg end)
	    (goto-char (point-min))
	    (set-syntax-table sort-fields-syntax-table)
	    (sort-subr tsv-descending
		       'tsv-nextrecfun 'end-of-line
		       startkeyfun endkeyfun)))
      (set-syntax-table tbl))))

(defun tsv-sort-fields (field beg end)
  "Sort lines in region lexicographically by the ARGth field of each line.
If not set, the region defaults to the TSV records around point.
Fields are separated by `tsv-separators' and null fields are allowed anywhere.
Field indices increase from 1 on the left or decrease from -1 on the right.
A prefix argument specifies a single field, otherwise prompt for field index.
Ignore blank and comment lines.  The variable `sort-fold-case'
determines whether alphabetic case affects the sort order.
When called non-interactively, FIELD is a single field index\;
BEG and END specify the region to sort."
  ;; (interactive "*P\nr")
  (interactive (tsv-interactive-args 'single))
  (barf-if-buffer-read-only)
  (tsv-sort-fields-1 field beg end
		     (lambda () (tsv-sort-skip-fields field) nil)
		     (lambda () (skip-chars-forward tsv--skip-regexp))))

(defun tsv-sort-numeric-fields (field beg end)
  "Sort lines in region numerically by the ARGth field of each line.
If not set, the region defaults to the TSV records around point.
Fields are separated by `tsv-separators'.
Null fields are allowed anywhere and sort as zeros.
Field indices increase from 1 on the left or decrease from -1 on the right.
A prefix argument specifies a single field, otherwise prompt for field index.
Specified non-null field must contain a number in each line of the region,
which may begin with \"0x\" or \"0\" for hexadecimal and octal values.
Otherwise, the number is interpreted according to sort-numeric-base.
Ignore blank and comment lines.
When called non-interactively, FIELD is a single field index\;
BEG and END specify the region to sort."
  ;; (interactive "*P\nr")
  (interactive (tsv-interactive-args 'single))
  (barf-if-buffer-read-only)
  (tsv-sort-fields-1 field beg end
		 (lambda ()
		   (tsv-sort-skip-fields field)
		   (let* ((case-fold-search t)
			  (base
			   (if (looking-at "\\(0x\\)[0-9a-f]\\|\\(0\\)[0-7]")
			       (cond ((match-beginning 1)
				      (goto-char (match-end 1))
				      16)
				     ((match-beginning 2)
				      (goto-char (match-end 2))
				      8)
				     (t nil)))))
		     (string-to-number (buffer-substring (point)
							 (save-excursion
							   (forward-sexp 1)
							   (point)))
				       (or base sort-numeric-base))))
		 nil))

(defun tsv-reverse-region (beg end)
  "Reverse the order of the lines in the region.
This is just a tsv-mode style interface to `reverse-region', which is
the function that should be used non-interactively.  It takes two
point or marker arguments, BEG and END, delimiting the region."
  ;; (interactive "*P\nr")
  (interactive (tsv-interactive-args 'noarg))
  (barf-if-buffer-read-only)
  (reverse-region beg end))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Moving by field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst tsv-end-of-field ()
  "Skip forward over one field."
  (skip-chars-forward " ")
  (if (eq (char-syntax (following-char)) ?\")
      (goto-char (scan-sexps (point) 1)))
  (skip-chars-forward tsv--skip-regexp))

(defsubst tsv-beginning-of-field ()
  "Skip backward over one field."
  (skip-syntax-backward " ")
  (if (eq (char-syntax (preceding-char)) ?\")
      (goto-char (scan-sexps (point) -1)))
  (skip-chars-backward tsv--skip-regexp))

(defun tsv-forward-field (arg)
  "Move forward across one field, cf. `forward-sexp'.
With ARG, do it that many times.  Negative arg -N means
move backward across N fields."
  (interactive "p")
  (if (< arg 0)
      (tsv-backward-field (- arg))
    (while (>= (setq arg (1- arg)) 0)
      (if (or (bolp)
	      (when (and (not (eobp)) (eolp)) (forward-char) t))
	  (while (and (not (eobp)) (tsv-not-looking-at-record))
	    (forward-line 1)))
      (if (memq (following-char) tsv-separator-chars) (forward-char))
      (tsv-end-of-field))))

(defun tsv-backward-field (arg)
  "Move backward across one field, cf. `backward-sexp'.
With ARG, do it that many times.  Negative arg -N means
move forward across N fields."
  (interactive "p")
  (if (< arg 0)
      (tsv-forward-field (- arg))
    (while (>= (setq arg (1- arg)) 0)
      (when (or (eolp)
		(when (and (not (bobp)) (bolp)) (backward-char) t))
	(while (progn
		 (beginning-of-line)
		 (tsv-not-looking-at-record))
	  (backward-char))
	(end-of-line))
      (if (memq (preceding-char) tsv-separator-chars) (backward-char))
      (tsv-beginning-of-field))))

(defun tsv-sort-skip-fields (n &optional yank)
  "Position point at the beginning of field N on the current line.
Fields are separated by `tsv-separators'\; null terminal field allowed.
Assumes point is initially at the beginning of the line.
YANK non-nil allows N to be greater than the number of fields, in
which case extend the record as necessary."
  (if (> n 0)
      ;; Skip across N - 1 fields.
      (let ((i (1- n)))
	(while (> i 0)
	  (tsv-end-of-field)
	  (if (eolp)
	      (if yank
		  (if (> i 1) (insert (car tsv-separators)))
		(error "Line has too few fields: %s"
		       (buffer-substring
			(save-excursion (beginning-of-line) (point))
			(save-excursion (end-of-line) (point)))))
	    (forward-char))		; skip separator
	  (setq i (1- i))))
    (end-of-line)
    ;; Skip back across -N - 1 fields.
    (let ((i (1- (- n))))
      (while (> i 0)
	(tsv-beginning-of-field)
	(if (bolp)
	    (error "Line has too few fields: %s"
		   (buffer-substring
		    (save-excursion (beginning-of-line) (point))
		    (save-excursion (end-of-line) (point)))))
	(backward-char)			; skip separator
	(setq i (1- i)))
      ;; Position at the front of the field
      ;; even if moving backwards.
      (tsv-beginning-of-field))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Field index mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Based partly on paren.el

(defcustom tsv-field-index-delay 0.125
  "Time in seconds to delay before updating field index display."
  :type '(number :tag "seconds"))

(defvar tsv-field-index-idle-timer nil)

(defvar tsv-field-index-string nil)
(make-variable-buffer-local 'tsv-field-index-string)

(defvar tsv-field-index-old nil)
(make-variable-buffer-local 'tsv-field-index-old)

(define-minor-mode tsv-field-index-mode
  "Toggle TSV-Field-Index mode.
With prefix ARG, turn TSV-Field-Index mode on if and only if ARG is positive.
Returns the new status of TSV-Field-Index mode (non-nil means on).
When TSV-Field-Index mode is enabled, the current field index appears in
the mode line after `tsv-field-index-delay' seconds of Emacs idle time."
  :global t
  :init-value t		       ; for documentation, since default is t
  ;; This macro generates a function that first sets the mode
  ;; variable, then runs the following code, runs the mode hooks,
  ;; displays a message if interactive, updates the mode line and
  ;; finally returns the variable value.

  ;; First, always disable the mechanism (to avoid having two timers):
  (when tsv-field-index-idle-timer
    (cancel-timer tsv-field-index-idle-timer)
    (setq tsv-field-index-idle-timer nil))
  ;; Now, if the mode is on and any buffer is in TSV mode then
  ;; re-initialize and enable the mechanism by setting up a new timer:
  (if tsv-field-index-mode
      (if (memq t (mapcar (lambda (buffer)
			    (with-current-buffer buffer
			      (when (derived-mode-p 'tsv-mode)
				(setq tsv-field-index-string nil
				      tsv-field-index-old nil)
				t)))
			  (buffer-list)))
	  (setq tsv-field-index-idle-timer
		(run-with-idle-timer tsv-field-index-delay t
				     'tsv-field-index)))
    ;; but if the mode is off then remove the display from the mode
    ;; lines of all TSV buffers:
    (mapc (lambda (buffer)
	    (with-current-buffer buffer
	      (when (derived-mode-p 'tsv-mode)
		(setq tsv-field-index-string nil
		      tsv-field-index-old nil)
		(force-mode-line-update))))
	    (buffer-list))))

(defun tsv-field-index ()
  "Construct `tsv-field-index-string' to display in mode line.
Called by `tsv-field-index-idle-timer'."
  (if (derived-mode-p 'tsv-mode)
      (save-excursion
	(let ((lbp (line-beginning-position)) (field 1))
	  (while (re-search-backward tsv-separator-regexp lbp 1)
	    ;; Move as far as possible, i.e. to beginning of line.
	    (setq field (1+ field)))
	  (if (tsv-not-looking-at-record) (setq field nil))
	  (when (not (eq field tsv-field-index-old))
	    (setq tsv-field-index-old field
		  tsv-field-index-string
		  (and field (propertize (format "F%d" field)
					 'help-echo tsv-mode-line-help-echo)))
	    (force-mode-line-update))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Killing and yanking fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tsv-killed-fields nil
  "A list of the fields or sub-records last killed by `tsv-kill-fields'.")

(defun tsv-kill-fields (fields beg end)
  "Kill specified fields of each line in the region.
If not set, the region defaults to the TSV records around point.
Fields are separated by `tsv-separators' and null fields are allowed anywhere.
Field indices increase from 1 on the left or decrease from -1 on the right.
The fields are stored for use by `tsv-yank-fields'.  Fields can be
specified in any order but are saved in increasing index order.
Ignore blank and comment lines.

When called interactively, a prefix argument specifies a single field,
otherwise prompt for a field list, which may include ranges in the form
m-n, where m < n and n defaults to the last field index if omitted.

When called non-interactively, FIELDS is a single field index or a
list of field indices, with ranges specified as (m.n) or (m), and BEG
and END specify the region to process."
  ;; (interactive "*P\nr")
  (interactive (tsv-interactive-args 'multiple))
  (barf-if-buffer-read-only)
  ;; Kill the field(s):
  (setq tsv-killed-fields nil)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (if (or (cdr fields) (consp (car fields)))
	  (tsv-kill-many-columns fields)
	(tsv-kill-one-column (car fields)))))
  (setq tsv-killed-fields (nreverse tsv-killed-fields)))

(defun tsv-kill-one-field (field)
  "Kill field with index FIELD in current line.
Return killed text.  Assumes point is at beginning of line."
  ;; Move to start of field to kill:
  (tsv-sort-skip-fields field)
  ;; Kill to end of field (cf. `kill-region'):
  (prog1 (delete-and-extract-region
          (point)
          (progn (tsv-end-of-field) (point)))
    (if (eolp)
        (unless (bolp) (delete-char -1)) ; Delete trailing separator at eol
      (delete-char 1))))                 ; or following separator otherwise.

(defun tsv-kill-one-column (field)
  "Kill field with index FIELD in all lines in (narrowed) buffer.
Save killed fields in `tsv-killed-fields'.
Assumes point is at `point-min'.  Called by `tsv-kill-fields'.
Ignore blank and comment lines."
  (while (not (eobp))
    (or (tsv-not-looking-at-record)
	(push (tsv-kill-one-field field) tsv-killed-fields))
    (forward-line)))

(defun tsv-kill-many-columns (fields)
  "Kill several fields in all lines in (narrowed) buffer.
FIELDS is an unordered list of field indices.
Save killed fields in increasing index order in `tsv-killed-fields'.
Assumes point is at `point-min'.  Called by `tsv-kill-fields'.
Ignore blank and comment lines."
  (if (eolp) (error "First record is empty"))
  ;; Convert non-positive to positive field numbers:
  (let ((last 1) (f fields))
    (tsv-end-of-field)
    (while (not (eolp))
      (forward-char)			; skip separator
      (tsv-end-of-field)
      (setq last (1+ last)))	     ; last = # fields in first record
    (while f
      (cond ((consp (car f))
	     ;; Expand a field range: (m.n) -> m m+1 ... n-1 n.
	     ;; If n is nil then it defaults to the number of fields.
	     (let* ((range (car f)) (cdrf (cdr f))
		    (m (car range)) (n (cdr range)))
	       (if (< m 0) (setq m (+ m last 1)))
	       (if n
		   (if (< n 0) (setq n (+ n last 1)))
		 (setq n last))
	       (setq range (list n))
	       (while (> n m) (push (setq n (1- n)) range))
	       (setcar f (car range))
	       (setcdr f (cdr range))
	       (setcdr (setq f (last range)) cdrf)))
	    ((zerop (car f)) (setcar f 1))
	    ((< (car f) 0) (setcar f (+ f last 1))))
      (setq f (cdr f))))
  (goto-char (point-min))
  ;; Kill from right to avoid miscounting:
  (setq fields (sort fields '>))
  (while (not (eobp))
    (or (tsv-not-looking-at-record)
	(let ((fields fields) killed-fields field)
	  (while fields
	    (setq field (car fields)
		  fields (cdr fields))
	    (beginning-of-line)
	    (push (tsv-kill-one-field field) killed-fields))
	  (push (mapconcat 'identity killed-fields (car tsv-separators))
		tsv-killed-fields)))
    (forward-line)))

(defun tsv-yank-fields (field beg end)
  "Yank fields as the ARGth field of each line in the region.
ARG may be arbitrarily large and records are extended as necessary.
If not set, the region defaults to the TSV records around point\;
if point is not in a TSV record then offer to yank as a new table.
The fields yanked are those last killed by `tsv-kill-fields'.
Fields are separated by `tsv-separators' and null fields are allowed anywhere.
Field indices increase from 1 on the left or decrease from -1 on the right.
A prefix argument specifies a single field, otherwise prompt for field index.
Ignore blank and comment lines.  When called non-interactively, FIELD
is a single field index\; BEG and END specify the region to process."
  ;; (interactive "*P\nr")
  (interactive (condition-case err
		   (tsv-interactive-args 'single)
		 (error (list nil nil err))))
  (barf-if-buffer-read-only)
  (if (null beg)
      (if (y-or-n-p (concat (error-message-string end)
			    ".  Yank as a new table? "))
	  (tsv-yank-as-new-table)
	(error (error-message-string end)))
    (if (<= field 0) (setq field (1+ field)))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(let ((fields tsv-killed-fields))
	  (while (not (eobp))
	    (unless (tsv-not-looking-at-record)
	      ;; Yank at start of specified field if possible,
	      ;; otherwise yank at end of record:
	      (if (zerop field)
		  (end-of-line)
		(tsv-sort-skip-fields field 'yank))
	      (and (eolp) (insert (car tsv-separators)))
	      (when fields
		(insert (car fields))
		(setq fields (cdr fields)))
	      (or (eolp) (insert (car tsv-separators))))
	    (forward-line)))))))

(defun tsv-yank-as-new-table ()
  "Yank fields as a new table starting at point.
The fields yanked are those last killed by `tsv-kill-fields'."
  (interactive "*")
  (let ((fields tsv-killed-fields))
    (while fields
      (insert (car fields) ?\n)
      (setq fields (cdr fields)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Aligning fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tsv--column-widths ()
  (let ((widths '()))
    ;; Construct list of column widths:
    (while (not (eobp))                   ; for each record...
      (or (tsv-not-looking-at-record)
          (let ((w widths)
                (beg (point))            ; Beginning of current field.
                x)
            (while (not (eolp))
              (tsv-end-of-field)
              (setq x (- (point) beg))    ; Field width.
              (if w
                  (if (> x (car w)) (setcar w x))
                (setq w (list x)
                      widths (nconc widths w)))
              (or (eolp) (forward-char))  ; Skip separator.
              (setq w (cdr w)
                    beg (point)))))
      (forward-line))
    widths))

(defun tsv-align-fields (hard beg end)
  "Align all the fields in the region to form columns.
The alignment style is specified by `tsv-align-style'.  The number of
spaces specified by `tsv-align-fields' appears after each separator.
Use soft alignment done by displaying virtual white space after the
separators unless invoked with an argument, in which case insert real
space characters into the buffer after the separators.
Unalign first (see `tsv-unalign-fields').  Ignore blank and comment lines.

In hard-aligned records, separators become invisible whenever
`buffer-invisibility-spec' is non-nil.  In soft-aligned records, make
separators invisible if and only if `buffer-invisibility-spec' is
non-nil when the records are aligned\; this can be changed only by
re-aligning.  \(Unaligning always makes separators visible.)

When called non-interactively, use hard alignment if HARD is non-nil\;
BEG and END specify the region to align.
If there is no selected region, default to the whole buffer."
  (interactive (cons current-prefix-arg
                     (if (use-region-p)
                         (list (region-beginning) (region-end))
                       (list (point-min) (point-max)))))
  (setq end (copy-marker end))
  (tsv-unalign-fields hard beg end) ; If hard then barfs if buffer read only.
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (set-marker end nil)
      (goto-char (point-min))
      (let ((widths (tsv--column-widths)))

	;; Align fields:
	(goto-char (point-min))
	(while (not (eobp))		; for each record...
	  (unless (tsv-not-looking-at-record)
            (let ((w widths)
                  (column 0))    ;Desired position of left-side of this column.
              (while (and w (not (eolp)))
                (let* ((beg (point))
                       (align-padding (if (bolp) 0 tsv-align-padding))
                       (left-padding 0) (right-padding 0)
                       (field-width
                        ;; FIXME: Don't assume length=string-width!
                        (progn (tsv-end-of-field) (- (point) beg)))
                       (column-width (pop w))
                       (x (- column-width field-width))) ; Required padding.
                  (set-marker end (point)) ; End of current field.
                  ;; beg = beginning of current field
                  ;; end = (point) = end of current field

                  ;; Compute required padding:
                  (cond
                   ((eq tsv-align-style 'left)
                    ;; Left align -- pad on the right:
                    (setq left-padding align-padding
                          right-padding x))
                   ((eq tsv-align-style 'right)
                    ;; Right align -- pad on the left:
                    (setq left-padding (+ align-padding x)))
                   ((eq tsv-align-style 'auto)
                    ;; Auto align -- left align text, right align numbers:
                    (if (string-match "\\`[-+.[:digit:]]+\\'"
                                      (buffer-substring beg (point)))
                        ;; Right align -- pad on the left:
                        (setq left-padding (+ align-padding x))
                      ;; Left align -- pad on the right:
                      (setq left-padding align-padding
                            right-padding x)))
                   ((eq tsv-align-style 'centre)
                    ;; Centre -- pad on both left and right:
                    (let ((y (/ x 2)))  ; truncated integer quotient
                      (setq left-padding (+ align-padding y)
                            right-padding (- x y)))))

                  (cond
                   (hard ;; Hard alignment...
                    (when (> left-padding 0) ; Pad on the left.
                      ;; Insert spaces before field:
                      (if (= beg end)   ; null field
                          (insert (make-string left-padding ?\ ))
                        (goto-char beg) ; beginning of current field
                        (insert (make-string left-padding ?\ ))
                        (goto-char end))) ; end of current field
                    (unless (eolp)
                      (if (> right-padding 0) ; pad on the right
                          ;; Insert spaces after field:
                          (insert (make-string right-padding ?\ )))
                      ;; Make separator (potentially) invisible;
                      ;; in Emacs 21.3, neighbouring overlays
                      ;; conflict, so use the following only
                      ;; with hard alignment:
                      (let ((ol (make-overlay (point) (1+ (point)) nil t)))
                        (overlay-put ol 'invisible t)
                        (overlay-put ol 'evaporate t))
                      (forward-char)))  ; skip separator

                   ;; Soft alignment...
                   (buffer-invisibility-spec ; tsv-invisibility-default

                    ;; Hide separators...
                    ;; Merge right-padding from previous field
                    ;; with left-padding from this field:
                    (if (zerop column)
                        (when (> left-padding 0)
                          ;; Display spaces before first field
                          ;; by overlaying first character:
                          (overlay-put
                           (make-overlay beg (1+ beg))
                           'before-string
                           (make-string left-padding ?\ )))
                      ;; Display separator as spaces:
                      (with-silent-modifications
                        (put-text-property
                         (1- beg) beg
                         'display `(space :align-to
                                          ,(+ left-padding column)))))
                    (unless (eolp) (forward-char)) ; Skip separator.
                    (setq column (+ column column-width align-padding)))

                   (t ;; Do not hide separators...
                    (let ((overlay (make-overlay beg (point) nil nil t)))
                      (when (> left-padding 0) ; Pad on the left.
                        ;; Display spaces before field:
                        (overlay-put overlay 'before-string
                                     (make-string left-padding ?\ )))
                      (unless (eolp)
                        (if (> right-padding 0) ; Pad on the right.
                            ;; Display spaces after field:
                            (overlay-put
                             overlay
                             'after-string (make-string right-padding ?\ )))
                        (forward-char)))) ; Skip separator.

                   )))))
	  (forward-line)))))
  (set-marker end nil))

(defun tsv-unalign-fields (hard beg end)
  "Undo soft alignment and optionally remove redundant white space.
Undo soft alignment introduced by `tsv-align-fields'.  If invoked with
an argument then also remove all spaces and tabs around separators.
Also make all invisible separators visible again.
Ignore blank and comment lines.  When called non-interactively, remove
spaces and tabs if HARD non-nil\; BEG and END specify region to unalign.
If there is no selected region, default to the whole buffer."
  (interactive (cons current-prefix-arg
                     (if (use-region-p)
                         (list (region-beginning) (region-end))
                       (list (point-min) (point-max)))))
  ;; Remove any soft alignment:
  (mapc 'delete-overlay	(overlays-in beg end))
  (with-silent-modifications
    (remove-list-of-text-properties beg end '(display)))
  (when hard
    (barf-if-buffer-read-only)
    ;; Remove any white-space padding around separators:
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (not (eobp))
	  (or (tsv-not-looking-at-record)
	      (while (not (eolp))
		;; Delete horizontal white space forward:
		;; (delete-horizontal-space)
		;; This relies on left-to-right argument evaluation;
		;; see info node (elisp) Function Forms.
		(delete-region (point)
			       (+ (point) (skip-chars-forward " \t")))
		(tsv-end-of-field)
		;; Delete horizontal white space backward:
		;; (delete-horizontal-space t)
		(delete-region (point)
			       (+ (point) (skip-chars-backward " \t")))
		(or (eolp) (forward-char))))
	  (forward-line))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Transposing rows and columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tsv-transpose (beg end)
  "Rewrite rows (which may have different lengths) as columns.
Null fields are introduced as necessary within records but are
stripped from the ends of records.  Preserve soft alignment.
This function is its own inverse.  Ignore blank and comment lines.
When called non-interactively, BEG and END specify region to process."
  ;; (interactive "*P\nr")
  (interactive (tsv-interactive-args 'noarg))
  (barf-if-buffer-read-only)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; Delete rows and collect them as a reversed list of lists of
      ;; fields, skipping comment and blank lines:
      (let ((sep (car tsv-separators))
	    (align (overlays-in beg end))
	    rows columns)
	;; Remove soft alignment if necessary:
	(when align
	  (mapc 'delete-overlay	align)
	  (setq align t))
	(while (not (eobp))
	  (if (tsv-not-looking-at-record)
	      ;; Skip blank and comment lines:
	      (forward-line)
	    (let ((lep (line-end-position)))
	      (push
	       (tsv-split-string
		(buffer-substring-no-properties (point) lep)
		tsv-separator-regexp nil t)
	       rows)
	      (delete-region (point) lep)
	      (or (eobp) (delete-char 1)))))
	;; Rows must have monotonic decreasing lengths to be
	;; transposable, so ensure this by padding with null fields.
	;; rows is currently a reversed list of field lists, which
	;; must therefore have monotonic increasing lengths.
	(let ((oldlen (length (car rows))) newlen
	      (r (cdr rows)))
	  (while r
	    (setq newlen (length (car r)))
	    (if (< newlen oldlen)
		(nconc (car r) (make-list (- oldlen newlen) nil))
	      (setq oldlen newlen))
	    (setq r (cdr r))))
	;; Collect columns as a reversed list of lists of fields:
	(while rows
	  (let (column (r rows) row)
	    (while r
	      (setq row (car r))
	      ;; Provided it would not be a trailing null field, push
	      ;; field onto column:
	      (if (or column (string< "" (car row)))
		  (push (car row) column))
	      ;; Pop field off row:
	      (setcar r (cdr row))
	      ;; If row is now empty then remove it:
	      (or (car r) (setq rows (cdr rows)))
	      (setq r (cdr r)))
	    (push column columns)))
	;; Insert columns into buffer as rows:
	(setq columns (nreverse columns))
	(while columns
	  (insert (mapconcat 'identity (car columns) sep) ?\n)
	  (setq columns (cdr columns)))
	;; Re-do soft alignment if necessary:
	(if align (tsv-align-fields nil (point-min) (point-max)))))))

;; The following generalised version of `split-string' is taken from
;; the development version of WoMan and should probably replace the
;; standard version in subr.el.  However, TSV mode (currently) needs
;; only the `allowbeg' option.

(defun tsv-split-string
  (string &optional separators subexp allowbeg allowend)
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
If SEPARATORS is absent, it defaults to \"[ \\f\\t\\n\\r\\v]+\".
SUBEXP specifies a subexpression of SEPARATORS to be the splitting
point\; it defaults to 0.

If there is a match for SEPARATORS at the beginning of STRING, we do
not include a null substring for that, unless ALLOWBEG is non-nil.
Likewise, if there is a match at the end of STRING, we do not include
a null substring for that, unless ALLOWEND is non-nil.

Modifies the match data; use `save-match-data' if necessary."
  (or subexp (setq subexp 0))
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning subexp))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning subexp) (length string)))
      (setq notfirst t)
      (or (and (not allowbeg) (eq (match-beginning subexp) 0))
	  (and (eq (match-beginning subexp) (match-end subexp))
	       (eq (match-beginning subexp) start))
	  (push (substring string start (match-beginning subexp)) list))
      (setq start (match-end subexp)))
    (or (and (not allowend) (eq start (length string)))
	(push (substring string start) list))
    (nreverse list)))

;;;; ChangeLog:

;; 2013-04-24  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* tsv-mode.el (tsv-kill-one-field): Check for presence before deleting trailing
;; 	separator.  Remove last arg and turn into a function.
;; 	(tsv-kill-one-column, tsv-kill-many-columns): Adjust callers.
;; 
;; 2012-10-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/tsv-mode/tsv-mode.el (tsv-end-of-field): Don't skip TABs.
;; 	(tsv--skip-regexp): Rename from tsv-skip-regexp.
;; 
;; 2012-10-10  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* tsv-mode.el: Bump version number.
;; 
;; 2012-10-10  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* tsv-mode.el: Use lexical-binding.  Remove redundant :group args.
;; 	(tsv-separators): Add TAB to the default.
;; 	(tsv-invisibility-default): Change default to t.
;; 	(tsv-separator-face): Inherit from escape-glyph.  Remove variable.
;; 	(tsv-mode-line-format): Remove trailing "--".  Move next to line-number.
;; 	(tsv-interactive-args): Use use-region-p.
;; 	(tsv--column-widths): New function, extracted from tsv-align-fields.
;; 	(tsv-align-fields): Use it.  Use whole buffer by default.
;; 	Use :align-to and text-properties when possible.
;; 	(tsv-unalign-fields): Also remove properties.
;; 	(tsv-mode): Truncate lines.
;; 
;; 2012-03-24  Chong Yidong  <cyd@gnu.org>
;; 
;; 	Commentary fix for quarter-plane.el.
;; 
;; 2012-03-24  Chong Yidong  <cyd@gnu.org>
;; 
;; 	Commentary tweaks for tsv-mode, ioccur, and nhexl-mode packages.
;; 
;; 2012-03-24  Chong Yidong  <cyd@gnu.org>
;; 
;; 	tsv-mode.el: Improve commentary.
;; 
;; 2012-03-12  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/tsv-mode/tsv-mode.el: Minor installation cleanups.
;; 	Fix up copyright notice.  Set version.
;; 	(tsv-separators, tsv-field-quotes): Fix calls to `error'.
;; 	(tsv-mode-line-help-echo, tsv-mode-line-format): Replace mode-line-format
;; 	for default-mode-line-format.
;; 	(tsv-mode-map): Declare and initialize.
;; 	(tsv-mode): Add autoload cookie.
;; 	(tsv-set-comment-start): Make sure vars are made buffer-local.
;; 	(tsv-field-index-mode, tsv-field-index): Use derived-mode-p.
;; 	(tsv-align-fields): Improve insertion types of overlay's markers.
;; 
;; 2012-03-12  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	Add tsv-mode.el.
;; 


(provide 'tsv-mode)

;;; tsv-mode.el ends here
