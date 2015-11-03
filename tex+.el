;;; tex+.el -- my enhancements to AUCTeX

;; Copyright (C) 2015 Marcin 'mbork' Borkowski

;; Author: Marcin Borkowski <mbork@mbork.pl>
;; Keywords: tex, convenience

;; This file is NOT part of GNU Emacs.

;; tex+.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.

;; tex+.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with tex+.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; tex+.el is a set of functions enhancing the experience of editing
;; (La)TeX files using AUCTeX.  The areas covered are currently
;; tokenization of the TeX file and complex math delimiters.

;;; Code:

(require 'cl-lib)
(require 'tex)
(require 'texmathp)

(defconst TeX+-normal-letter "a-zA-Z"
  "Character class matching TeX's notion of \"letters\" (in a normal
file).")

(defconst TeX+-internal-letter (concat TeX+-normal-letter "@")
  "Character class matching TeX's notion of \"letters\" within
a package, a class or after \\makeatletter.")

(defconst TeX+-expl3-letter (concat TeX+-normal-letter ":_")
  "Character class matching TeX's notion of \"letters\" within
a LaTeX3 package or after \\ExplSyntaxOn.")

(defvar TeX+-letter TeX+-normal-letter
  "Character class matching TeX's notion of \"letters\" (in the
current context).")
(make-variable-buffer-local 'TeX+-letter)

(defvar TeX+-letter-regex (concat "[" TeX+-letter "]")
  "A regex matching a TeX letter.")
(make-variable-buffer-local 'TeX+-letter-regex)

(defun TeX+-make-at-letter ()
  "Change `TeX+-letter' to `TeX+-internal-letter'."
  (interactive)
  (setq TeX+-letter TeX+-internal-letter)
  (TeX+-update-regexen))

(defun TeX+-make-at-other ()
  "Change `TeX+-letter' to `TeX+-normal-letter'."
  (interactive)
  (setq TeX+-letter TeX+-normal-letter)
  (TeX+-update-regexen))

(defun TeX+-expl-syntax-on ()
  "Change `TeX+-letter' to `TeX+-expl3-letter'."
  (interactive)
  (setq TeX+-letter TeX+-expl3-letter)
  (TeX+-update-regexen))

(defalias 'TeX+-expl-syntax-off 'TeX+-make-at-other)

(defun TeX+-update-regexen ()
  "Update the regexen according to the current definition of
a letter."
  (setq TeX+-letter-regex (concat "[" TeX+-letter "]")))

(defconst TeX+-esc-char (string-to-char TeX-esc)
  "The backslash as a character.")

(defun TeX+-looking-at-esc ()
  "Return t if the point is on a backslash."
  (eq (char-after) TeX+-esc-char))

(defun TeX+-looking-at-unescaped-esc ()
  "Return t if the point is on an unescaped backslash."
  (and (TeX+-looking-at-esc)
       (not (TeX-escaped-p))))

(defun TeX+-looking-back-at-unescaped-esc ()
  "Return t if the character before point is an unescaped
backslash."
  (and (eq (char-before) TeX+-esc-char)
       (not (TeX-escaped-p (1- (point))))))

(defun TeX+-skip-blanks-backward ()
  "Skip whitespace characters (spaces, tabs and newlines)
backward.  Return the number of newline characters skipped this
way."
  (let ((newlines 0))
    (while (progn
	     (skip-chars-backward " \t")
	     (eq (char-before) ?\n))
      (backward-char)
      (cl-incf newlines))
    newlines))

; Moving to the beginning of the token at point is the fundamental,
; non-trivial operation.  There are quite a few categories of possible
; tokens: a control symbol, a control word (possibly followed by
; whitespace), implicit \par (more than one \newline, possibly
; interspersed by other whitespace), beginning of buffer, whitespace
; (but not after a control word, unless it's an implicit \par), other
; character.  The purpose of this function is to determine which one
; we are at and to move to its beginning.
(defun TeX+-move-beginning-of-token ()
  "Move to the beginning of TeX token the point is at.
Treat whitespace after a control word as belonging to it."
  (interactive)
  (let ((opoint (point)))
    (cond ((looking-at-p TeX+-letter-regex)
	   (skip-chars-backward TeX+-letter)
	   (if (bobp)
	       (goto-char opoint)
	     (backward-char)
	     (if (TeX+-looking-at-unescaped-esc)
		 (point)
	       (goto-char opoint))))
	  ((looking-at-p "[ \t\n]")
	   (cond ((TeX+-looking-back-at-unescaped-esc)
		  (backward-char))
		 ((save-excursion
		    (skip-chars-forward " \t\n")
		    (> (TeX+-skip-blanks-backward) 1))
		  (skip-chars-backward " \t\n")
		  ;; We are now at the beginning of a string of
		  ;; whitespace including more than one newline, i.e.,
		  ;; an implicit \par.  But we might have landed on
		  ;; a control symbol like "\ " etc.; if yes, we go
		  ;; forward by one character.
		  (when (TeX+-looking-back-at-unescaped-esc)
		    (forward-char)))
		 ;; The following two conditions are a nasty hack.  If
		 ;; the first one is satisfied, we must be on
		 ;; a control word.  If not, we have traveled too far,
		 ;; and have to back up.  Note: the first form in
		 ;; `(and...)' is always satisfied, because
		 ;; `skip-chars-backward' returns a number.  This way,
		 ;; we can use `and' instead of a `progn', which is
		 ;; pretty clever, probably marginally faster, and
		 ;; goes again good programming practices.
		 ((and (skip-chars-backward " \t\n")
		       (< (skip-chars-backward TeX+-letter) 0)
		       (TeX+-looking-back-at-unescaped-esc))
		  (backward-char))
		 (t (skip-chars-forward TeX+-letter))))
	  (t (if (TeX+-looking-back-at-unescaped-esc)
		 (backward-char))))))

; This function could probably be optimized for speed.  We'll see,
; however, whether this will be necessary.
(defun TeX+-info-about-token-beginning-at-point ()
  "Return a cons cell with car being the token beginning at point
and cdr being one of the following symbols: 'control-symbol,
'control-word, 'implicit-par, 'whitespace, 'normal-character,
'eob.

For simplicity, if the point is at a backslash, which is the last
character in the buffer, it treats it as a control symbol."
  (cond ((eobp) (cons "" 'eob))
	((eq (char-after) TeX+-esc-char)
	 (save-excursion
	   (forward-char)
	   (if (looking-at-p TeX+-letter-regex)
	       (cons (buffer-substring-no-properties
		      (1- (point))
		      (save-excursion
			(skip-chars-forward TeX+-letter)
			(point)))
		     'control-word)
	     (cons (buffer-substring-no-properties
		    (1- (point))
		    (min (1+ (point)) (point-max)))
		   'control-symbol))))
	((looking-at-p "[ \t\n]")
	 (cons (buffer-substring-no-properties
		(point)
		(save-excursion
		  (skip-chars-forward " \t\n")
		  (point)))
	       (if (looking-at "[ \t]*\\(?:\n[ \t]*\\)\\{2,\\}")
		   'implicit-par
		 'whitespace)))
	(t (cons (buffer-substring-no-properties
		  (point)
		  (1+ (point)))
		 'normal-character))))

; This function assumes that the point is at the beginning of
; a token.  It's behavior is undefined otherwise.
(defun TeX+-move-from-beginning-to-end-of-token ()
  "Move past the current token, assuming the point is at its
beginning.  If at a control word, move past the trailing
whitespace, too."
  (cl-case (cdr (TeX+-info-about-token-beginning-at-point))
    (control-symbol (forward-char 2))
    (control-word (forward-char)
		   (skip-chars-forward TeX+-letter)
		   (skip-chars-forward " \t")
		   (unless (looking-at-p "\\(?:\n[ \t]*\\)\\{2,\\}")
		     (skip-chars-forward " \t\n")))
    ((implicit-par whitespace) (skip-chars-forward " \t\n"))
    (normal-character (forward-char))))

(defun TeX+-move-end-of-token ()
  "Move to the end of the token the point is at."
  (interactive)
  (TeX+-move-beginning-of-token)
  (TeX+-move-from-beginning-to-end-of-token))

(defun TeX+-name-of-token-at-point ()
  "Returns a string with the token at point."
  (save-excursion
    (TeX+-move-beginning-of-token)
    (car (TeX+-info-about-token-beginning-at-point))))

(defun TeX+-name-of-previous-token ()
  "Returns a string with the name of the token before the one
point is at.  This is needed if e.g. we are on a \\lbrace and
want to know whether there is a \\left before it."
  (save-excursion
    (TeX+-move-beginning-of-token)
    (if (bobp)
	""
      (backward-char)
      (TeX+-name-of-token-at-point))))

(defun TeX+-name-of-next-token ()
  "Returns a string with the name of the token after the one
point is at.  This is needed if e.g. we are on a \\left and
want to know whether there is a delimiter after it."
  (save-excursion
    (TeX+-move-end-of-token)
    (if (eobp)
	""
      (TeX+-name-of-token-at-point))))

(defun TeX+-forward-token (&optional count)
  "Move forward COUNT tokens."
  (interactive "^p")
  (let ((count (or count 1)))
    (if (> count 0)
	(progn
	  (TeX+-move-beginning-of-token)
	  (dotimes (unused count)
	    (TeX+-move-end-of-token)))
      (dotimes (unused (- count))
	(backward-char)
	(TeX+-move-beginning-of-token)))))

(defun TeX+-backward-token (&optional count)
  "Move backward until encountering the beginning of a token.
With argument ARG, do this that many times."
  (interactive "^p")
  (TeX+-forward-token (- (or count 1))))

(defun TeX+-mark-token (&optional count allow-extend)
  "Mark COUNT tokens, assuming that point is at the beginning of
a token.  When COUNT is negative, mark backward.

If this command is repeated or the region is active, extend the region
by COUNT tokens.

The second argument is non-nil when called interactively; when called
from a Lisp program, it determines whether the region-extending
behavior should be turned on.

This function is copied almost verbatim from `mark-word'."
  (interactive "P\np")
  (cond ((and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (region-active-p)))
	 (setq count (if count (prefix-numeric-value count)
		     (if (< (mark) (point)) -1 1)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (TeX+-forward-token count)
	    (point))))
	(t
	 (push-mark
	  (save-excursion
	    (TeX+-forward-token (prefix-numeric-value count))
	    (point))
	  nil t))))

(defun TeX+-delete-token (&optional count)
  "Delete COUNT tokens from point on.  Assume that point is at
the beginning of a token."
  (interactive "*p")
  (delete-region
   (point)
   (progn (TeX+-forward-token count)
	  (point))))

(defun TeX+-kill-token (&optional count)
  "Kill COUNT tokens from point on.  Assume that point is at
the beginning of a token."
  (interactive "p")
  (kill-region
   (point)
   (progn (TeX+-forward-token count)
	  (point))))

(defun TeX+-backward-delete-token (&optional count)
  "Delete COUNT tokens to the left of the point."
  (interactive "*p")
  (TeX+-delete-token (- (or count 1))))

(defun TeX+-backward-kill-token (&optional count)
  "Kill COUNT tokens to the left of the point."
  (interactive "p")
  (TeX+-kill-token (- (or count 1))))

(defvar TeX+-left-delimiters
  '("(" "[" "\\lbrack" "\\{" "\\lbrace" "\\langle" "\\lfloor" "\\lceil" "\\lvert" "\\lVert"
    "\\ulcorner" "\\llcorner")
  "List of left delimiters acceptable after \\bigl etc.")

(defvar TeX+-right-delimiters
  '(")" "]" "\\rbrack" "\\}" "\\rbrace" "\\rangle" "\\rfloor" "\\rceil" "\\rvert" "\\rVert"
    "\\urcorner" "\\lrcorner")
  "List of right delimiters acceptable after \\bigr etc.")

(defvar TeX+-ambiguous-delimiters
  '("." "/" "\\backslash" "\\uparrow" "\\downarrow" "\\updownarrow" "|" "\\vert"
    "\\Uparrow" "\\Downarrow" "\\Updownarrow" "\\|" "\\Vert")
  "List of delimiters acceptable after \\bigl/\\bigr etc. with
  otherwise undefined direction.")

; A list of all possible delimiters (left, right and ambiguous), kept
; for efficiency reasons.
(setq TeX+-delimiters (append TeX+-left-delimiters TeX+-right-delimiters TeX+-ambiguous-delimiters))

(defvar TeX+-delim-prefix-pairs
  '(("\\mathopen" . "\\mathclose")
    ("\\bigl" . "\\bigr")
    ("\\Bigl" . "\\Bigr")
    ("\\biggl" . "\\biggr")
    ("\\Biggl" . "\\Biggr")
    ("\\left" . "\\right"))
  "List of pairs of prefixes for TeX delimiters, from smallest to
largest.  The first pair is \\mathopen/\\mathclose, which is
equivalent to null strings for non-ambiguous delimiters.")

(defun TeX+-smaller-prefix (prefix &optional ambiguous)
  "Return a prefix smaller by one than the one found.  If there
is no such prefix, return nil.  Special case: if PREFIX is
\"\\bigl\" or \"\\bigr\", return the empty string unless
AMBIGUOUS is non-nil, in which case return \"\\mathopen\" or
\"\\mathclose\"."
  (let ((side (cond			; which one to take?
	       ((assoc prefix TeX+-delim-prefix-pairs) #'car)
	       ((rassoc prefix TeX+-delim-prefix-pairs) #'cdr))))
    (if side
	(if (and (not ambiguous)
		 (let ((case-fold-search nil)) (string-match "^\\\\big[lr]$" prefix)))
	    ""
	  (let ((prev nil)
		(rest TeX+-delim-prefix-pairs))
	    (while rest
	      (if (string= prefix (funcall side (car rest)))
		  (setq rest nil)	; exit loop
		(setq prev (car rest)
		      rest (cdr rest))))
	    (funcall side prev))))))

(defun TeX+-larger-prefix (prefix)
  "Return a prefix larger by one than the one found.  If there is
no such prefix (this may happen if either PREFIX is invalid or it
is the last one), return nil.  Note: unlike TeX+-smaller-prefix,
no special cases are involved."
  (let ((side (cond
	       ((assoc prefix TeX+-delim-prefix-pairs) #'car)
	       ((rassoc prefix TeX+-delim-prefix-pairs) #'cdr))))
    (if side
	(let ((rest TeX+-delim-prefix-pairs))
	  (while (and rest
		      (not (string= prefix (funcall side (car rest)))))
	    (setq rest (cdr rest)))
	  (funcall side (cadr rest))))))

(defun TeX+-corresponding-delim-prefix (prefix)
  "Find the corresponding delimiter prefix in the
TeX+-delim-prefix-pairs table.  Returns nil if not found."
  (or (cdr (assoc prefix TeX+-delim-prefix-pairs))
      (car (rassoc prefix TeX+-delim-prefix-pairs))))

; Lists of left, right and all delim prefixes (generated once from
; TeX+-delim-prefix-pairs for efficiency reasons)
(setq TeX+-left-delim-prefixes (mapcar #'car TeX+-delim-prefix-pairs))
(setq TeX+-right-delim-prefixes (mapcar #'cdr TeX+-delim-prefix-pairs))
(setq TeX+-delim-prefixes (append TeX+-left-delim-prefixes TeX+-right-delim-prefixes))

(defun TeX+-current-delimiter ()
  "Check whether we are in math mode and at a delimiter.  The
result is non-nil if the point is on a valid delimiter; more
precisely, it is one of the following symbols:
- 'left-prefix if at \\bigl etc.
- 'right-prefix if at \\bigr etc.
- 'left-with-prefix if at \\lbrace or | etc., with a prefix before
- 'right-with-prefix if at \\rbrace etc., with a prefix before
- 'left-without-prefix if at \\lbrace etc., without a prefix before
- 'right-without-prefix if at \\rbrace etc., without a prefix before
A value of nil means that either we are not on any delimiter (or
prefix), or on an invalid one (e.g. a prefix without any delimiter, or
an ambiguous delimiter without a prefix), or outside math mode."
  (when (texmathp)
    (let ((current-token (TeX+-name-of-token-at-point)))
      (cond
       ((member current-token TeX+-left-delim-prefixes)
	(if (member (TeX+-name-of-next-token) TeX+-delimiters)
	    'left-prefix))
       ((member current-token TeX+-right-delim-prefixes)
	(if (member (TeX+-name-of-next-token) TeX+-delimiters)
	    'right-prefix))
       ((member current-token TeX+-delimiters)
	(let ((prev (TeX+-name-of-previous-token)))
	  (cond
	   ((member prev TeX+-left-delim-prefixes) 'left-with-prefix)
	   ((member prev TeX+-right-delim-prefixes) 'right-with-prefix)
	   ((member current-token TeX+-left-delimiters)
	    'left-without-prefix)
	   ((member current-token TeX+-right-delimiters)
	    'right-without-prefix))))))))

(defun TeX+-find-matching-delimiter (&optional interactive)
  "If at a delimiter, goto the (beginning of) the matching
one (or its prefix, if present) and return t.  In case of an
unmatched delimiter, stop right before the border of math mode.

Currently, if both delimiters have non-matching prefixes, we still
move to the right spot, but throw an error when INTERACTIVE is non-nil
\(which it is in case of an interactive call) or return nil
otherwise."
  (interactive "p")
  (let ((current-delim (TeX+-current-delimiter)))
    (when current-delim
      (if (memq current-delim '(left-prefix right-prefix))
	  (TeX+-forward-token)
	(TeX+-move-beginning-of-token))
      ;; Now we are at the current delimiter proper (not the prefix).
      (let* ((direction (if (memq current-delim
				  '(left-prefix left-with-prefix left-without-prefix))
			    'right
			  'left))
	     (prefix (if (memq current-delim
			       '(left-prefix right-prefix left-with-prefix right-with-prefix))
			 (TeX+-name-of-previous-token)
		       ""))
	     (goto-next			; function to move by one
					; token in the appropriate
					; direction
	      (if (eq direction 'right) #'TeX+-forward-token #'TeX+-backward-token))
	     (inc-tokens     ; tokens which increase the delim counter
	      (if (eq direction 'right)
		  '(left-with-prefix left-without-prefix)
		'(right-with-prefix right-without-prefix)))
	     (dec-tokens     ; tokens which decrease the delim counter
	      (if (eq direction 'right)
		  '(right-with-prefix right-without-prefix)
		'(left-with-prefix left-without-prefix)))
	     (delim-counter 1)
	     (error nil))
	(while (and (> delim-counter 0)
		    (texmathp))		; if we get outside math mode,
					; we'd better stop!
	  (funcall goto-next)
	  (setq current-delim (TeX+-current-delimiter)) ; we reuse
					; this no longer needed
					; variable for optimization
	  (if (memq current-delim inc-tokens)
	      (incf delim-counter))
	  (if (memq current-delim dec-tokens)
	      (decf delim-counter)))
	(if (memq (TeX+-current-delimiter)
		  '(left-with-prefix right-with-prefix))
	    (if (string= (TeX+-name-of-previous-token) (TeX+-corresponding-delim-prefix prefix))
		(TeX+-backward-token)
	      (when interactive
		(error "Prefix mismatch!")
		(setq error t)))
	  (unless (string= prefix "")
	    (when interactive
	      (error "Prefix mismatch!")
	      (setq error t))))
	(unless (texmathp)
	  (funcall goto-next -1)	; get back into math mode
	  (setq error t))
	(not error)))))

(defun TeX+-change-token-at-point (new-token)
  "Delete the token at point and insert NEW-TOKEN in its place.
Do nothing but beep if NEW-TOKEN is not a string (this is needed
in e.g. TeX+-enlarge-delimiters, when NEW-TOKEN is nil when we
want to enlarge \"\\left\" or \"\\right\")."
  (if (stringp new-token)		; This is WTFery; this test
					; should probably be done
					; inside
					; TeX+-enlarge-delimiters...
      (progn (TeX+-move-beginning-of-token)
	     (delete-region (point)
			    (progn
			      (TeX+-move-from-beginning-to-end-of-token)
			      (point)))
	     (save-excursion (insert new-token)))
    (beep)))

(defun TeX+-enlarge-delimiters ()
  "Enlarges the delimiter at point by one size, together with its
matching one.  If point is not on a delimiter, throws an error."
  (interactive)
  (let ((current (TeX+-current-delimiter)))
    (if (not current)
	(error "Not at a delimiter")
      ;; If we are at a prefix, check whether a delimiter follows, and
      ;; if yes, make the prefix one size larger.
      (TeX+-move-beginning-of-token)
      (if (memq current '(left-without-prefix right-without-prefix))
	  (let (this that)		; define THIS and THAT to what
					; should be put here and there.
	    (if (memq current '(left-without-prefix))
		(setq this (caadr TeX+-delim-prefix-pairs)
		      that (cdadr TeX+-delim-prefix-pairs))
	      (setq this (cdadr TeX+-delim-prefix-pairs)
		    that (caadr TeX+-delim-prefix-pairs)))
	    (save-excursion
	      (TeX+-find-matching-delimiter)
	      (insert that))
	    (save-excursion (insert this)))
	(if (memq current '(left-with-prefix right-with-prefix)) ; we
					; are on a prefixed delimiter,
					; so let's back up
	    (TeX+-backward-token))
	(save-excursion			; now we must be on a prefix
	  (TeX+-find-matching-delimiter)
	  (TeX+-change-token-at-point (TeX+-larger-prefix (TeX+-name-of-token-at-point))))
	(TeX+-change-token-at-point (TeX+-larger-prefix (TeX+-name-of-token-at-point)))))))

(defun TeX+-diminish-delimiters ()
  "Diminishes the delimiter at point by one size, together with its
counterpart.  If point is not at a delimiter, throws an error.

This works wrong in one case: when diminishing \\bigl ... \\bigr, and
the delimiter is not ambiguous (like \"|\"), it just does not produce
any prefix.  This is wrong in cases like \"\\bigr(\".  Also, dot (with
\\left or \\right) should block diminishing."
  (interactive)
  (let ((current (TeX+-current-delimiter)))
    (if (not current)
	(error "Not at a delimiter")
      (if (memq current '(left-without-prefix right-without-prefix))
	  (beep)			; nothing to diminish!
	(if (memq current '(left-with-prefix right-with-prefix))
	    (TeX+-backward-token))
	(let ((ambiguous (member (TeX+-name-of-next-token)
				 TeX+-ambiguous-delimiters)))
	  (save-excursion
	    (TeX+-find-matching-delimiter)
	    (TeX+-change-token-at-point (TeX+-smaller-prefix
					 (TeX+-name-of-token-at-point) ambiguous)))
	  (TeX+-change-token-at-point (TeX+-smaller-prefix (TeX+-name-of-token-at-point) ambiguous)))))))

(defun TeX+-introduce-delimiters (count)
  "Experimental: introduce a \\mathopen .. \\mathclose pair at point
and at a supposed closing delimiter, i.e. the identical delimiter
token occurrence, COUNT occurrences from here.

For instance, in this situation:
  -!-|a+b|
we get
  -!-\\mathopen|a+b\mathclose|
COUNT may be also negative.

Note: this finds the matching delimiter with a simple call to
`search-forward'.  This means that this function may be tricked
e.g. by escaped backslashes, interspersed comments etc.

TODO: this funtion might benefit from implementing TeX's rules whether
to treat an ordinary symbol as a delimiter (see the TeXbook, chapters
on math)."
  (interactive "p")
  (save-excursion
    (let* ((forward (> count 0))
	   (here (if forward "\\mathopen" "\\mathclose"))
	   (there (if forward "\\mathclose" "\\mathopen")))
      (insert here)
      (search-forward (TeX+-name-of-token-at-point) nil t (if forward
							      (1+ count)
							    count))
      (goto-char (match-beginning 0))
      (insert there))))

(defun TeX+-current-delim-pos-info ()
  "Get positional information about the delimiter at point and return
it as a cons of (BEGIN . END), or nil if not t a delimiter."
  (let ((current (TeX+-current-delimiter)))
    (save-excursion
      (cond ((memq current '(left-prefix right-prefix))
	     (TeX+-move-beginning-of-token)
	     (cons (point)
		   (progn (TeX+-forward-token 2) (point))))
	    ((memq current '(left-with-prefix right-with-prefix))
	     (TeX+-move-end-of-token)
	     (let ((end (point)))
	       (TeX+-backward-token 2)
	       (cons (point) end)))
	    ((memq current '(left-without-prefix
			     right-without-prefix))
	     (cons (progn (TeX+-move-beginning-of-token)
			  (point))
		   (progn (TeX+-move-end-of-token)
			  (point))))))))

(defun TeX+-show-paren--LaTeX ()
  "A helper function enabling show-paren-mode to highlight LaTeX
\"parens\".  Returns a list of: pos of begin/end of the current
delimiter, then begin/end of the other one, then a Boolean
indicating whether there is a mismatch.  If the point is not on
a delimiter, resorts to default show-paren--default function."
  (let ((current (TeX+-current-delimiter)))
    (if (not current)
	(show-paren--default)
      (save-excursion
	(let* ((here (TeX+-current-delim-pos-info))
	       (mismatch (not (TeX+-find-matching-delimiter)))
	       (there (unless mismatch
			(TeX+-current-delim-pos-info))))
	  (list (car here) (cdr here) (car there) (cdr there) mismatch))))))

(define-minor-mode TeX+-show-paren-mode
  "Toggle a minor mode showing TeX-style matching delimiters."
  :init-value nil
  :lighter " \\bigl"
  (if TeX+-show-paren-mode
      (setq show-paren-data-function #'TeX+-show-paren--LaTeX)
    (setq show-paren-data-function #'show-paren--default)))
(make-variable-buffer-local 'show-paren-data-function)

(defun TeX+-whats-next ()
  "Determine what is the next thing from point.  Assume that we are
at the beginning of a token.

Possible results:
- nothing-special
- environment
- other-token (a control sequence)
- implicit-par
- group
- math-formula
- subexpression (i.e., part of a formula between delimiters)
- eob"
  (let* ((token (TeX+-info-about-token-beginning-at-point))
	 (token-string (car token)))
    (cl-case (cdr token)
      (control-symbol
       (cond
	((member token-string '("\\(" "\\["))
	 'math-formula)
	((and (string= token-string "\\{")
	      (texmathp))
	 'subexpression)
	(t 'other-token)))
      (control-word
       (cond
	((string= token-string "\\begin") 'environment)
	((and (or (member token-string TeX+-left-delim-prefixes)
		  (member token-string TeX+-left-delimiters))
	      (let ((current-delim (TeX+-current-delimiter))
		    (matching-delim (save-excursion
				      (if (TeX+-find-matching-delimiter)
					  (TeX+-current-delimiter)))))
		(if matching-delim
		    (or (and (eq current-delim 'left-prefix)
			     (eq matching-delim 'right-prefix))
			(and (eq current-delim 'left-without-prefix)
			     (eq matching-delim 'right-without-prefix))))))
	 'subexpression)
	(t 'other-token)))
      (implicit-par 'implicit-par)
      (whitespace
       (save-excursion
	 (skip-chars-forward " \t\n")
	 (TeX+-whats-next)))
      (normal-character
       (cond
	((string= token-string "{") 'group)
	((string= token-string "$") 'math-formula)
	((and (member token-string '("(" "["))
	      (texmathp))
	 'subexpression)
	(t 'nothing-special)))
      (eob 'eob))))

(defun TeX+-forward-one-unit ()
  "Move forward by TeX \"word-like unit\"."
  (cl-case (prog1
	       ;; This is a trick which moves the point past
	       ;; whitespace /after/ determining what is ahead (so
	       ;; that implicit par gets recognized), but before any
	       ;; other action is taken (so that we may assume that
	       ;; the point is /not/ on whitespace).  This means that
	       ;; if (TeX+-whats-next) actually returned
	       ;; 'implicit-par, nothing else should be done.
	       (TeX+-whats-next)
	     (skip-chars-forward " \t\n"))
    (nothing-special (forward-word))
    (environment (TeX+-forward-token)
		 (LaTeX-find-matching-end))
    (other-token (TeX+-forward-token))
    (group (forward-sexp))
    (math-formula (TeX+-forward-math-formula))
    (subexpression
     (TeX+-find-matching-delimiter)
     (TeX+-forward-token (if (eq (TeX+-current-delimiter) 'right-prefix) 2 1)))))

(defun TeX+-forward-math-formula ()
  "Move past the formula starting at point.
Assume that the point is on a formula start.  Resort to `forward-sexp'
if the formula starts with a dollar sign, and use a hand-crafted
solution otherwise.

Assume well-formedness of the formula (i.e., if math delimiters
are not properly paired, the result is undefined)."
  (interactive)
  (let ((token (car (TeX+-info-about-token-beginning-at-point))))
    (if (string= token "$")
	(forward-sexp)
      (let ((bal 0))
	(while (progn
		 (when (member token '("\\(" "\\["))
		   (cl-incf bal))
		 (when (member token '("\\)" "\\]"))
		   (cl-decf bal))
		 (TeX+-move-from-beginning-to-end-of-token)
		 (setq token (car (TeX+-info-about-token-beginning-at-point)))
		 (> bal 0)))))))

(defun TeX+-whats-prev ()
  "Determine what is there before the point.
Assume that we are at the beginning of a token.

Possible results:
- bob
- group
- environment
- other-token (a control sequence)
- implicit-par
- math-formula
- subexpression
- nothing-special"
  (if (not (bobp))
      (save-excursion
	(backward-char)
	(TeX+-move-beginning-of-token)
	(let* ((token (TeX+-info-about-token-beginning-at-point))
	       (token-string (car token)))
	  (cl-case (cdr token)
	    ((control-symbol control-word)
	     (cond ((member token-string '("\\)" "\\]"))
		    'math-formula)
		   ((memq (TeX+-current-delimiter) '(right-with-prefix right-without-prefix))
		    'subexpression)
		   (t 'other-token)))
	    (implicit-par 'implicit-par)
	    (whitespace
	     (save-excursion
	       (skip-chars-backward " \t\n")
	       (TeX+-whats-prev)))
	    (normal-character
	     (cond
	      ((and (string= token-string "$")
		    (texmathp))
	       'math-formula)
	      ((memq (TeX+-current-delimiter) '(right-with-prefix right-without-prefix))
	       'subexpression)
	      ((string= token-string "}")
	       (forward-char)
	       (backward-sexp)
	       (skip-chars-backward " \t") ; here we assume that there
					   ; is no newline between
					   ; \end and the environment
					   ; name
	       (backward-char 4)
	       (if (looking-at-p "\\\\end")
		   'environment
		 'group))
	      (t 'nothing-special))))))
    'bob))

(defun TeX+-backward-one-unit (&optional count)
  "Move backward by one TeX \"word-like unit\"."
  (skip-chars-backward " \t")
  (cl-case (TeX+-whats-prev)
    (nothing-special (backward-word))
    (environment
     (backward-sexp)
     (TeX+-backward-token)
     (LaTeX-find-matching-begin))
    (other-token (TeX+-backward-token))
    (group (backward-sexp))
    (math-formula (TeX+-backward-math-formula))
    (subexpression (TeX+-find-matching-delimiter))))

(defun TeX+-backward-math-formula ()
  "Move before the formula the point is after.
Resort to `forward-sexp' if the formula ends with a dollar sign,
and use a hand-crafted solution otherwise.

Assume well-formedness of the formula (i.e., if math delimiters
are not properly paired, the result is undefined)."
  (interactive)
  (if (eq (char-before) ?$)
      (backward-sexp)
    (let (token)
      (let ((bal 0))
	(while (progn
		 (TeX+-forward-token -1)
		 (setq token (car (TeX+-info-about-token-beginning-at-point)))
		 (when (member token '("\\)" "\\]"))
		   (cl-incf bal))
		 (when (member token '("\\(" "\\["))
		   (cl-decf bal))
		 (> bal 0)))))))

(defun TeX+-forward-unit (count)
  "Move forward by COUNT TeX \"word-like units\"."
  (interactive "p")
  (if (> count 0)
      (dotimes (_ count) (TeX+-forward-one-unit))
    (dotimes (_ count) (TeX+-backward-one-unit))))

(defun TeX+-backward-unit (count)
  "Move backward by COUNT TeX \"word-like units\"."
  (interactive "p")
  (TeX+-forward-unit (- count)))

(eval-after-load 'latex '(progn
			   (define-key LaTeX-mode-map (kbd "C-c C-0") 'TeX+-enlarge-delimiters)
			   (define-key LaTeX-mode-map (kbd "C-c C-9") 'TeX+-diminish-delimiters)
			   (define-key LaTeX-mode-map (kbd "C-c (") 'TeX+-find-matching-delimiter)
			   (define-key LaTeX-mode-map (kbd "C-c )") 'TeX+-find-matching-delimiter)
			   (define-key LaTeX-mode-map (kbd "C-c C-t (") 'TeX+-show-paren-mode)))

(provide 'tex+)

;;; tex+.el ends here
