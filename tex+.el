;;;; My Elisp hacks for better AUCTeX experience

(require 'cl)

(defun TeX+-letter (&optional at-is-letter)
  "Returns a character class matching letters (including \"@\" if
  AT-IS-LETTER is true)."
  (concat "a-zA-Z"
	  (if at-is-letter "@")))

(defun TeX+-looking-at-letter (&optional at-is-letter)
  "Returns t if the point is at a letter (including \"@\" if
AT-IS-LETTER; default is not)."
  (looking-at (concat "[" (TeX+-letter at-is-letter) "]")))

(defun TeX+-info-about-token-at-point (&optional at-is-letter)
  "Returns a list with the information of the token at point.
The first and second elements are the positions of the beginning
and end of the token and the third is the type of the token (one
of the symbols: :control-word :control-symbol
:normal-character :end-of-buffer :backslash-at-eob)."
  (if (eobp) ; end of buffer is special
      (list (point) (point) :end-of-buffer)
    (let ((opoint (point)))
      (save-excursion
; There are 3 cases: a control word, a control symbol or anything else.
; If we are on a letter, go back until we get to a nonletter.
	(cond ((TeX+-looking-at-letter at-is-letter)
	       (skip-chars-backward (TeX+-letter at-is-letter))
	       (if (bobp)
		   (list opoint
			 (1+ opoint)
			 :normal-character)
		 (backward-char)
;   If we are on an unescaped backslash, we were on a control word.
		 (if (and
		      (looking-at (regexp-quote TeX-esc))
		      (not (TeX-escaped-p)))
		     (list (point)
			   (progn (forward-char) ; here forward-char is safe - we are not on eob.
				  (skip-chars-forward (TeX+-letter at-is-letter))
				  (point))
			   :control-word)
;   If we are on an escaped backslash, we were on an ordinary character.
		   (list opoint (1+ opoint) :normal-character))))
; If we are on an unescaped backslash, this might be a control word or a control symbol.
	      ((and (looking-at (regexp-quote TeX-esc))
		    (not (TeX-escaped-p)))
	       (forward-char)
	       (cond
;   If this is eob, we have a special case
		((eobp) (list (1- (point)) (point) :backslash-at-eob))
;   If this is a control word, return (opoint (opoint + 1 + (number of letters)) :control-word)
		((TeX+-looking-at-letter)
		 (list opoint
		       (+ opoint 1 (skip-chars-forward (TeX+-letter at-is-letter)))
		       :control-word))
;   Otherwise this is a control symbol, return (opoint (opoint + 2) :control-symbol)
		(t (list opoint (+ 2 opoint) :control-symbol))))
; If we are on a something else, back up one char and check whether there's an unescaped backslash
	      (t
	       (if (bobp)
		   (list opoint (1+ opoint) :normal-character)
		 (backward-char)
		 (if (and (looking-at (regexp-quote TeX-esc))
			  (not (TeX-escaped-p)))
;   If yes, we are at a control symbol
		     (list (1- opoint) (1+ opoint) :control-symbol)
;   If not, we are on a normal character
		   (list opoint (1+ opoint) :normal-character)))))))))

(defun TeX+-name-of-token-at-point ()
  "Returns a string with the token at point."
  (let ((token (TeX+-info-about-token-at-point)))
    (buffer-substring-no-properties (car token) (cadr token))))

(defun TeX+-move-to-token-beginning ()
  "Move point to the beginning of the token at point."
  (interactive)
  (goto-char (car (TeX+-info-about-token-at-point))))

(defun TeX+-name-of-previous-token ()
  "Returns a string with the name of the token before the one
point is at.  This is needed if e.g. we are on a \\lbrace and
want to know whether there is a \\left before it."
  (save-excursion
    (TeX+-move-to-token-beginning)
    (if (bobp)
	""
      (backward-char)
      (TeX+-name-of-token-at-point))))

(defun TeX+-name-of-next-token ()
  "Returns a string with the name of the token after the one
point is at.  This is needed if e.g. we are on a \\left and
want to know whether there is a delimiter after it."
  (save-excursion
    (TeX+-move-to-token-end)
    (if (eobp)
	""
      (forward-char)
      (TeX+-name-of-token-at-point))))

(defun TeX+-move-to-token-end ()
  "Move point to the last character of the token at point."
  (interactive)
  (unless (eobp) ; this is a special case of a "token" of zero length!
    (goto-char (1- (cadr (TeX+-info-about-token-at-point))))))

(defun TeX+-forward-token (&optional count)
  "Move forward COUNT tokens."
  (interactive "^p")
  (let ((count (or count 1)))
    (if (> count 0)
	(dotimes (unused count)
	  (goto-char (cadr (TeX+-info-about-token-at-point))))
      (dotimes (unused (- count))
	(backward-char)
	(let ((token-info (TeX+-info-about-token-at-point)))
	  (unless (= 1 (- (cadr token-info) (car token-info)))
	    (goto-char (car token-info))))))))

(defun TeX+-backward-token (&optional count)
  "Move backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "^p")
  (TeX+-forward-token (- (or count 1))))

(defvar TeX+-left-delimiters
  '("(" "[" "\\lbrace" "\\{" "\\langle" "\\lvert")
  "List of left delimiters acceptable after \\bigl etc.")

(defvar TeX+-right-delimiters
  '(")" "]" "\\rbrace" "\\}" "\\rangle" "\\rvert")
  "List of right delimiters acceptable after \\bigr etc.")

(defvar TeX+-ambiguous-delimiters
  '("." "|" "\\|")
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

(defun TeX+-corresponding-delim (delimiter)
  "Find the corresponding delimiter in the
TeX+-delim-prefix-pairs table.  Returns nil if not found."
  (or (cdr (assoc delimiter TeX+-delim-prefix-pairs))
      (car (rassoc delimiter TeX+-delim-prefix-pairs))))

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

(defun TeX+-find-matching-delimiter ()
  "If at a delimiter, goto the (beginning of) the matching
one (or its prefix, if present).  In case of an unmatched delimiter,
stop right before the border of math mode.

Currently, if both delimiters have non-matching prefixes, we still
move to the right spot, but signal an error."
  (let ((current-delim (TeX+-current-delimiter)))
    (when current-delim
      (if (memq current-delim '(left-prefix right-prefix)) (TeX+-forward-token))
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
	     (delim-counter 1))
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
	    (if (string= (TeX+-name-of-previous-token) (TeX+-corresponding-delim prefix))
		(TeX+-backward-token)
	      (error "Prefix mismatch!"))
	  (unless (string= prefix "")
	    (error "Prefix mismatch!")))
	(unless (texmathp)
	  (funcall goto-next -1)))))) ; get back into math mode

(defun TeX+-change-token-at-point (new-token)
  "Delete the token at point and insert NEW-TOKEN in its place.
Do nothing but beep if NEW-TOKEN is not a string (this is needed
in e.g. TeX+-enlarge-delimiters, when NEW-TOKEN is nil when we
want to enlarge \"\\left\" or \"\\right\")."
  (if (stringp new-token)		; This is WTFery; this test
					; should probably be done
					; inside
					; TeX+-enlarge-delimiters...
      (let ((token-info (TeX+-info-about-token-at-point)))
	(delete-region (car token-info) (cadr token-info))
	(insert new-token))
    (beep)))

