;;;; My Elisp hacks for better AUCTeX experience

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
:normal-character)."
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
	     (forward-char)		; at EOF, this would lead to an error!
	     (if (TeX+-looking-at-letter)
;   If this is a control word, return (opoint (opoint + 1 + (number of letters)) :control-word)
		 (list opoint
		       (+ opoint 1 (skip-chars-forward (TeX+-letter at-is-letter)))
		       :control-word)
;   If this is a control symbol, return (opoint (opoint + 2) :control-symbol)
	       (list opoint (+ 2 opoint) :control-symbol)))
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
		 (list opoint (1+ opoint) :normal-character))))))))

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

