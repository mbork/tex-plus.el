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

(defun TeX+-token-at-point (&optional at-is-letter)
  "Returns a cons cell with the boundaries of the token at point."
  (let ((opoint (point)))
    (save-excursion
; There are 3 cases: a control word, a control symbol or anything else.
; If we are on a letter, go back until we get to a nonletter.
      (cond ((TeX+-looking-at-letter at-is-letter)
	     (skip-chars-backward (TeX+-letter at-is-letter))
	     (if (bobp)
		 (cons opoint (1+ opoint))
	       (backward-char)
;   If we are on an unescaped backslash, we were on a control word.
	       (if (and
		    (looking-at (regexp-quote TeX-esc))
		    (not (TeX-escaped-p)))
		   (cons (point) (progn (forward-char)
					(skip-chars-forward (TeX+-letter at-is-letter))
					(point)))
;   If we are on an escaped backslash, we were on an ordinary character.
	       (cons opoint (1+ opoint)))))
; If we are on an unescaped backslash, this might be a control word or a control symbol.
	    ((and (looking-at (regexp-quote TeX-esc))
		  (not (TeX-escaped-p)))
	     (forward-char)		; at EOF, this would lead to an error!
;   If this is a control symbol, return (opoint . opoint + 2)
	     (cons opoint (if (not (TeX+-looking-at-letter))
				   (+ 2 opoint)
;   If this is a control word, return (opoint . opoint + 1 + (number of letters))
				 (+ opoint 1 (skip-chars-forward (TeX+-letter at-is-letter))))))
; If we are on a something else, back up one char and check whether there's an unescaped backslash
	    (t
	     (if (bobp)
		 (cons opoint (1+ opoint))
	       (backward-char)
	       (cons (if (and (looking-at (regexp-quote TeX-esc))
			      (not (TeX-escaped-p)))
;   If yes, return (opoint - 1 . opoint + 1).
			 (- opoint 1)
;   If not, return (opoint . opoint + 1).
		       opoint)
		     (1+ opoint))))))))

(defun TeX+-forward-token (&optional count)
  "Move forward COUNT tokens."
  (interactive "^p")
  (let ((count (or count 1)))
    (if (> count 0)
	(dotimes (unused count)
	  (goto-char (cdr (TeX+-token-at-point))))
      (dotimes (unused (- count))
	(backward-char)
	(let ((boundaries (TeX+-token-at-point)))
	  (unless (= 1 (- (cdr boundaries) (car boundaries)))
	    (goto-char (car boundaries))))))))

(defun TeX+-backward-token (&optional count)
  "Move backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "^p")
  (TeX+-forward-token (- (or count 1))))

