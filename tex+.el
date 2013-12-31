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
  (let ((token-start (point)) (token-end (+ (point) 1)))
    (save-excursion
; There are 3 cases: a control word, a control symbol or anything else.
; If we are on a letter, go back until we get to a nonletter.
      (cond ((TeX+-looking-at-letter at-is-letter)
	     (skip-chars-backward (TeX+-letter at-is-letter))
	     (if (bobp)
		 (cons token-start (1+ token-start))
	       (backward-char)
;   If we are on an unescaped backslash, we were on a control word.
	       (if (and
		    (looking-at (regexp-quote TeX-esc))
		    (not (TeX-escaped-p)))
		   (cons (point) (progn (forward-char)
					(skip-chars-forward (TeX+-letter at-is-letter))
					(point)))
;   If we are on an escaped backslash, we were on an ordinary character.
	       (cons token-start token-end))))
; If we are on an unescaped backslash, this might be a control word or a control symbol.
	    ((and (looking-at (regexp-quote TeX-esc))
		  (not (TeX-escaped-p)))
	     (forward-char)		; at EOF, this would lead to an error!
;   If this is a control symbol, return (token-start . token-end + 1)
	     (cons token-start (if (not (TeX+-looking-at-letter))
				   (1+ token-end)
;   If this is a control word, return (token-start . token-end + (number of letters))
				 (+ token-end (skip-chars-forward (TeX+-letter at-is-letter))))))
; If we are on a something else, back up one char and check whether there's an unescaped backslash
	    (t
	     (if (bobp)
		 (cons token-start (1+ token-start))
	       (backward-char)
	       (cons (if (and (looking-at (regexp-quote TeX-esc))
			      (not (TeX-escaped-p)))
;   If yes, return (token-start - 1 . token-end).
			 (- token-start 1)
;   If not, return (token-start . token-end).
		       token-start)
		     token-end)))))))

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

