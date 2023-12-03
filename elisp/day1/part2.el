(defun mifz--get-input ()
  "Return the content of the *input* buffer."
  (with-current-buffer (get-buffer-create "*input*")
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun mifz--set-output (output-str)
  "Set the content of the *output* buffer.
'OUTPUT-STR' is the string set in the output buffer."
  (with-current-buffer (get-buffer-create "*output*")
    (erase-buffer)
    (insert output-str)
    ))

;; (defun mifz--get-line ()
;;   ""
;;   (buffer-substring-no-properties (pos-bol) (pos-eol)))

(defvar mifz--dic
  "The alist used to compute the code."
  ;; the ` is important here, it's not a ', to allow the '@ to develop
  ;; the expression
  `(("one" . 1)
    ("two" . 2)
    ("three" . 3)
    ("four" . 4)
    ("five" . 5)
    ("six" . 6)
    ("seven" . 7)
    ("eight" . 8)
    ("nine" . 9)
    ,@(mapcar (lambda (n) (cons (number-to-string n) n))
              (number-sequence 0 9))))

(defvar mifz--regexp
  "The regexp used to find the numbers."
  (regexp-opt (mapcar #'car mifz--dic)))

(defun mifz--digitp (c)
  "Predicate: tell if the character C is a digit."
  (and (characterp c)
       (<= ?0 c ?9)))

(defun mifz--read-line-to-code-number (regexp)
  "Read the line with 'REGEXP' and return the code number."
  (let ((code-list '()))
    ;; code-list contains (tens units), with tens a digit between 0
    ;; and 9 and units too
    
    (end-of-line)
    (re-search-backward regexp)
    (let* ((found-string (match-string 0))
           (found-value (cdr (assoc found-string mifz--dic))))
      (setq code-list (cons found-value code-list)))
    
    (beginning-of-line)
    (re-search-forward regexp)
    (let* ((found-string (match-string 0))
           (found-value (cdr (assoc found-string mifz--dic))))
      (setq code-list (cons found-value code-list)))
    
    (let ((tens (* (car code-list) 10))
          (units (car (cdr code-list))))
      (+ tens units))))

(defun mifz--last-linep ()
  "Verify if we are in the last line."
  (save-excursion (end-of-line) (eobp)))

(defun mifz--get-code-list (regexp)
  "Get the code list from the buffer *input* with the regexp
'REGEXP'"
  (let* ((code-list '()))
    (with-current-buffer (get-buffer-create "*input*")
      (goto-char (point-min))
      (while (not (mifz--last-linep))
        (let* ((current-code (mifz--read-line-to-code-number regexp)))
          (setq code-list (cons current-code code-list))
          (forward-line)))
      code-list)))

(defun mifz--compute-code (regexp)
  "Sum all the code from the *input* buffer.
Using the regexp 'REGEXP'."
  (apply #'+ (mifz--get-code-list regexp)))

(mifz--set-output
 (number-to-string (mifz--compute-code mifz--regexp)))

