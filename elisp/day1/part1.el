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

(defun mifz--get-line ()
  "Get the entire line content as string."
  (buffer-substring-no-properties (pos-bol) (pos-eol)))

(defun mifz--digitp (c)
  "Predicate: tell if the character C is a digit."
  (and (characterp c)
       (<= ?0 c ?9)))

(defun mifz--string-to-code-number (s)
  "Decipher the string 'S' and return the code."
  (let* ((number-list (seq-filter #'mifz--digitp s))
         (code-string (string
                        (car number-list)
                        (car (last number-list))))
         (code-number (string-to-number code-string)))
    code-number))

(defun mifz--last-linep ()
  "Verify if we are in the last line."
  (save-excursion (end-of-line) (eobp)))

(defun mifz--get-code-list ()
  "Get the code list from the buffer *input*."
  (let* ((code-list '()))
    (with-current-buffer (get-buffer-create "*input*")
      (goto-char (point-min))
      (while (not (mifz--last-linep))
        (let* ((current-line (mifz--get-line))
               (current-code (mifz--string-to-code-number current-line)))
          (setq code-list (cons current-code code-list))
          (forward-line)))
      code-list)))
(defun mifz--compute-code ()
  "Sum all the code from the *input* buffer."
  (let ((final-code (apply #'+ (mifz--get-code-list))))
    (mifz--set-output (number-to-string final-code))
  ))

(mifz--set-output
 (number-to-string (mifz--compute-code)))

