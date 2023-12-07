(defun mifz--get-numbers-info-from-str-t (line-str)
  "'...324.*..$.12..' gives ((324 4 6) (12 13 14))"
  (with-current-buffer (get-buffer-create "*process*")
    (insert line-str)
    (point-min)
    (let ((search-point t)
          (info-list '())
          (test-list '()))
      (while search-point
        (setq search-point
              (re-search-forward "[0-9]+" (pos-eol) t))
        (if search-point
            (push
             (list (string-to-number (match-string 0))
                   (match-beginning 0)
                   (match-end 0))
             info-list))
        (push 1 test-list))
      (message "%d" (length info-list))
      )))
(defun mifz--symbolp (c)
  ""
  (or (char-equal c 46)
      (<= ?0 c ?9)))
(defun mifz--get-symbols-positions-from-str (line-str)
  "'...324.*..$.12..' gives (8 11)"
  (let ((nmax (length line-str))
        (pos-list '())
        (i 0))
    (while (< i nmax)
      (let* ((ci (aref line-str i))
             (pos (1+ i)))
        (if (not (mifz--symbolp ci))
            (push pos pos-list))
        (setq i (1+ i))
        ))
    pos-list))
(mifz--get-symbols-positions-from-str
 "...324.*..$.12..")
(mifz--get-symbols-positions-from-str
 "*")
(mifz--get-symbols-positions-from-str
 "...*.1")

(defun mifz--get-numbers-info-from-str (line-str)
  "'...324.*..$.12..' gives ((324 4 6) (12 13 14))"
  (with-temp-buffer
    (insert line-str)
    (goto-char (point-min))
    (let ((search-point t)
          (info-list '())
          (while-nb 0))
      (while (and search-point
                  (<= while-nb 5))
        (let ((match-found
               (re-search-forward "[0-9]+" (pos-eol) t)))
          ;; (message "match found:")
          ;; (message match-found)
          ;; (message "point: %d" (point))
          (setq while-nb (+ while-nb 1))
          (if match-found
              (let* ((match-str (match-string 0))
               (begin-pos (match-beginning 0))
               (end-pos (match-end 0)))
                (push
                 (list (string-to-number match-str)
                       begin-pos end-pos)
                 info-list)))))
      (message "%d" (length info-list))
      info-list)))

(mifz--get-numbers-info-from-str "...32..1..3485..48")
;; (mifz--get-numbers-info-from-str-t "...32..1..3485..48")
(if 0 (message "y"))
