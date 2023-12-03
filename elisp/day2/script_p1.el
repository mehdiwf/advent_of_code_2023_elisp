

(defun mifz--load-file-to-input (filepath)
  (with-current-buffer (get-buffer-create "*input*")
    (erase-buffer)
    (insert-file-contents filepath)))

;; from d1
;;--------------------
(defun mifz--get-line ()
  "Get the entire line content as string."
  (buffer-substring-no-properties (pos-bol) (pos-eol)))

(defun mifz--last-linep ()
  "Verify if we are in the last line."
  (save-excursion (end-of-line) (eobp)))

(defun mifz--set-output (output-str)
  "Set the content of the *output* buffer.
'OUTPUT-STR' is the string set in the output buffer."
  (with-current-buffer (get-buffer-create "*output*")
    (erase-buffer)
    (insert output-str)
    ))
;;--------------------


(defvar mifzv--max
  '(("red" . 12)
    ("green" . 13)
    ("blue" . 14)))

(defvar mifzv--max-r 12 )
(defvar mifzv--max-g 13 )
(defvar mifzv--max-b 14 )

(defun mifz--parse-number-info (number-info-str)
  (let* ((fields (split-string number-info-str))
         (number (string-to-number (car fields)))
         (color (cadr fields))
         (alist-pair (list number color)))
    alist-pair))

(defun mifz--check-pull-info (pull-info-str)
  "the pull info should be ' 4 red, 3 blue' for instance"
  (let ((red-count 0)
        (green-count 0)
        (blue-count 0)
        (possible t)
        (count-info-list (split-string pull-info-str ",")))
    (while count-info-list
      (let* ((parsed-info (mifz--parse-number-info
                           (pop count-info-list)))
             (number (car parsed-info))
             (color (cadr parsed-info)))
        
        ;; (if (string= color "red")
        ;;     (setq red-count (+ number red-count)))
        ;; (if (string= color "green")
        ;;     (setq green-count (+ number green-count)))
        ;; (if (string= color "blue")
        ;;     (setq blue-count (+ number blue-count)))))

        (if (string= color "red")
            (if (> number mifzv--max-r)
                (setq possible nil)))
        (if (string= color "green")
            (if (> number mifzv--max-g)
                (setq possible nil)))
        (if (string= color "blue")
            (if (> number mifzv--max-b)
                (setq possible nil)))))
    possible))

(defun mifz--check-multiple-pulls-info (pulls-info-str)
  "the pull info should be ' 4 red, 3 blue; 2 red, 10 green' for
instance"
  (let ((possible t)
        (fields (split-string pulls-info-str ";")))
    (while fields
      (if (not (mifz--check-pull-info (pop fields)))
          (setq possible nil)))
    possible))

(defun mifz--check-game-info (full-game-info-str)
  "Parse the line, returns the game number if the game is possible,
0 if not"
  (let* ((game-number-str
          (car (split-string full-game-info-str ":")))
         (game-pulls-info-str
          (cadr (split-string full-game-info-str ":")))
         (game-nb (string-to-number
                   (cadr (split-string game-number-str))))
         (game-possible
          (mifz--check-multiple-pulls-info game-pulls-info-str)))
    (if game-possible
        game-nb
      0)))
(defun mifz--compute-code-from-input ()
  ""
  (let ((code 0))
    (with-current-buffer (get-buffer-create "*input*")
      (while (not (mifz--last-linep))
        (let* ((current-line (mifz--get-line))
               (number-to-add (mifz--check-game-info current-line)))
          (setq code (+ code number-to-add))
          (message "iter, code: %d" code)
          (forward-line))))
    code))


(mifz--load-file-to-input "./input_test.txt")
(mifz--parse-number-info " 3 red")
(mifz--check-pull-info " 3 red, 4 blue")
(mifz--check-game-info
 "Game 7: 2 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(mifz--load-file-to-input "./input.txt")
(mifz--set-output
 (number-to-string (mifz--compute-code-from-input)))

