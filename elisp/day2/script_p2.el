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
  "parses a string like ' 4 red' and returns the list (4 'red')"
  (let* ((fields (split-string number-info-str))
         (number (string-to-number (car fields)))
         (color (cadr fields))
         (alist-pair (list number color)))
    alist-pair))

(defun mifz--return-pull-info (pull-info-str)
  "the pull info should be ' 4 red, 3 blue' for instance.
Returns '(4 0 3) in this example"
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

        (if (string= color "red")
            (setq red-count number))
        (if (string= color "green")
            (setq green-count number))
        (if (string= color "blue")
            (setq blue-count number))
    ))
    (list red-count green-count blue-count)))

;; (defun mifz--check-pull-info (pull-info-str)
;;   "the pull info should be ' 4 red, 3 blue' for instance"
;;   (let ((red-count 0)
;;         (green-count 0)
;;         (blue-count 0)
;;         (possible t)
;;         (count-info-list (split-string pull-info-str ",")))
;;     (while count-info-list
;;       (let* ((parsed-info (mifz--parse-number-info
;;                            (pop count-info-list)))
;;              (number (car parsed-info))
;;              (color (cadr parsed-info)))

;;         (if (string= color "red")
;;             (if (> number mifzv--max-r)
;;                 (setq possible nil)))
;;         (if (string= color "green")
;;             (if (> number mifzv--max-g)
;;                 (setq possible nil)))
;;         (if (string= color "blue")
;;             (if (> number mifzv--max-b)
;;                 (setq possible nil)))))
;;     possible))

(defun mifz--compute-power-from-game-info (pulls-info-str)
  "the pull info should be ' 4 red, 3 blue; 2 red, 10 green' for
instance"
  (let ((red-count-list '())
        (green-count-list '())
        (blue-count-list '())
        (fields (split-string pulls-info-str ";")))
    (while fields
      (let ((pull-info (mifz--return-pull-info (pop fields))))
        (push (nth 0 pull-info) red-count-list)
        (push (nth 1 pull-info) green-count-list)
        (push (nth 2 pull-info) blue-count-list)))
    (* (seq-max red-count-list)
       (seq-max green-count-list)
       (seq-max blue-count-list))))

;; (defun mifz--check-game-info (full-game-info-str)
;;   "Parse the line, returns the game number if the game is possible,
;; 0 if not"
;;   (let* ((game-number-str
;;           (car (split-string full-game-info-str ":")))
;;          (game-pulls-info-str
;;           (cadr (split-string full-game-info-str ":")))
;;          (game-nb (string-to-number
;;                    (cadr (split-string game-number-str))))
;;          (game-possible
;;           (mifz--check-multiple-pulls-info game-pulls-info-str)))
;;     (if game-possible
;;         game-nb
;;       0)))
(defun mifz--compute-code-from-input ()
  ""
  (let ((code 0))
    (with-current-buffer (get-buffer-create "*input*")
      (goto-char (point-min))
      (while (not (mifz--last-linep))
        (let* ((current-line (mifz--get-line))
               (game-info
                (nth 1 (split-string current-line ":")))
               (number-to-add (mifz--compute-power-from-game-info game-info)))
          ;; (message "line: %s\nnumber: %s"
          ;;          current-line
          ;;          (number-to-string number-to-add))
          (setq code (+ code number-to-add))
          (forward-line))))
    code))

(mifz--load-file-to-input "./input.txt")

(mifz--set-output
 (number-to-string (mifz--compute-code-from-input)))
