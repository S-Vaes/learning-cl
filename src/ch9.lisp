;; Input output

"strings"

(setf a "This is a string")
(stringp a)
(setf b 'this-is-a-symbol)
(stringp b)


(format t "Hi, world!")
;; ~% is a newline
(format t "Fruit flies~%~%like bananas.")
;; ~& moves to a new line unless it's already at the start of a line.
(defun mary ()
  (format t "~&Mary had a little bat.")
  (format t "~&Its wing were long and brown."))
(mary)
;; ~S formats an sexpr
(format t "From ~S to ~S in ~S minutes!"
        'boston '(new york) 55)
(defun square-talk (n)
  (format t "~&~S squared is ~S" n (* n n)))
(square-talk 10)

(defun pilots ()
  (format t "There are old pilots,~&there are bold pilots~&, but there are no old bold pilots."))
(pilots)

(defun draw-line (n)
  (cond ((zerop n) nil)
        (t (format t "*")
           (draw-line (1- n)))))
(draw-line 10)
(defun draw-box (col row)
  (cond ((zerop row) nil)
        (t (draw-line col)
           (format t "~&")
           (draw-box col (1- row)))))
(draw-box 10 4)

(defun ninety-nine-bottles (n)
  (cond ((zerop n) nil)
        (t (format t "~&~S bottles of beer on the wall,~&~S bottles of beer!~&Take one down,~&Pass it around," n n)
           (ninety-nine-bottles (1- n)))))
(ninety-nine-bottles 1)

(defun print-board (board)
  "Takes a list of 9 elements (X, O, or NIL) and prints a tic-tac-toe board."
  (format t "~&~A | ~A | ~A" (or (nth 0 board) " ") (or (nth 1 board) " ") (or (nth 2 board) " "))
  (format t "~&---------")
  (format t "~&~A | ~A | ~A" (or (nth 3 board) " ") (or (nth 4 board) " ") (or (nth 5 board) " "))
  (format t "~&---------")
  (format t "~&~A | ~A | ~A" (or (nth 6 board) " ") (or (nth 7 board) " ") (or (nth 8 board) " "))
  nil)
;; (print-board '(X O X nil nil nil O X O))

(defun read-square ()
  (format t "Please type in a number: ")
  (let ((x (read)))
    (format t "The numbers ~S squared is ~S.~%" x (* x x))))
'(read-square)

(defun calc-wages ()
  (format t "~&Hours worked: ")
  (let ((hours (read)))
    (format t "~&Wage per hour: ")
    (let ((wages (read)))
      (format t "~&Gross pay is ~A~&" (* hours wages)))))
;; (calc-wages)

(defun cookie-monster ()
  "Reads input from terminal until the symbol COOKIE is encountered."
  (format t "~%Feed me! Enter something: ")
  (let ((input (read)))
    (cond
      ((eq input 'COOKIE)
       (format t "~%Om nom nom nom... COOKIE! *munch* *munch*~%")
       input)
      (t
       (format t "~%~A's not a cookie! Me want cookie!~%" input)
       (cookie-monster)))))
;; (cookie-monster)

(defun riddle ()
  (if (yes-or-no-p
       "Do you seek enlightenment?")
      (format t "Then do not ask for it!")
      (format t "You have found it.")))
;; (riddle)

;; Read files with `with-open-file
;; Special stream object in a `let` like context
(defun get-tree-data ()
  (with-open-file (stream "./src/timber.dat")
    (let* ((tree-loc (read stream))
           (tree-table (read stream))
           (num-trees (read stream)))
      (format t "~&There are ~S trees on ~S."
              num-trees tree-loc)
      (format t "~&They are ~S" tree-table))))
;; (get-tree-data)
;; Writing files
(defun save-tree-data (tree-loc tree-table num-trees)
  (with-open-file (stream "./src/timber.newdat"
                          :direction :output)
    (format stream "~S~%" tree-loc)
    (format stream "~S~%" tree-table)
    (format stream "~S~%" num-trees)))
;; (save-tree-data "The West Ridge"
;;                 '((45 redwood) (22 oak) (43 maple))
;;                 110)
;; (format t "a~S" 'b)
;; (format t "always~%broke")
;; (format t "~S~S" 'alpha 'bet)

(defun space-over (n)
  (cond ((zerop n) nil)
        ((< n 0) (format t "Error!"))
        (t (format t " ")
           (space-over (1- n)))))
(defun test-space-over (n)
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))
;; (test-space-over 10)

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~A~%" plotting-string))
(format t "~%")
(plot-one-point "****" 4)

(defun plot-points (plotting-string point-list)
  (mapcar (lambda (elem) (plot-one-point plotting-string elem)) point-list))
(plot-points "< >" '(4 6 8 10 8 6 4))

(defun generate (lower upper)
  (cond ((eq lower (1+ upper)) nil)
        ((> lower upper) 'ERROR)
        (t (cons lower (generate (1+ lower) upper)))))
(generate -3 3)

(defun make-graph (func start end plotting-string)
  (plot-points plotting-string (mapcar func (generate start end))))
(defun square (x)
  (* x x))
(make-graph #'square -14 14 "mahsa")

(defun read-my-file ()
  (with-open-file (stream "./src/timber.dat")
    (let ((contents
            (read-all-objects stream (list '$eof$))))
      (format t "~&Read ~S object from the file."
              (length contents))
       contents)))
(defun read-all-objects (stream eof-indicator)
  (let ((result (read stream nil eof-indicator)))
    (if (eq result eof-indicator)
        nil
        (cons result
              (read-all-objects stream
                                eof-indicator)))))
(read-my-file)
