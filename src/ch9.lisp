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
(print-board '(X O X nil nil nil O X O))

(defun read-square ()
  (format t "Please type in a number: ")
  (let ((x (read)))
    (format t "The numbers ~S squared is ~S.~%" x (* x x))))
(read-square)
