(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  (cond ((equal v 1) "O")
        ((equal v 10) "X")
        (t " ")))

(defun print-row (x y z)
  (format t "~&    ~A | ~A | ~A"
          (convert-to-letter x)
          (convert-to-letter y)
          (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~&   -----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~&   -----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~&   -----------"))

(setf b (make-board))
(print-board b)

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(setf *computer* 10)
(setf *opponent* 1)
(make-move *opponent* 3 b)
(make-move *computer* 5 b)
(print-board b)

(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9)         ;Horizontal
        (1 4 7) (2 5 8) (3 6 9)         ;Middle
        (1 5 9) (3 5 7)))               ;Bottom

(setf *corners* '(1 3 7 9))
(setf *sides* '(2 4 6 8))
(setf *diagonals* '((1 5 9) (3 5 7)))

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))
(sum-triplet b '(3 5 7))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet) (sum-triplet board triplet))
          *triplets*))
(compute-sums b)

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))

(defun play-one-game ()
  (if (y-or-n-p "Would you like to got first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move
                     *opponent*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&You win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (computer-move new-board)))))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t
                   "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move
                     *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&I win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (opponent-move new-board)))))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (take-center board)
      (setup-squeeze-play board)
      (setup-two-on-one board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (random-move-strategy board)))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
        (pick-random-empty-position board))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board
                           (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if
                  #'(lambda (trip)
                      (equal (sum-triplet board
                                          trip)
                             target-sum))
                  *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           squares))

(setf b '(board
          1   0   0
          0   10  0
          0   0   1))

(defun block-squeeze-play (board)
  (let ((pos (check-diagonals board)))
    (when pos
      (list pos "block squeeze play"))))

;; (defun check-diagonals (board)
;;   (and (eq (sixth board) 10)
;;        (or (eq (+ (nth (first *corners*) board) (nth (fourth *corners*) board)) 2)
;;            (eq (+ (nth (second *corners*) board) (nth (third *corners*) board)) 2))))

(defun check-diagonals (board)
  (cond
    ;; Check diagonal 1-5-9
    ((and (eq (nth 5 board) *computer*)
          (eq (nth (first *corners*) board) *opponent*)
          (eq (nth (fourth *corners*) board) *opponent*))
     (pick-random-empty-position-from-list board *sides*))

    ;; Check diagonal 3-5-7
    ((and (eq (nth 5 board) *computer*)
          (eq (nth (second *corners*) board) *opponent*)
          (eq (nth (third *corners*) board) *opponent*))
     (pick-random-empty-position-from-list board *sides*))

    ;; No squeeze play detected
    (t nil)))

(defun pick-random-empty-position-from-list (board positions)
  (let ((empty-positions (remove-if-not
                          #'(lambda (pos)
                              (zerop (nth pos board)))
                          positions)))
    (when empty-positions
      (nth (random (length empty-positions)) empty-positions))))

(defun block-two-on-one (board)
  (let ((pos (check-two-on-one board)))
    (when pos
      (list (pick-random-empty-position-from-list board *corners*) "block two-on-one threat"))))

(defun check-two-on-one (board)
  (or
   ;; Check diagonal 1-5-9
   (check-diagonal-threat board (first *diagonals*))

   ;; Check diagonal 3-5-7
   (check-diagonal-threat board (second *diagonals*)))
   ;; No two-on-one threat detected
   ;; nil
  )

(defun check-diagonal-threat (board diagonal)
  (let ((sum (sum-triplet board diagonal))
        (p1 (first diagonal))
        (p3 (third diagonal)))

    ;; Check if sum is computer + 2*opponent (X+O+O)
    (when (= sum (+ *computer* (* 2 *opponent*)))
      (cond
        ((eq *computer* (nth p1 board)) p1)
        ((eq *computer* (nth p3 board)) p3)

        ;; Otherwise no corner to defend
        (t nil)))))

(defun take-center (board)
  (when (zerop (nth 5 board))
    (list 5 "take center")))

(defun setup-squeeze-play (board)
  "Try to set up a squeeze play by placing X in the center with the opponent
   having only one piece on the board."
  (let ((opponent-count (count *opponent* board))
        (computer-count (count *computer* board)))
    (when (and (= opponent-count 1)
               (= computer-count 1)
               (zerop (nth 5 board))) ;; Center is empty
      ;; Take the center to begin setting up a squeeze play
      (list 5 "setup squeeze play - take center"))))

(defun setup-two-on-one (board)
  "Try to set up a two-on-one situation on a diagonal"
  (let ((opponent-count (count *opponent* board))
        (computer-count (count *computer* board)))
    (when (and (= opponent-count 1)
               (= computer-count 1))
      (let ((computer-pos (position *computer* board))
            (opponent-pos (position *opponent* board)))
        (cond
          ;; Computer in center, set up two-on-one on a diagonal
          ((= computer-pos 5)
           (let ((diagonal (find-diagonal-with opponent-pos)))
             (when diagonal
               (let ((corner (find-empty-corner-in-diagonal board diagonal opponent-pos)))
                 (when corner
                   (list corner "setup two-on-one attack"))))))

          ;; Computer in corner, opponent not in center, take center
          ((and (member computer-pos *corners*)
                (not (= opponent-pos 5))
                (zerop (nth 5 board)))
           (list 5 "setup two-on-one from corner"))

          ;; Computer in corner, opponent in center, take opposite corner
          ((and (member computer-pos *corners*)
                (= opponent-pos 5))
           (let ((opposite-corner (get-opposite-corner computer-pos)))
             (when (zerop (nth opposite-corner board))
               (list opposite-corner "setup fork opportunity"))))

          (t nil))))))

(defun find-diagonal-with (position)
  "Find a diagonal that contains the given position"
  (find-if #'(lambda (diag) (member position diag)) *diagonals*))

(defun find-empty-corner-in-diagonal (board diagonal opponent-pos)
  "Find an empty corner in the given diagonal that is not the opponent position"
  (let ((corners (remove-if-not #'(lambda (pos)
                                    (and (member pos diagonal)
                                         (member pos *corners*)
                                         (not (= pos opponent-pos))
                                         (zerop (nth pos board))))
                                (range 1 9))))
    (when corners
      (car corners))))

(defun get-opposite-corner (corner)
  "Get the opposite corner position"
  (case corner
    (1 9)
    (3 7)
    (7 3)
    (9 1)
    (t nil)))

(defun range (start end)
  "Create a list of numbers from start to end"
  (loop for i from start to end collect i))

(setf b1 '(board
           10   0   0
           0   1  0
           0   0   1))
;; (computer-move b1)

;; (computer-move b)
