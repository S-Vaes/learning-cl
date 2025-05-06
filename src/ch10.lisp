(setf *total-glasses* 0)
(defun sell (n)
  "Ye Olde Lemonade Stand: Sales by the Glass."
  (setf *total-glasses* (+ *total-glasses* n))
  (format t
          "~&That makes ~S glasses so far today."
          *total-glasses*))

(defun sell2 (n)
  "Ye Olde Lemonade Stand: Sales by the Glass."
  (setf *total-glasses* (incf *total-glasses* n))
  (format t
          "~&That makes ~S glasses so far today."
          *total-glasses*))
(sell 1)
(sell 5)

(setf a 2)
(incf a 10)
(decf a)

(sell2 5)

(setf mystack nil)
(push 'dish1 mystack)
(push 'dish2 mystack)
(push 'dish3 mystack)

(pop mystack)
mystack

;; Equivalent to pop
(let ((top-element (first mystack)))
  (setf mystack (rest mystack))
  top-element)

(setf *friends* nil)
(setf *mult-met-counter* 0)
(defun meet (person)
  (cond ((equal person (first *friends*))
         (incf *mult-met-counter*)
         'we-just-met)
        ((member person *friends*)
         (incf *mult-met-counter*)
         'we-know-each-other)
        (t (push person *friends*)
           'pleased-to-meet-you)))
(defun forget (person)
  (cond ((not (member person *friends*))
         'i-do-not-know-you)
        (t (setf *friends* (remove person *friends*)))))

(meet 'fred)
(meet 'cindy)
(meet 'cindy)
(meet 'joe)
(meet 'fred)
*friends*

*mult-met-counter*

;; Don't update local and arg variables, it's inelegant.
;; Exception is `disciplined assignment`

;; When and unless

(defun picky-multiply (x y)
  "X must be odd and y must be even"
  (unless (oddp x)
    (incf x)
    (format t
            "~&Changing X to ~S to make it odd." x))
  (when (oddp y)
    (decf y)
    (format t
            "~&Changing Y to ~S to make it even." y))
  (* x y))
(picky-multiply 4 6)
(picky-multiply 2 9)

;; A generalized variable is any place a pointer may be stored.
(setf x '(jack benny was 39 for many years))
(setf (sixth x) 'several)
(decf (fourth x) 2)
x

(defun ugly (x y)
  (when (> x y)
    (setf temp y)
    (setf y x)
    (setf x temp))
  (setf avg (/ (+ x y) 2.0))
  (setf pct (* 100 (/ avg y)))
  (list 'average avg 'is pct 'percent 'of 'max y))

(defun nice (x y)
  (let* ((min-val (min x y))
         (max-val (max x y))
         (avg (/ (+ min-val max-val) 2.0))
         (pct (* 100 (/ avg max-val))))
    (list 'average avg 'is pct 'percent 'of 'max max-val)))
(nice 20 2)

(setf stack2 '(1 2 3 4))
(push 2 stack2)
(setf x nil)
(push x x)
;; (setf (length x) 3)

(defun caverage (x y)
  (unless (and (numberp x ) (numberp y))
    (error "Arguments must be numbers: ~S, ~S" x y))
  (/ (+ x y) 2.0))

;; Modifying lists directly by changing the pointers in the cons cells is called as list surgery

;; Destructive operations on lists
;; nconc is a destructive version of append
(setf x '(a b c))
(setf y '(d e f))
(append x y)

;; (nconc x y)
;; If the first arg is nil, its value will be unchanged -> use setf anyway

;; nsubst is the destructive version of subst
;; also: nreverse nunion nintersect nset-difference
;; delete is the destructive version of remove

(car '(fee foe foo))
(defun chop (xs)
  (cond ((null xs) nil)
        (t (setf (cdr xs) nil)
           xs)))
(setf testx '(fee foe foo))
;; (chop testx)
(defun tack (xs newx)
  (nconc xs (list newx)))
(tack testx 'fum)
testx
(setf h '(hi ho))
