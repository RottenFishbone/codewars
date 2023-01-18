(defpackage #:challenge/solution
  (:use #:cl)
  (:export #:solution))
(in-package #:challenge/solution)

(defun solution (number)
  " Use the summation closed form (n(n+1))/2 to sum multiples
    of 3 and 5 then subtract common multiples (15's)
  "
  (if (>= number 0)
      (let* ((n (- number 1))
             (n3 (floor n 3))
             (n5 (floor n 5))
             (n15 (floor n 15))
             (s3 (* (* n3 (+ n3 1)) 3))
             (s5 (* (* n5 (+ n5 1)) 5))
             (s15 (* (* n15 (+ n15 1)) 15))
             (ans (floor (- (+ s3 s5) s15) 2)))
        ans)
      0))
