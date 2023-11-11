(defun sublist-of-depth-N (lst1 lst2 n)
  (cond
    ((or (null lst2) (= n 0)) t)
    ((null lst1) nil)
    ((equal (car lst1) (car lst2))
     (or (sublist-of-depth-N (cdr lst1) (cdr lst2) n)
         (sublist-of-depth-N (cdr lst1) lst2 n)))
    (t (sublist-of-depth-N (cdr lst1) lst2 n))))

(write (sublist-of-depth-N '(1 2 3 4 5 (6 (7 8))) '(3 4) 2))