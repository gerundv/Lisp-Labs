(defun _reverse (w)
  (cond ((null w) nil)
        ((_append (_reverse (cdr w)) (list (car w))))))
          
(defun _append (w v)
  (cond ((null w) v) 
        ((cons (car w) (_append (cdr w) v)))))

(defun rev_list (lst)
  (if (null lst)
      '()
      (_append (_reverse lst) (rev_list (cdr lst)))))

(setq input-list '(1 2 3 4 5 6))
(write (rev_list input-list))