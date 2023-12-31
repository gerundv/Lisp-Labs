(defun insert (element sorted-list)
  (if (null sorted-list)
      (list element)
      (if (< element (car sorted-list))
          (cons element sorted-list)
          (cons (car sorted-list) (insert element (cdr sorted-list))))))

(defun insertion-sort (lst)
  (if (null lst)
      '()
      (insert (car lst) (insertion-sort (cdr lst)))))

(terpri)
(write (insertion-sort '(12 8 14 6 4 9 1 8 13 5 11 3 18 3 10 9)))
(terpri)
