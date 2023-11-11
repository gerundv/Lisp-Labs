(defun reverse-and-append (lst)
  (labels ((reverse-tail (tail)
             (if (null tail)
                 '()
                 (append (reverse tail) (reverse-tail (cdr tail))))))
    (reverse-tail lst)))

(setq example-list '(1 2 3 4 5 6))
(write (reverse-and-append example-list))