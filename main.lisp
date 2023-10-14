#| Задание 1 |#
(write-string "1")
(write
    (
        (lambda (x y z)
            (
                cons (car x) (cons (car y) (cons (car z) nil))
            )
        )
        '(Y U I) '(G1 G2 G3) '(KK LL MM JJJ)
    )
)
(terpri)

(terpri)
#| Задание 2 |#
(write-string "2")
(defun func (x y z)
            (
                cons (elt x 1) (cons (elt y 1) (cons (elt z 2) nil))
            )
        )

(write (func '(Y U I) '(G1 G2 G3) '(KK LL MM JJJ)))
(terpri)

(terpri)
#| Задание 3 |#
(write-string "3")
(defun funcThree (list1 list2)
  (if (and (listp list1) (numberp (car list1)))
      list2
      (cons (car list2) (cdr list1))))

(setq list1 '(3 4 5))
(setq list2 '(a b c))

(write (funcThree list1 list2))
(terpri)