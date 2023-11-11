(defun _exponent (base exponent)    
    (cond
        ((= exponent 0) 1)
        ((= exponent 1) base)
        (T (list '** base exponent))
    )
)

(defun is_exponent (x)
    (and (listp x) (eq (car x) '**))
)

(defun is_number_and_equal (exp num)
    (and (numberp exp) (= exp num))
)

(defun is_variable (x)
    (symbolp x)
)

(defun variable_is (v1 v2)
    (and
        (is_variable v1)
        (is_variable v2)
        (eq v1 v2)
    )
)

(defun make_product (m1 m2)
    (cond
            ((or (is_number_and_equal m1 0) (is_number_and_equal m2 0)) 0)
            ((is_number_and_equal m1 1) m2)
            ((is_number_and_equal m2 1) m1)
            ((and (numberp m1) (numberp m2)) (* m1 m2))
            (T (list '* m1 m2))
    )
)

(defun is_product (x)
    (and (listp x) (eq (car x) '*))
)

(defun make_sum (a1 a2)
    (cond
        ((is_number_and_equal a1 0) a2)
        ((is_number_and_equal a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (T (list '+ a1 a2))
    )
)

(defun is_sum (x)
    (and (listp x) (eq (car x) '+))
)

(defun deriv (exp var)
    (cond
        ((numberp exp) 0)
        ((is_variable exp) (if (variable_is exp var) 1 0))
        ((is_sum exp) (make_sum (deriv (cadr exp) var) (deriv (caddr exp) var)))
        ((is_product exp)
            (make_sum
               (make_product (cadr exp) (deriv (caddr exp) var))
               (make_product (deriv (cadr exp) var) (caddr exp))
            )
        )
        ((is_exponent exp)
             (make_product
                  (make_product (caddr exp)
                         (_exponent (cadr exp) (- (caddr exp) 1))
                  )
                  (deriv  (cadr exp) var)
             )
        )
        (T (error "Error type" exp))
    )
)


(terpri)
(write (deriv '(** x 3) 'x))
(terpri)