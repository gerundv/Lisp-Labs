(defun interpret (expr)
    (cond
        ((not (symbolp (car expr))) expr)
        ((not (fboundp (car expr))) expr)
        (T
            (
                (lambda (m n)
                    (apply m n)
                )
                (car expr)
                (arg_proc (cdr expr))
            )
        )
    )
)

(defun arg_proc (lst)
    (cond
        ((null lst) nil)
        ((atom (car lst)) (cons (car lst) (arg_proc (cdr lst))))
        ((equal (symbol-name (caar lst)) "QUOTE") (cdar lst))
        (T
            (cons (interpret (car lst))
                  (arg_proc (cdr lst)))
        )
    )
)

(write (interpret '(cons (car (cdr '(e r t w))) (cons (cdr '(g h 6)) '()))))