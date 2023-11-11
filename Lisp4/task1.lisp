(defun _factorial(n r)
    (
        (lambda (n r)
            (cond
                    ((= n 0) r)
                    (T
                        (_factorial (- n 1) (* r n))
                    )
            )
        )
        n r
    )
)

(terpri)
(write (_factorial 5 1))
(terpri)