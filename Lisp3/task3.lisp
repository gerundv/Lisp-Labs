(defun merge (frt sec)
    (cond
        ((null frt) sec)
        ((null sec) frt)
        (
            (< (car frt) (car sec))
            (cons (car frt) (merge (cdr frt) sec))
        )
        (T
            (cons (car sec) (merge frt (cdr sec)))
        )
    )
)   

(terpri)
(write (merge '(-5 -2 1 3 6 7) '(-2 0 1 2 3 4 5 20)))
(terpri)