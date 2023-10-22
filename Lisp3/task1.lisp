(defun method_sedjvik(s)               
    (if (= (mod s 2) 0)
        (
            + (- (* 9 (expt 2 s)) (* 9 (expt 2 (floor s 2)))) 1
        )
        (
            + (- (* 8 (expt 2 s)) (* 6 (expt 2 (floor (+ s 1) 2)))) 1
        )
    )
)

(defun coefficient (s size last) 
    (cond
        ((>= last size) (if (> s 0) (- s 2) 0 ) )
        (T
            (coefficient (+ s 1) size (method_sedjvik s))
        )
    )
)

(defun steps_list (s size last)
    (cond
        ((>= last size) '())
        (T
            (append (list (method_sedjvik s)) (steps_list (+ s 1) size (method_sedjvik s))
            )
        )
    )
)

(defun get_list_element (lst n) 
    (cond
        ((= n 0) (car lst))
        (T
            (get_list_element (cdr lst) (- n 1))
        )
    )
)

(defun replace_list_element (lst n elem) 
    (cond
        ((= n 0) (cons elem (cdr lst)))
        (T
            (cons (car lst) (replace_list_element (cdr lst) (- n 1) elem))
        )
    )
)

(defun remove_list_element (lst n) 
    (cond
        ((null lst) '())
        ((= n 0) (cdr lst))
        (T
            (cons (car lst) (remove_list_element (cdr lst) (- n 1)))
        )
    )
)

(defun cons_list(element lst) 
    (cond
        ((null lst) '())
        (T
            (cons
                (cons element (car lst))
                (cons_list element (cdr lst))
            )
        )
    )
)

(defun for_last (lst j tmp inc) 
    (cond
        ((or (< j 0) (<= (get_list_element lst j) tmp)) (replace_list_element lst (+ j inc) tmp))
        (T
            (for_last (replace_list_element lst (+ j inc) (get_list_element lst j)) (- j inc) tmp inc)
        )
    )
)

(defun for_first (lst size i inc) 
    (cond
        ((>= i size) lst)
        (T
            (for_first (for_last lst (- i inc) (get_list_element lst i) inc) size (+ i 1) inc)
        )
    )
)

#|------------------------------------------------------------------------------------------|#

(defun start (lst size s slst) 
    (cond
        ((< s 0) lst)
        (T
            (start (for_first lst size (get_list_element slst s) (get_list_element slst s)) size (- s 1) slst)
        )
    )
)

(defun _init (lst)
    (cond
      ((null lst) 'Empty)
      (T (start lst (length lst) (coefficient 0 (length lst) 0) (steps_list 0 (length lst) 0)))
    )
)

(write-string "Result ")
(write (_init '(12 8 14 6 4 9 1 8 13 5 11 3 18 3 10 9)))
(terpri)