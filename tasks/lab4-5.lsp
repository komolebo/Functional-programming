(defun go1 (lst)
	(COND
	((NULL lst)  nil)
	(t (append (list (CAR lst) (CADR lst) (CADDR lst)) (go1 (CDDDDR (CDDDR lst)))))
	)
)

(defun lab4 (lst)
	(remove 'nil (go1 lst))
)

(defun go2 (lst1 lst2) 
	(COND 
	((NULL lst1) nil)
	((NOT (member (CAR lst1) lst2)) (append (list (CAR lst1)) (go2 (CDR lst1) lst2)))
	)
)

(defun lab5 (lst1 lst2)
	(append (go2 lst1 lst2) lst2)
)