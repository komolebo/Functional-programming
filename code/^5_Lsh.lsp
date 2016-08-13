(defun add-n-elements (lst)
	(COND 
	((OR (not (numberp (CAR lst))) (<= (CAR lst) 0)) nil)
	(t (cons (CADR lst) (add-n-elements (list (- (CAR lst) 1) (CADR lst)))))
	)
)

(defun F5 (lst)
	(COND ((NULL lst) nil)
	(t (append (add-n-elements (CAR lst)) (F5 (CDR lst))))
	)
)