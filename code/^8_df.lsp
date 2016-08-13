(setq graph '((a c 10) (c f 6) (e f 5) (d e 6) (a d 4) (d f 12) (e e 9) (f b 40)))
(defun get_children(lst elem)
	(COND
	((NULL lst) nil)
	((eq (CAAR lst) elem) (CONS (CAR lst) (get_children (CDR lst) elem)))
	(t (get_children (CDR lst) elem))
	)
)

(defun go_deep (children n target way) 
	(COND
	((NULL (CAR children)) nil)	; зупинка рекурсії
		; якщо вершина знайдена, то перевіряємо її на 
		; оптимальність пройденого шляху і тоді запамятов. результат
	( (AND 	(eq (CADAR children) target) 
		(OR (< (+ n (CADDAR children)) best_length) (< best_length 0))) 
		   (progn (setq best_length (+ n (CADDAR children)))
			(setq best_way way)))
		; аналізуємо синів даної вершини (рекурсивно)
	((not (eq (CAAR children) (CADAR children)))	
		(go_deep (get_children graph (CADAR children)) 
			 (+ n (CADDAR children)) target  (append way (list (CADAR children)))
		))
	)
		; аналізуємо інші вершини цього батька
	(if (not (null (CDR children))) 
		(go_deep (CDR children) n target way))     
)
(defun depth-first (graph start target)
	(setq best_length -1)
	(setq best_way 	 nil)
	; запускаємо основну функцію
	(go_deep (get_children graph start) 0 target (list start))
	; додаємо у шлях останню вершину
	(if (atom best_way) (setq best_way (list best_way target)) (setq best_way (append best_way (list target))))
	; виводимо результат
	(if (> best_length 0) (format t "Best way: ~s  len:~d~%" best_way best_length) (format t "No way found~%"))
)