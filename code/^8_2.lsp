(setq graph '((a c 10) (c f 6) (e f 5) (d e 6) (a d 4) (d f 12) (f b 40) (e e 9)))
;~~~~~~~~~~~~~~~~~~~~
(defun degree (graph node)	
	(setq counter 0)
	(loop for x in graph do 
		(if (eq (CAR x) node) (setq counter (1+ counter))) )	
	(return-from degree counter)
)
;~~~~~~~~~~~~~~~~~~~~
; предикат наявності елемента в списку
(defun contains (lst el)
	(COND
	((NULL lst) nil)
	((eq (CAR lst) el) t)
	(t (contains (CDR lst) el))
	)
)
; шукає та виводить список всіх вершин
(defun form-all-nodes (graph)
	(setq all-nodes nil)
	(if (OR (NULL graph) (NULL (CAR graph))) (return-from form-all-nodes))	
	(loop for x in graph do 
		(if (not (contains all-nodes (CADR x)))  (setq all-nodes (append all-nodes (list (CADR x))))) )
	(loop for x in graph do 
		(if (not (contains all-nodes (CAR x)))  (setq all-nodes (append all-nodes (list (CAR x))))) )
	(return-from form-all-nodes all-nodes)
)
; повертає елемент із мінімальним ступенем
(defun get-min (graph lst)
	(setq min_node (CAR lst))
	(loop for x in lst do
		(if (< (degree graph x) (degree graph min_node)) (setq min_node x)) )
	(return-from get-min min_node)
)
; сортує список відповідно до ступеня вершин
(defun nodes-sort (graph lst)
	(if (NULL lst) (return-from nodes-sort))
	(setq min (get-min graph lst))
	(cons min (nodes-sort graph (remove min lst)))
)
; створює відсортований список вершин за зростанням ступеня
(defun nodes-list (graph)
	(return-from nodes-list (nodes-sort graph (form-all-nodes graph)))
)
