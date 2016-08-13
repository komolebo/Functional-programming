(setq graph '((a c 10) (c f 6) (e f 5) (d e 6) (a d 4) (d f 12) (f b 40) (e e 9)))

; �������� �������� �������� � ������
(defun contains (lst el)
	(COND
	((NULL lst) nil)
	((eq (CAR lst) el) t)
	(t (contains (CDR lst) el))
	)
)

; ������� ���� ������ ���� � ����� ���� �� ����� �������
(defun get-to-last (lst el)
	(COND
	((NULL lst) nil)
	((eq (CAR lst) el) (list el))
	(t (CONS (CAR lst) (get-to-last (CDR lst) el)))
	)
)
; �� ����� ������ ������ ����� ����� �� ����� �� �������� ����� �� ������
(defun get-needed-ways (lst end)
	(COND
	((NULL lst) nil)
	((NOT (contains (CAR lst) end)) (get-needed-ways (CDR lst) end))
	(t (CONS (get-to-last (CAR lst) end) (get-needed-ways (CDR lst) end)))
	)
)
; ������������ ��� ������ �������� ����� �� �����, ���� ������������� ������������
(defun no-isolated-nodes (graph)
	(COND
	((NULL (CAR graph)) nil)
	((not (eq (CAAR graph) (CADAR graph))) (CONS (CAR graph) (no-isolated-nodes (CDR graph))))
	(t (no-isolated-nodes (CDR graph)))
	)
)
; ������� get-children ���� ������ ���� ������  parent
(defun get-children (graph parent)
	(setq lst nil)
	(loop for node in graph do
		(if (eq (CAR node) parent) (setq lst (append lst (list (CADR node)) )))
	)
	(return-from get-children lst)
)
; ������� get-conn-weight �������� ���� ���� ����� �� ����� �� start->finish
(defun get-conn-weight (graph start finish)
	(COND 
	((NULL (CAR graph)) 0)
	;((eq start finish) 0)
	((AND (eq (CAAR graph) start) (eq (CADAR graph) finish))  (CADDAR graph))
	(t (get-conn-weight (CDR graph) start finish))
	)
)
; ������� get-path-weight �������� ���� ��� ��� ����� �� ������� �����
(defun get-path-weight (graph lst counter)
	(if (NULL (CDR lst)) (return-from get-path-weight counter))
	(setq counter (+ counter (get-conn-weight graph (CAR lst) (CADR lst))))
	(get-path-weight graph (CDR lst) counter)
)
; get-ways ������� ������� ���� � ������ ��� �������� ������ (��������� ������������)
(defun get-ways (graph queue ways end)
	(format t "queue: ~S       ways: ~S~%" queue ways)
	; ����� ������� ������
	(if (NULL queue) (return-from get-ways ways))
	(setq tmp nil) 	; ����� �� ���� ��������� ������� �� ����� ����

	; ��������� �� ����� �� ������������ ����
	(loop for way in ways do
		(loop for kid in (get-children graph (CAR (last way))) do
			(setq tmp (append tmp (list (append way (list kid))))) ) )
	; ���� ���� ����� ��� ��������
	(loop for way in ways do 
		(if (NULL (get-children graph (CAR (last way)))) (setq tmp (append tmp (list way)))) )	
	(setq ways tmp)	
		
	; ������� ���� ����� �� ����� ���� ������� �������� ������
	(setq queue nil)	
	(loop for way in ways do
		(setq queue (append queue (get-children graph (CAR (last way))))) )
	;(format t "new_ways:~S~%queue: ~S~%" ways queue)
	(get-ways graph (remove-duplicates queue) ways end)
)

; ������� breadth-first ������ ����� �� ����� �����
(defun breadth-first (graph start end)
	(setq all_ways (get-ways (no-isolated-nodes graph) (get-children graph start) (list (list start)) end))
	(setq all_ways (get-needed-ways all_ways end))

	(setq min (CAR all_ways))
	(loop for x in all_ways do
		(if (< (get-path-weight graph x 0) (get-path-weight graph min 0)) (setq min x))
	)
	(if (= (length min) 1) (setq min (append min (list (CAR min))))) ; �������� ������� ���������� �������
	(setq best_way_length (get-path-weight graph min 0)) ; ��������� ������� ����� min
	(if (> best_way_length 0 ) (format t "best-way: ~S  len:~S~%" min best_way_length) (format t "No way found ~%"))
)