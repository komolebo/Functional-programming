(setq circle '(par (L 20) (seq (L 4) (par (L 15) (C 15e-6) (C 0.002) ))))
(setq omega 0.01)

(defun R (res)
	(list res 0.0)
)

(defun L (ind)
	(list 0.0 (* omega ind))
)

(defun C (cap)
	(list 0.0 (/ -1.0 (* omega cap)))
)

(defun KPLUS (knums)
	(SUM 0.0 0.0 (CAR knums) (CDR knums))
)

(defun SUM (r-part i-part num rest)
	(if (NULL num) (return-from SUM (list r-part i-part)))
	(SUM (+ r-part (CAR num)) (+ i-part (CADR num)) (CAR rest) (CDR rest))
)

(defun SEQ(&REST elements)
	(KPLUS elements)
)

(defun LINVERSE (lz)
	(COND ((NULL lz) nil)
	(t (CONS (INVERSE (CAR lz)) (LINVERSE (CDR lz))))
	)
)

(defun INVERSE (num)
	(LET* ((a (CAR num)) (b (CADR num)) (s (+ (* a a) (* b b))))
	(list (/ a s) (* b (/ 1.0 s)))
	)
)

(defun PAR (&REST lz)
	(INVERSE (kplus (LINVERSE lz)))
)

