
(defclass gaussiandb () ((mean :initform nil
			       :initarg :mean
			       :accessor gaussiandb-mean)
			 (var :initform nil
			      :initarg :var
			      :accessor gaussiandb-var)))

(defun double-random ()
  (let ((i (random 1.0)))
    (if (eq i 0.0)
	(setq i (double-random))) i))

(defmethod gaussiandb-random ((gs gaussiandb))
  (let* ((r (double-random))
	 (c (sqrt (* -2 (log r)))))
    (if (< (double-random) 0.5)
	(+    (* c
	      (sin (* 2.0 pi (double-random)))
	      (gaussiandb-var gs))
	      
	      (gaussiandb-mean gs))
	(+    (* c
	      (cos (* 2.0 pi (double-random)))
	      (gaussiandb-var gs))
	      (gaussiandb-mean gs)))))
