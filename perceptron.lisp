
(load "gaussiandb.lisp")
(use-package :utils)


(defclass pmodel ()
  ((trainN :initform 1000
	   :initarg :trainn
	   :accessor pmodel-trainN)
   (testN :initform 200
	  :initarg :testn
	  :accessor pmodel-testN)
   (unitN :initform 2
	  :initarg :unitn
	  :accessor pmodel-unitN)
   (epochN :initform 2000
	   :initarg :epochn
	   :accessor pmodel-epochN)
   (learning-late :initform 1.0
		  :initarg :ll
		  :accessor pmodel-learning-late)
   (train-datas) (train-labels) (test-datas) (test-labels) (predicted-data)
   (w :initform nil
      :accessor pmodel-w)))

(defun make-gtrain-data (x y g1 g2)
  (let ((data (make-array `(,x ,y)))
	(rgns (list g1 g2)))
    (dotimes (c x)
      (dotimes (layerN y)
	(setf (aref data c layerN)
	      (gaussiandb-random (nth (mod layerN 2) rgns)))))
    data))

(defun make-glabel-data (n l)
  (make-array n :initial-element l))

; 学習の評価と、epoch scoreの値を使うようにしたい。
; iが2/n以下はg1で2/n以上はg2で学習データ作りたい

(defmethod pmodel-train ((model pmodel))
  (let* ((g1 (make-instance 'gaussiandb :mean -2.0 :var 1.0))
	 (g2 (make-instance 'gaussiandb :mean 2.0 :var 1.0))
 	 (train-datas
	   (make-gtrain-data (pmodel-trainN model) (pmodel-unitN model) g1 g2))
	 (train-labels (make-glabel-data (pmodel-trainN model) 1))
	 (test-datas (make-gtrain-data (pmodel-testN model)
				       (pmodel-unitN model) g1 g2))
 	 (test-labels (make-glabel-data (pmodel-unitN model) 1))
	 (score 0))

    (setf (pmodel-w model)
	  (make-array (pmodel-unitn model)))
    
    (dotimes (k (pmodel-epochN model))
      (dotimes (i (pmodel-trainN model))
	(setq score (+ score (pmodel-forward-epoch model
						   i
					           train-datas
						   train-labels
						   (pmodel-learning-late model))))))))

(defun pmodel-forward-epoch (model iter x l rate)
  (let ((w (slot-value model 'w)) ; 副作用出る？
	(c 0.0)
	(unitn (pmodel-unitn model)))
    (dotimes (i unitn)
      (setq c (+ c (* (aref w i) (aref x iter i) (aref l iter)))))
    (if (> c 0)
	1
	(progn
	  (dotimes (i unitn)
	    (setf (aref w i) (+ (aref w i) (* rate (aref x iter i) (aref l iter)))))
	  0))))

(defun pstep (x) (if (>= x 0) 1 -1))

(defun pmodel-predict (model input)
  (let ((act 0)
	(w (slot-value model 'w)))
    (dotimes (i (pmodel-unitn model))
      (setq act (+ act (aref w i) (aref input i))))
    (pstep act)))
