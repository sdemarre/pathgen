;;;; pathgen.lisp

(in-package #:pathgen)

;;; "pathgen" goes here. Hacks and glory await!

(defclass path-node ()
  ((row :initarg :row :initform 0 :accessor row)
   (column :initarg :column :initform 0 :accessor column)
   (direction :initarg :direction :initform :none :accessor direction)
   (rev-direction :initarg :direction :initform :none :accessor rev-direction)
   (grid :initarg :grid :accessor grid)))
(defmethod print-object ((path-node path-node) stream)
  (print-unreadable-object (path-node stream :type t)
    (format stream ":row ~a :column ~a :dir ~a :rev-dir ~a"
	    (row path-node)
	    (column path-node)
	    (direction-rep (direction path-node))
	    (direction-rep (rev-direction path-node)))))


(defclass grid ()
  ((width :initarg :width :initform 10 :accessor width)
   (height :initarg :height :initform 10 :accessor height)
   (grid-nodes :initform nil :accessor grid-nodes)))

(defmacro for-all-rows ((grid row) &body body)
  `(iter (for ,row :from 0 :below (height ,grid))
	 ,@body))
(defmacro for-all-columns ((grid column) &body body)
  `(iter (for ,column :from 0 :below (width ,grid))
	 ,@body))
(defmacro for-all-nodes ((grid node) &body body)
  (let ((row (gensym "row"))
	(column (gensym "column")))
    `(for-all-rows (,grid ,row)
       (for-all-columns (,grid ,column)
	 (let ((,node (grid-node ,grid ,row ,column)))
	   ,@body)))))

(defmethod grid-node ((grid grid) row column)
  (aref (grid-nodes grid) row column))
(defsetf grid-node (grid row column) (new-value)
  `(setf (aref (grid-nodes ,grid) ,row ,column) ,new-value))

(defmethod initialize-instance :after ((grid grid) &key)
  (setf (grid-nodes grid) (make-array (list (height grid) (width grid))))
  (for-all-rows (grid row)
    (for-all-columns (grid column)
      (setf (grid-node grid row column) (make-instance 'path-node :row row :column column :grid grid)))))

(defmethod possible-directions ((path-node path-node))
  (let ((row (row path-node))
	(column (column path-node))
	(grid (grid path-node)))
   (cond ((= 0 row) (cond ((= 0 column) '(:right :down))
			  ((= (1- (width grid)) column) '(:left :down))
			  (t  '(:right :down :left))))
	 ((= (1- (height grid)) row) (cond ((= 0 column) '(:right :up))
					   ((= (1- (width grid)) column) '(:left :up))
					   (t '(:right :up :left))))
	 ((= 0 column) '(:up :down :right))
	 ((= (1- (width grid)) column) '(:up :down :left))
	 (t '(:up :down :left :right)))))

(defun invert-direction (direction)
  (case direction
    (:up :down)
    (:left :right)
    (:right :left)
    (:down :up)
    (otherwise :none)))
(defun init-rev-grid-directions (grid)
  (for-all-rows (grid row)
    (for-all-columns (grid column)
      (let ((direction (direction (grid-node grid row column))))
       (when (not (eq :none direction))
	 (destructuring-bind (erow ecolumn) (extend-in-direction row column direction)
	   (setf (rev-direction (grid-node grid erow ecolumn)) (invert-direction direction))))))))
(defun initialize-initial-path (grid)
  (for-all-rows (grid row)
    (for-all-columns (grid column)
      (setf (direction (grid-node grid row column)) :none)))
  (for-all-rows (grid row)
    (setf (direction (grid-node grid row 0)) :down))
  (for-all-columns (grid column)
    (setf (direction (grid-node grid (1- (height grid)) column)) :right))
  (setf (direction (grid-node grid (1- (height grid)) (1- (width grid)))) :none)
  (init-rev-grid-directions grid)
  grid)


(defun direction-rep (direction)
  (case direction
    (:up "^")
    (:down "v")
    (:left "<")
    (:right ">")
    (otherwise ".")))
(defmethod print-object ((grid grid) stream)
  (print-unreadable-object (grid stream :type t)
    (for-all-rows (grid row)
      (format stream "~%")
      (for-all-columns (grid column)
	(let ((node (grid-node grid row column)))
	 (format stream "[~a|~a] " 
		 (direction-rep (direction node))
		 (direction-rep (rev-direction node))))))))

(defun extend-in-direction (row column direction)
  (case direction
    (:left (list row (1- column)))
    (:right (list row (1+ column)))
    (:up (list (1- row) column))
    (:down (list (1+ row) column))))
(defun find-random-node-on-path (grid)
  (let ((row (random (height grid)))
	(column (random (width grid))))
    (iter (until (not (eq :none (direction (grid-node grid row column)))))
	  (setf row (random (height grid)))
	  (setf column (random (width grid))))
    (grid-node grid row column)))
(defun three-node-category (node1 node2 node3)
  (cond ((= (row node1) (row node2) (row node3)) 
	 (if (< (column node1) (column node2)) :horizontal-right :horizontal-left))
	((= (column node1) (column node2) (column node3)) 
	 (if (< (row node1) (row node2)) :vertical-down :vertical-up))
	((and (= (column node1) (column node2)) (= (row node2) (row node3)))
	 (if (< (row node1) (row node2))
	     (if (< (column node2) (column node3)) :down-right :down-left)
	     (if (< (column node2) (column node3)) :up-right :up-left)))
	((and (= (row node1) (row node2)) (= (column node2) (column node3)))
	 (if (< (column node1) (column node2))
	     (if (< (row node2) (row node3)) :right-down :right-up)
	     (if (< (row node2) (row node3)) :left-down :left-up)))))))
(defun find-possible-extensions (node)
     (let ((directions (possible-directions node)))
       (iter (for direction in directions)
	     (destructuring-bind (erow ecolumn) (extend-in-direction row column direction)
	       (let ((next-node (next-node node))
		     (previous-node (previous-node node)))
		 (when (and next-node previous-node)
		   (case (three-node-category previous-node node next-node)
		     (:horizontal)
		     (:vertical)
		     (:l-0)
		     (:l-90)
		     (:l-180)
		     (:l-270))))))))
(defun next-node (node)
  (let ((next-node-direction (direction node)))
    (when (not (eq :none next-node-direction))
      (destructuring-bind (row column) (extend-in-direction (row node) (column node) next-node-direction)
	(grid-node (grid node) row column)))))
(defun previous-node (node)
  (let ((previous-node-direction (rev-direction node)))
    (when (not (eq :none previous-node-direction))
      (destructuring-bind (row column) (extend-in-direction (row node) (column node) previous-node-direction)
	(grid-node (grid node) row column)))))
(defun try-to-extend-path (grid)
  (let ((node (find-random-node-on-path grid)))
   (let ((possible-extensions (find-possible-extensions node)))
     ())))
