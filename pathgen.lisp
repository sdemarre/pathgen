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

(defun neighbouring-node (node direction)
  (destructuring-bind (n-row n-column) (extend-in-direction (row node) (column node) direction)
    (grid-node (grid node) n-row n-column)))
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
	     (if (< (row node2) (row node3)) :left-down :left-up)))))
(defun find-possible-extensions (node)
  (let ((next-node (next-node node))
	(previous-node (previous-node node)))
    (when (and next-node previous-node)
      (case (three-node-category previous-node node next-node)
	(:horizontal-left)
	(:horizontal-right)
	(:vertical-up)
	(:vertical-down)
	(:down-righ)
	(:down-left)
	(:up-right)
	(:up-left)
	(:right-down)
	(:right-up)
	(:left-down)
	(:left-up)
	(otherwise (error "unknown three node category"))))))
(defun next-node (node)
  (let ((next-node-direction (direction node)))
    (when (not (eq :none next-node-direction))
      (neighbouring-node node next-node-direction))))
(defun previous-node (node)
  (let ((previous-node-direction (rev-direction node)))
    (when (not (eq :none previous-node-direction))
      (neighbouring-node node previous-node-direction))))
(defun try-to-extend-path (grid)
  (let ((node (find-random-node-on-path grid)))
   (let ((possible-extensions (find-possible-extensions node)))
     ())))


(defclass extension ()
  ((name :initarg :name :accessor name)
   (first-node :initarg :first-node :accessor first-node)
   (second-node :initarg :second-node :accessor second-node)
   (third-node :initarg :third-node :accessor third-node)
   (conditions :initarg :conditions :accessor conditions)
   (node-modifications :initarg :path-modifications :accessor node-modifications)))

(defun can-extend-in-direction-p (node direction)
  (position direction (possible-directions node)))
(defun node-available-p (node)
  (and (eq :none (direction node))
       (eq :none (rev-direction node)))))
(defun make-conditions-form (conditions)
  `(and ,@(iter (for (condition-type node-name direction) in conditions)
		(when (not (eq :is-free-node condition-type))
		  (error "unhandled condition type ~a" condition-type))
		(collect `(and
			   (can-extend-in-direction-p ,node-name ,direction)
			   (node-available-p (neighbouring-node ,node-name ,direction)))))))
(defun make-path-mods-form (specs)
  (declare (ignorable specs)))
(defmacro make-extension (&body body)
  `(make-instance 'extension
		  :name ,(getf body :name)
		  :first-node first-node
		  :second-node second-node
		  :third-node third-node
		  :conditions #'(lambda ()
				 (and first-node second-node third-node
				      ,(make-conditions-form (getf body :conditions))))
		  :path-modifications ,(make-path-mods-form body)))

(make-extension :name :vertical-down-a
		:category :vertical-down
		:conditions ((:is-free-node first-node :right)
			     (:is-free-node second-node :right))
		:node-insertions ((:node first-node :direction :right :prev :left :next :down)
				  (:node second-node :direction :right :prev :up :next :left))
		:path-modifications ((:node first-node :next :right)
				     (:node second-node :prev :right)))
(make-extension :name :vertical-down-b
		:category :vertical-down
		:conditions ((:is-free-node first-node :left)
			     (:is-free-node second-node :left))
		:node-insertions ((:node first-node :direction :left :prev :right :next :down)
				  (:node second-node :direction :left :prev :up :next :right))
		:path-modifications ((:node first-node :next :left)
				     (:node second-node :prev :left)))
(make-extension :name :vertical-down-c
		:category :vertical-down
		:conditions ((:is-free-node second-node :left)
			     (:is-free-node third-node :left))
		:node-insertions ((:node second-node :direction :left :prev :right :next :down)
				  (:node third-node :direction :left :prev :up :next :right))
		:path-modifications ((:node second-node :next :left)
				     (:node third-node :prev :left)))
(make-extension :name :vertical-down-d
		:category :vertical-down
		:conditions ((:is-free-node second-node :right)
			     (:is-free-node third-node :right))
		:node-insertions ((:node second-node :direction :right :prev :left :next :down)
				  (:node third-node :direction :right :prev :up :next :left))
		:path-modifications ((:node second-node :next :right)
				     (:node third-node :prev :right)))

(make-extension :name :vertical-up-a
		:category :vertical-up
		:conditions ((:is-free-node second-node :right)
			     (:is-free-node third-node :right))
		:node-insertions ((:node second-node :direction :right :prev :left :next :up)
				  (:node third-node :direction :right :prev :down :next :left))
		:path-modifications ((:node second-node :next :right)
				     (:node third-node :prev :right)))
(make-extension :name :vertical-up-b
		:category :vertical-up
		:conditions ((:is-free-node second-node :left)
			     (:is-free-node third-node :left))
		:node-insertions ((:node second-node :direction :left :prev :right :next :up)
				  (:node third-node :direction :left :prev :down :next :right))
		:path-modifications ((:node second-node :next :left)
				     (:node third-node :prev :left)))
(make-extension :name :vertical-up-c
		:category :vertical-up
		:conditions ((:is-free-node first-node :left)
			     (:is-free-node second-node :left))
		:node-insertions ((:node first-node :direction :left :prev :right :next :up)
				  (:node second-node :direction :left :prev :down :next :right))
		:path-modifications ((:node first-node :next :left)
				     (:node second-node :prev :left)))
(make-extension :name :vertical-up-d
		:category :vertical-up
		:conditions ((:is-free-node first-node :right)
			     (:is-free-node second-node :right))
		:node-insertions ((:node first-node :direction :right :prev :left :next :up)
				  (:node second-node :direction :right :prev :down :next :left))
		:path-modifications ((:node first-node :next :right)
				     (:node second-node :prev :right)))



(make-extension :name :horizontal-right-a
		:category :horizontal-right
		:conditions ((:is-free-node first-node :up)
			     (:is-free-node second-node :up))
		:node-insertions ((:node first-node :direction :up :prev :down :next :right)
				  (:node second-node :direction :up :prev :left :next :down))
		:path-modifications ((:node first-node :next :up)
				     (:node second-node :prev :up)))
(make-extension :name :horizontal-right-b
		:category :horizontal-right
		:conditions ((:is-free-node second-node :up)
			     (:is-free-node third-node :up))
		:node-insertions ((:node second-node :direction :up :prev :down :next :right)
				  (:node third-node :direction :up :prev :left :next :down))
		:path-modifications ((:node second-node :next :up)
				     (:node third-node :prev :up)))
(make-extension :name :horizontal-right-c
		:category :horizontal-right
		:conditions ((:is-free-node first-node :down)
			     (:is-free-node second-node :down))
		:node-insertions ((:node first-node :direction :down :prev :up :next :right)
				  (:node second-node :direction :down :prev :left :next :up))
		:path-modifications ((:node first-node :next :down)
				     (:node second-node :prev :left)))
(make-extension :name :horizontal-right-d
		:category :horizontal-right
		:conditions ((:is-free-node second-node :down)
			     (:is-free-node third-node :down))
		:node-insertions ((:node second-node :direction :down :prev :up :next :right)
				  (:node third-node :direction :down :prev :left :next :up))
		:path-modifications ((:node second-node :next :right)
				     (:node third-node :prev :left)))

(make-extension :name :horizontal-left-a
		:category :horizontal-left
		:conditions ((:is-free-node second-node :up)
			     (:is-free-node third-node :up))
		:node-insertions ((:node second-node :direction :up :prev :right :next :up)
				  (:node third-node :direction :up :prev :right :next :down))
		:path-modifications ((:node second-node :next :up)
				     (:node third-node :prev :up)))
(make-extension :name :horizontal-left-b
		:category :horizontal-left
		:conditions ((:is-free-node first-node :up)
			     (:is-free-node second-node :up))
		:node-insertions ((:node first-node :direction :up :prev :down :next :left)
				  (:node second-node :direction :up :prev :right :next :down))
		:path-modifications ((:node first-node :next :up)
				     (:node second-node :prev :up)))
(make-extension :name :horizontal-left-c
		:category :horizontal-left
		:conditions ((:is-free-node second-node :down)
			     (:is-free-node third-node :down))
		:node-insertions ((:node second-node :direction :down :prev :up :next :left)
				  (:node third-node :direction :down :prev :right :next :up))
		:path-modifications ((:node second-node :next :down)
				     (:node third-node :prev :down)))
(make-extension :name :horizontal-left-d
		:category :horizontal-left
		:conditions ((:is-free-node first-node :down)
			     (:is-free-node second-node :down))
		:node-insertions ((:node first-node :direction :down :prev :up :next :left)
				  (:node second-node :direction :down :prev :right :next :up))
		:path-modifications ((:node first-node :next :down)
				     (:node second-node :prev :down)))


(make-extension :name :down-right
		:category :down-right
		:conditions ((:is-free-node first-node :right))
		:node-deletions ((:node second-node))
		:node-insertions ((:node first-node :direction :right :prev :left :next :down))
		:path-modifications ((:node first-node :next :right)
				     (:node third-node :prev :up)))

(make-extension :name :up-right
		:category :up-right
		:conditions ((:is-free-node first-node :right))
		:node-deletions ((:node second-node))
		:node-insertions ((:node first-node :direction :right :prev :left :next :up))
		:path-modifications ((:node first-node :next :right)
				     (:node third-node :prev :down)))
(make-extension :name :up-left
		:category :up-left
		:conditions ((:is-free-node first-node :left))
		:node-deletions ((:node second-node))
		:node-insertions ((:node first-node :direction :left :prev :right :next :up))
		:path-modifications ((:node first-node :next :left)
				     (:node third-node :prev :down)))

(make-extension :name :down-left
		:category :down-left
		:conditions ((:is-free-node first-node :left))
		:node-deletions ((:node second-node))
		:node-insertions ((:node first-node :direction :left :prev :right :next :down))
		:path-modifications ((:node first-node :next :left)
				     (:node third-node :prev :up)))

(make-extension :name :right-up
		:category :right-up
		:conditions ((:is-free-node first-node :up))
		:node-deletions ((:node second-node))
		:node-insertions ((:node first-node :direction :up :prev :down :next :right))
		:path-modifications ((:node first-node :next :up)
				     (:node third-node :prev :left)))

(make-extension :name :right-down
		:category :right-down
		:conditions ((:is-free-node first-node :down))
		:node-deletions ((:node second-node))
		:node-insertions ((:node first-node :direction :down :prev :up :next :right))
		:path-modifications ((:node first-node :next :down)
				     (:node third-node :prev :left)))

(make-extension :name :left-up
		:category :left-up
		:conditions ((:is-free-node first-node :up))
		:node-deletions ((:node second-node))
		:node-insertions ((:node first-node :direction :up :prev :down :next :left))
		:path-modifications ((:node first-node :next :up)
				     (:node third-node :prev :left)))

(make-extension :name :left-down
		:category :left-down
		:conditions ((:is-free-node first-node :down))
		:node-deletions ((:node second-node))
		:node-insertions ((:node first-node :direction :down :prev :up :next :left))
		:path-modifications ((:node first-node :next :down)
				     (:node third-node :prev :right)))
