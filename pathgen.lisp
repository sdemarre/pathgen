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
  (let ((third-node (next-node node))
	(first-node (previous-node node))
	(second-node node))
    (when (and first-node third-node)
      (case (three-node-category first-node second-node third-node)
	(:horizontal-left (list (make-horizontal-left-extension
				  :name :horizontal-left-a
				  :extension-direction :up
				  :nodes (first-node second-node))
				(make-horizontal-left-extension
				  :name :horizontal-left-b
				  :extension-direction :up
				  :nodes (second-node third-node))
				(make-horizontal-left-extension
				  :name :horizontal-left-c
				  :extension-direction :down
				  :nodes (first-node second-node))
				(make-horizontal-left-extension
				  :name :horizontal-left-d
				  :extension-direction :down
				  :nodes (second-node third-node))))
	(:horizontal-right (list (make-horizontal-right-extension
				   :name :horizontal-right-a
				   :extension-direction :up
				   :nodes (first-node second-node))
				 (make-horizontal-right-extension
				   :name :horizontal-right-b
				   :extension-direction :up
				   :nodes (second-node third-node))
				 (make-horizontal-right-extension
				   :name :horizontal-right-c
				   :extension-direction :down
				   :nodes (first-node second-node))
				 (make-horizontal-right-extension
				   :name :horizontal-right-d
				   :extension-direction :down
				   :nodes (second-node third-node))))
	(:vertical-up (list (make-vertical-up-extension
			      :name :vertical-up-a
			      :extension-direction :left
			      :nodes (first-node second-node))
			    (make-vertical-up-extension
			      :name :vertical-up-b
			      :extension-direction :left
			      :nodes (second-node third-node))
			    (make-vertical-up-extension
			      :name :vertical-up-c
			      :extension-direction :right
			      :nodes (first-node second-node))
			    (make-vertical-up-extension
			      :name :vertical-up-d
			      :extension-direction :right
			      :nodes (second-node third-node))))
	(:vertical-down (list (make-vertical-down-extension
				:name :vertical-down-a
				:extension-direction :left
				:nodes (first-node second-node))
			      (make-vertical-down-extension
				:name :vertical-down-b
				:extension-direction :left
				:nodes (second-node third-node))
			      (make-vertical-down-extension
				:name :vertical-down-c
				:extension-direction :right
				:nodes (first-node second-node))
			      (make-vertical-down-extension
				:name :vertical-down-d
				:extension-direction :right
				:nodes (second-node third-node))))
	(:down-right (list (make-corner-extension :name :down-right :directions (:down :right))))
	(:down-left (list (make-corner-extension :name :down-left :directions (:down :left))))
	(:up-right (list (make-corner-extension :name :up-right :directions (:up :right))))
	(:up-left (list (make-corner-extension :name :up-left :directions (:up :left))))
	(:right-down (list (make-corner-extension :name :right-down :directions (:right :down))))
	(:right-up (list (make-corner-extension :name :right-up :directions (:right :up))))
	(:left-down (list (make-corner-extension :name :left-down :directions (:left :down))))
	(:left-up (list (make-corner-extension :name :left-up :directions (:left :up))))
	(otherwise (error "unknown three second-node category"))))))
(defun next-node (node)
  (let ((next-node-direction (direction node)))
    (when (not (eq :none next-node-direction))
      (neighbouring-node node next-node-direction))))
(defun previous-node (node)
  (let ((previous-node-direction (rev-direction node)))
    (when (not (eq :none previous-node-direction))
      (neighbouring-node node previous-node-direction))))

(defclass extension ()
  ((name :initarg :name :accessor name)
   (category :initarg :category :accessor category)
   (first-node :initarg :first-node :accessor first-node)
   (second-node :initarg :second-node :accessor second-node)
   (third-node :initarg :third-node :accessor third-node)
   (conditions :initarg :conditions :accessor conditions)
   (node-modifications :initarg :path-modifications :accessor node-modifications)))

(defun can-extend-in-direction-p (node direction)
  (position direction (possible-directions node)))
(defun node-available-p (node)
  (and (eq :none (direction node))
       (eq :none (rev-direction node))))

(defun do-random-extension (grid)
  (let ((node (find-random-node-on-path grid)))
    (when node
      (let ((candidate-extensions (find-possible-extensions node)))
	(when candidate-extensions
	  (let ((possible-extensions (remove-if-not #'(lambda (extension) (funcall (conditions extension))) candidate-extensions)))
	    (when possible-extensions
	      (let ((extension (elt possible-extensions (random (length possible-extensions)))))
		(funcall (node-modifications extension))))))))))

(defun path-length (grid)
  (let ((node (grid-node grid 0 0)))
    (iter (while (not (eq :none (direction node))))
	  (sum 1)
	  (setf node (next-node node)))))

(defun path-has-loop-p (grid)
  (let ((node (grid-node grid 0 0))
	(length 0))
    (iter (while (and (not (eq :none (direction node)))
		      (< length (1+ (* (width grid) (height grid))))))
	  (incf length)
	  (setf node (next-node node)))
    (> length (* (width grid) (height grid)))))


(defun copy-grid (grid)
  (let ((new-grid (make-instance 'grid :width (width grid) :height (height grid))))
    (for-all-nodes (grid node)
      (setf (direction (grid-node new-grid (row node) (column node))) (direction node))
      (setf (rev-direction (grid-node new-grid (row node) (column node))) (rev-direction node)))
    new-grid))

(defun make-ps-output (grid stream)
  (format stream "%!
/cm {1 2.54 div 72 mul} def
/mm {0.1 cm mul} def
/slength {0.5 mm mul} def
/r {slength 0 rlineto} def
/l {slength -1 mul 0 rlineto} def
/d {0 slength rlineto} def
/u {0 -1 slength mul rlineto} def
newpath
0.5 cm mul dup moveto
")
  (let ((node (grid-node grid 0 0))
	(cmd-count 0))
    (iter (while (not (eq :none (direction node))))
	  (case (direction node)
	    (:left (format stream "l "))
	    (:right (format stream "r "))
	    (:up (format stream "u "))
	    (:down (format stream "d ")))
	  (incf cmd-count)
	  (when (> cmd-count 50)
	    (setf cmd-count 0)
	    (format stream "~%"))
	  (setf node (next-node node))))
  (format stream "stroke
showpage~%"))

(defun make-ps-file (ps-file-name grid)
  (with-open-file (s ps-file-name :direction :output :if-exists :supersede)
    (make-ps-output grid s)))
(defun max-path-length (grid)
  (* (width grid) (height grid)))
