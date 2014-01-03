(in-package :pathgen)

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

(defmacro make-extension (&body body)
  `(make-instance 'extension
		  :name ,(getf body :name)
		  :category ,(getf body :category)
		  :first-node first-node
		  :second-node second-node
		  :third-node third-node
		  :conditions #'(lambda ()
				  (and first-node second-node third-node
				       ,(make-conditions-form (getf body :conditions))))
		  :path-modifications #'(lambda () ,@(make-path-mods-form body))))

(defmacro make-linear-extension (&body body)
  (let ((extension-direction (getf body :extension-direction))
	(inv-extension-direction (invert-direction (getf body :extension-direction)))
	(original-direction (getf body :original-direction))
	(inv-original-direction (invert-direction (getf body :original-direction))))
    (destructuring-bind (node-1 node-2) (getf body :nodes)
      `(make-extension :name ,(getf body :name)
		       :category ,(getf body :category)
		       :conditions ((:is-free-node ,node-1 ,extension-direction)
				    (:is-free-node ,node-2 ,extension-direction))
		       :node-insertions ((:node ,node-1 :direction ,extension-direction 
						:prev ,inv-extension-direction :next ,original-direction)
					 (:node ,node-2 :direction ,extension-direction 
						:prev ,inv-original-direction :next ,inv-extension-direction))
		       :path-modifications ((:node ,node-1 :next ,extension-direction)
					    (:node ,node-2 :prev ,extension-direction))))))
(defmacro make-horizontal-right-extension (&body body)
  `(make-linear-extension :category :horizontal-right :original-direction :right ,@body))
(defmacro make-horizontal-left-extension (&body body)
  `(make-linear-extension :category :horizontal-left :original-direction :left ,@body))
(defmacro make-vertical-down-extension (&body body)
  `(make-linear-extension :category :vertical-down :original-direction :down ,@body))
(defmacro make-vertical-up-extension (&body body)
  `(make-linear-extension :category :vertical-up :original-direction :up ,@body))

(defmacro make-corner-extension (&body body)
  `(make-extension :category ,(getf body :name)
		   :name ,(getf body :name)
		   :conditions ((:is-free-node first-node ,(second (getf body :directions))))
		   :node-deletions ((:node second-node))
		   :node-insertions ((:node first-node :direction ,(second (getf body :directions))
					    :prev ,(invert-direction (second (getf body :directions)))
					    :next ,(first (getf body :directions))))
		   :path-modifications ((:node first-node :next ,(second (getf body :directions)))
					(:node third-node :prev ,(invert-direction (first (getf body :directions)))))))

