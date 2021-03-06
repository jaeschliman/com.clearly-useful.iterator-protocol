;;;; com.clearly-useful.iterator-protocol.lisp

(in-package #:com.clearly-useful.iterator-protocol)

(defprotocol iterator
  "A simple iteration protocol for Common Lisp.
Unlike sequences, iterators are stateful, and are
not intended to be held onto or passed around. Their
use should be hidden by some other construct. If you
are explicitly creating and advancing an iterator,
chances are you are Doing It Wrong.
  Iterators should be be treated as though they
have dynamic extent.
"
  (:eponymous-method t)
  
  (iterator-next! (it) "returns two values:
  1. the next value of the iterator or nil,
  2. t or nil depending depending whether a value was found. 
then advances the iterator.")
  
  (iterator-finish! (it) "performs 'clean up' operations specific
to the iterator. e.g. closing a file."))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct %seq-iterator seq)
  (defstruct %vector-iterator size vec pos)
  (defstruct %indexable-iterator size idx pos))

(defun %value (o) (head (%seq-iterator-seq o)))
(defun %has-value (o) (%seq-iterator-seq o))
(defun %advance (o) (prog1 o
		      (setf (%seq-iterator-seq o)
			    (tail (%seq-iterator-seq o)))))


(extend-type %seq-iterator     
  iterator
  (iterator-next! (it)
		  (let* ((has-value (%has-value it))
			 (value (and has-value
				     (%value it))))
		    (%advance it)
		    (values value has-value)))
  (iterator-finish! (it)
		    (declare (ignore it))))


(extend-type %vector-iterator
  iterator
  (iterator-next! (it)
		  (if (< (%vector-iterator-pos it)
			 (%vector-iterator-size it))
		      (let ((n (%vector-iterator-pos it)))
			(incf (%vector-iterator-pos it))
			(values (aref (%vector-iterator-vec it) n)
				t))
		      (values nil nil)))
  (iterator-finish! (it)
		    (declare (ignore it))))

(extend-type %indexable-iterator
  iterator
  (iterator-next! (it)
		  (if (< (%indexable-iterator-pos it)
			 (%indexable-iterator-size it))
		      (let ((n (%indexable-iterator-pos it)))
			(incf (%indexable-iterator-pos it))
			(values (element-at (%indexable-iterator-size it) n)
				t))
		      (values nil nil)))
  (iterator-finish! (it)
		     (declare (ignore it))))


(defmethod iterator (object)
  (etypecase object
    (indexable (if (counted-p object)
		   ;;using indexable only makes sense
		   ;;for constant-time access
		   (make-%indexable-iterator :idx object
					     :size (count-elements object)
					     :pos 0)
		   (make-%seq-iterator :seq (seq object))))
    (seq (make-%seq-iterator :seq object))
    (associative (make-%seq-iterator :seq (seq object)))
    (t (error "No method to convert ~S to ~S" object 'iterator))))


(defmethod iterator ((a vector))
  (make-%vector-iterator :vec a
			 :pos 0
			 :size (length a)))

(defmethod iterator ((an array))
  (let ((size (array-total-size an)))
    (make-%vector-iterator :vec (make-array size
					    :displaced-to an
					    :displaced-index-offset 0
					    :element-type
					    (array-element-type an))
			   :pos 0
			   :size size)))


(defmethod iterator ((a hash-table))
  (iterator (seq a)))

(defmacro do-iterator ((var form &optional return-form) &body body)
  (let ((it (gensym))
	(val (gensym))
	(continue (gensym)))
    `(block nil
       (let ((,it (iterator ,form)))
	 (unwind-protect
	      (loop named ,(gensym)
		 for (,val ,continue) =
		   (multiple-value-list (iterator-next! ,it))
		 while ,continue
		 for ,var = ,val
		 do (progn ,@body)
		   ,@(when return-form
			   `(finally (return ,return-form))))
	  	   (iterator-finish! ,it))))))
