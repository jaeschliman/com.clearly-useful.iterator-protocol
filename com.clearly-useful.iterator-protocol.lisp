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
  (iterator-next! (it) "returns two values:
  1. the next value of the iterator or nil,
  2. t or nil depending depending whether a value was found. 
then advances the iterator.")
  (iterator-finish! (it) "performs 'clean up' operations specific
to the iterator. e.g. closing a file."))


(defstruct %seq-iterator seq)

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

(defstruct %vector-iterator size vec pos)
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

(defgeneric iterator (object)
  (:documentation "produce an iterator from object.
acts as the identity function for iterators,
and provides a default implementation for seqs.")
  (:method (object)
    (etypecase object
      (iterator object)
      (seq (make-%seq-iterator :seq object))
      (t (error "No method to convert ~A to an ITERATOR" object)))))

(defmethod iterator ((a vector))
  (make-%vector-iterator :vec a
			 :pos 0
			 :size (length a)))

(defmethod iterator ((an array))
  (let ((size (array-total-size an)))
    (make-%vector-iterator :vec (make-array size
					    :displaced-to an
					    :displaced-index-offset 0)
			   :pos 0
			   :size size)))


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
