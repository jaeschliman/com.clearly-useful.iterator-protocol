(defpackage #:com.clearly-useful.iterator-protocol.test
  (:use #:cl
	#:com.clearly-useful.generic-collection-interface
	#:com.clearly-useful.iterator-protocol))

(in-package #:com.clearly-useful.iterator-protocol.test)

(defun test-builtin-iterator (seq)
  "confirm that it works as is, and after seq."
  (assert (typep seq 'seqable))
  (let ((it (iterator seq)))
    (doseq (o seq)
      (multiple-value-bind (val continue) (iterator-next! it)
	(assert (equalp o val))
	(assert continue)))
    (iterator-next! it)
    (iterator-finish! it)
    (assert (null (nth-value 1 (iterator-next! it)))))
  (let ((s seq))
    (do-iterator (x seq)
      (assert (equalp x (fst s)))
      (setf s (rst s))))
  (unless (typep seq 'seq)
    (test-builtin-iterator (seq seq))))

(assert (com.clearly-useful.iterator-protocol::%seq-iterator-p
	 (iterator '(a b c))))
(assert (com.clearly-useful.iterator-protocol::%vector-iterator-p
	 (iterator #(a b c))))

(mapcar #'test-builtin-iterator
	'(;;cons
	  (a b c)
	  ;;vector
	  "abc"
	  ;;vector again
	  #(a b c)
	  ;;array
	  #2A((a b) (c d))
	  ;;mixed
	  (a . #(b c))
	  ))

