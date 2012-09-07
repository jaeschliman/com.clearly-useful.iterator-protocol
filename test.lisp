(defpackage #:com.clearly-useful.iterator-protocol.test
  (:use #:cl
	#:com.clearly-useful.sequence-protocol
	#:com.clearly-useful.iterator-protocol))

(in-package #:com.clearly-useful.iterator-protocol.test)

(defun test-builtin-iterator (seq)
  (assert (typep seq 'seq))
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
      (assert (equalp x (head s)))
      (setf s (tail s)))))

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

