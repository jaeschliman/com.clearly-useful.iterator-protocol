;;;; package.lisp

(defpackage #:com.clearly-useful.iterator-protocol
  (:use #:cl
	#:com.clearly-useful.protocols
	#:com.clearly-useful.sequence-protocol)
  (:export
   #:iterator
   #:iterator-p
   #:iterator-next!
   #:iterator-finish!
   #:do-iterator))

