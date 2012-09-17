;;;; com.clearly-useful.iterator-protocol.asd

(asdf:defsystem #:com.clearly-useful.iterator-protocol
  :serial t
  :description "A simple iterator protocol for Common Lisp."
  :author "Jason Aeschliman <j.aeschliman@gmail.com>"
  :license "revised BSD"
  :version "0.1"
  :depends-on (#:com.clearly-useful.protocols
               #:com.clearly-useful.generic-collection-interface)
  :components ((:file "package")
               (:file "com.clearly-useful.iterator-protocol")))

