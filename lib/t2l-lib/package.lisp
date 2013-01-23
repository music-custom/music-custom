(in-package :cl-user)
(screamer:define-screamer-package :t2l (:use :ompw :system))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (import '(ccl::PWGLdef) :t2l))