(in-package :cl-user)

(defpackage :igp
  (:use #:cl
	#:cffi
	#:3d-matrices
	#:3d-vectors
	#:vktk)
  (:shadowing-import-from #:3d-matrices #:mat)
  (:shadowing-import-from #:3d-vectors #:vec)
  (:export #:run-demo))
  
