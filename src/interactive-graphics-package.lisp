(in-package :cl-user)

(defpackage :igp
  (:use #:cl
	#:vktk
	#:oc
	#:gp
	#:3d-matrices
	#:3d-vectors)
  (:shadowing-import-from #:3d-matrices #:mat)
  (:shadowing-import-from #:3d-vectors #:vec))
  
