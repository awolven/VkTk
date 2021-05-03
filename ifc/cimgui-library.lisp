(in-package :cl-user)

(defmacro my-define-foreign-library (name-and-options &body pairs)
  `(cffi:define-foreign-library ,name-and-options
     ,@(mapcar (lambda (pair)
		 (list (first pair) (eval (second pair))))
	       pairs)))

(my-define-foreign-library cimgui
  (:darwin (asdf/system:system-relative-pathname :vktk "ifc/lib/cimgui.dylib"))
  (:windows (asdf/system:system-relative-pathname :vktk "ifc/lib/cimgui.dll"))
  (:linux (asdf/system:system-relative-pathname :vktk "ifc/lib/cimgui.so")))

(cffi:use-foreign-library cimgui)
