(in-package :cl-user)

(my-define-foreign-library cimgui
  (:darwin (asdf/system:system-relative-pathname :vktk "ifc/lib/cimgui.dylib"))
  (:windows (asdf/system:system-relative-pathname :vktk "ifc/lib/cimgui.dll"))
  (:linux (asdf/system:system-relative-pathname :vktk "ifc/lib/cimgui.so")))

(cffi:use-foreign-library cimgui)
