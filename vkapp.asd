(defsystem vkapp
  :description "Sample vulkan application using glfw and imgui."
  :depends-on (:cffi-libffi)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :components
  ((:file "ifc/library/library")
   (:file "ifc/cimgui/imgui-package")
   (:file "ifc/glfw/glfw-package")
   (:file "ifc/vulkan/vk-package")
   (:file "ifc/imgui-glfw-vulkan-package")
   (:file "src/vkapp-package")
   (:file "ifc/cimgui/cimgui")
   (:file "ifc/glfw/glfw")
   (:file "ifc/vulkan/vk-macros")
   (:file "ifc/vulkan/vk-types")
   (:file "ifc/vulkan/s-type-table")
   (:file "ifc/vulkan/vk-funcs")
   (:file "ifc/imgui-glfw-vulkan")
   (:file "src/vulkan-application")))

;; Hacks:
;; swig generated lisp files for cimgui and glfw have been post-modified
;; build-dll.bat and imgui-glfw-vulkan.def are necessary to build imgui-glfw-vulkan-1.dll
;; ~/libffi is a repo which contains libffi plus visual c solution files to build lib
;; output on vc14_x64 was changed from .lib to .dll
;; ffi.h of this repo has __declspec(dllexport) put in on necessary external symbols
;; probably want to change this back, and back to output .lib and then make a build.bat
;; script which takes the libffi.lib and makes a libffi.dll using additional custom .def file
;; the quicklisp sources for cffi-libffi were hacked to hardcode libffi.dll path and skip
;; grovelling in libffi-types.lisp.  groveller was built with msvcc and cmd command line:
;; cl /I %HOME%\libffi\include /I %HOME%\libffi\src\x86 /I %HOME%\quicklisp\dists\quicklisp\software\cffi_0.19.0 libffi-types__grovel.c
;; to make libffi-types__grovel.exe > libffi-types-pre-grovelled.lisp
;; and put in asdf file like this:
#|
(in-package :asdf)

(eval-when (:compile-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel)
  (asdf:oos 'asdf:load-op :trivial-features))

(defsystem cffi-libffi
  :description "Foreign structures by value"
  :author "Liam Healy <lhealy@common-lisp.net>"
  :maintainer "Liam Healy <lhealy@common-lisp.net>"
  :defsystem-depends-on (#:trivial-features #:cffi-grovel)
  :components
  ((:module libffi
    :serial t
    :components
    ((:file "libffi")
     ;;(cffi-grovel:grovel-file "libffi-types")
     (:file "libffi-types-pre-grovelled")
     (:file "libffi-functions")
     (:file "type-descriptors")
     (:file "funcall"))))
  :depends-on (#:cffi #:cffi-grovel #:trivial-features))
|#
;; because groveller is broken too (no gcc found by sbcl) and caused a debugger entry asdf loading this file
;; the purpose of cffi-libffi is to allow ImVec2 and ImVec4 structs to be called by value in cimgui.lisp
;; an example of this is:
;; (defcstruct test_struct (foo :double) (bar :double) (baz :double) (blah :long-long))
;; (cffi:defcfun test :int (arg (:struct test_struct)))
;; VKAPP> (test '(foo 1.0d0 bar 2.0d0 baz 3.0d0 blah 4))
;; 0
;; VKAPP> (test '(foo 2.0d0 bar 4.0d0 baz 6.0d0 blah 8))
;; 1
;; $ cat test.c
;; struct test_struct
;; {
;;   double foo;
;;   double bar;
;;   double baz;
;;   long blah;
;; };

;; __declspec(dllexport)
;; int test(struct test_struct arg) {
;;   if ( arg.foo == 2.0 && arg.bar == 4.0 && arg.baz == 6.0 && arg.blah == 8L )
;;     return 1;
;;   return 0;
;; }
;; build using cmd native x64 prompt:
;; cl test.c /LD
