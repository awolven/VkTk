;; Copyright 2019 Andrew Kenneth Wolven <awolven@gmail.com>
;; 
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defsystem vktk
  :description "A Vulkan application toolkit for Common Lisp using Dear ImGui."
  :depends-on (:cl-vulkan)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :components
  ((:file "ifc/cimgui/imgui-package")
   (:file "ifc/cimgui/cimgui")
   (:file "ifc/cimgui/lisp-wrappers")
   (:file "src/package")
   (:file "src/imgui-class")   
   (:file "src/glfw-imgui")
   (:file "src/vulkan-imgui")
   (:file "src/general-imgui")
   (:file "examples/main")
   (:file "ifc/cimgui-library")))



	  

