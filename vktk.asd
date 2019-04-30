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
  :description "A Vulkan application toolkit using GLFW and ImGui."
  :depends-on (:cffi :3d-matrices :lisp-unit)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :components
  ((:file "ifc/foreign-libraries")
   (:file "ifc/cimgui/imgui-package")
   (:file "ifc/glfw/glfw-package")
   (:file "ifc/vulkan/vk-package")
   (:file "src/vktk-package")
   (:file "ifc/cimgui/cimgui")
   (:file "ifc/glfw/glfw")
   (:file "ifc/vulkan/vk-types")
   (:file "ifc/vulkan/s-type-table")
   (:file "ifc/vulkan/vk-macros")
   (:file "ifc/vulkan/vk-funcs")
   (:file "src/macros")
   (:file "src/support")
   (:file "src/vulkan-toolkit")
   (:file "ifc/imgui-glfw-vulkan")
   (:file "src/vktk-demo")))
	  

