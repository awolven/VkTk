;; Copyright 2019 Andrew Kenneth Wolven
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

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
(defmacro my-define-foreign-library (name-and-options &body pairs)
  `(cffi:define-foreign-library ,name-and-options
     ,@(mapcar (lambda (pair)
		 (list (first pair) (eval (second pair))))
	       pairs)))

(defparameter *home-dir* #+darwin "/Users/awolven" #+windows "C:/Users/awolven")

(defparameter *vktk-dir* (concatenate 'string *home-dir* "/vktk"))

(defparameter *vulkan-sdk-path*
  #+darwin (concatenate 'string *home-dir* "/vulkansdk-macos-1.1.106.0/macOS")
  #+windows "C:/VulkanSDK/1.1.97.0")
)

#+darwin
(progn
  (sb-posix:setenv "VULKAN_SDK" *vulkan-sdk-path* 0)
  (sb-posix:setenv "VK_ICD_FILENAMES" (concatenate 'string *vulkan-sdk-path* "/etc/vulkan/icd.d/MoltenVK_icd.json") 0)
  (sb-posix:setenv "VK_LAYER_PATH" (concatenate 'string *vulkan-sdk-path* "/etc/vulkan/explicit_layer.d") 0)
  (sb-posix:setenv "DYLD_LIBRARY_PATH" (concatenate 'string *vulkan-sdk-path* "/lib") 0)
  (sb-posix:setenv "VULKAN_FRAMEWORK_PATH" (concatenate 'string *vulkan-sdk-path* "/Frameworks") 0)
  (sb-posix:setenv "DYLD_FRAMEWORK_PATH" (concatenate 'string *vulkan-sdk-path* "/Frameworks") 0)
)

(my-define-foreign-library vulkan-loader
  (:darwin "libvulkan.1.dylib")
  (:windows (concatenate 'string *vulkan-sdk-path* "/Source/lib/vulkan-1.dll")))

(my-define-foreign-library cimgui
  (:darwin (concatenate 'string *vktk-dir* "/ifc/lib/cimgui.dylib"))
  (:windows (concatenate 'string *vktk-dir* "/ifc/lib/cimgui.dll")))

(my-define-foreign-library glfw3
  (:darwin (concatenate 'string *vktk-dir* "/ifc/lib/libglfw.3.4.dylib"))
  (:windows (concatenate 'string *vktk-dir* "/ifc/lib/glfw3.dll")))

(cffi:use-foreign-library vulkan-loader)
(cffi:use-foreign-library cimgui)
(cffi:use-foreign-library glfw3)
