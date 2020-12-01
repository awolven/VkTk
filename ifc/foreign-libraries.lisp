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

(defparameter *home-dir* #-(or darwin windows) "/home/awolven"
  #+darwin "/Users/awolven" #+windows "C:/Users/awolven")

(defparameter *vktk-dir* (namestring (asdf/system:system-relative-pathname :vktk "")))

(defparameter *vulkan-sdk-path*
  #+darwin (concatenate 'string *home-dir* "/vulkansdk-macos-1.1.130.0/macOS")
  #+linux (concatenate 'string *home-dir* "/swiftshader-build1/Linux")
  #+windows "C:/VulkanSDK/1.1.121.0")

(defparameter *libshaderc-path*
  #+linux nil
  #+windows (concatenate
	     'string *home-dir*
	     "/Documents/Visual Studio 2015/projects/shaderc_wrap/x64/Debug/shaderc_wrap.dll")
  #+darwin (namestring (asdf/system:system-relative-pathname :vktk "ifc/shaderc/shaderc_wrap.dylib"))))

#+darwin
(progn
  (sb-posix:setenv "VULKAN_SDK" *vulkan-sdk-path* 0)
  (sb-posix:setenv "VK_ICD_FILENAMES" (concatenate 'string *vulkan-sdk-path* "/etc/vulkan/icd.d/MoltenVK_icd.json") 0)
  (sb-posix:setenv "VK_LAYER_PATH" (concatenate 'string *vulkan-sdk-path* "/etc/vulkan/explicit_layer.d") 0)
  (sb-posix:setenv "DYLD_LIBRARY_PATH" (concatenate 'string *vulkan-sdk-path* "/lib") 0)
  (sb-posix:setenv "VULKAN_FRAMEWORK_PATH" (concatenate 'string *vulkan-sdk-path* "/Frameworks") 0)
  (sb-posix:setenv "DYLD_FRAMEWORK_PATH" (concatenate 'string *vulkan-sdk-path* "/Frameworks") 0)
  )

#+linux
(progn
  (sb-posix:setenv "VK_ICD_FILENAMES" (concatenate 'string *vulkan-sdk-path* "/vk_swiftshader_icd.json") 0)
  (sb-posix:setenv "VK_LAYER_PATH" (concatenate 'string *vulkan-sdk-path* "/usr/share/vulkan/explicit_layer.d") 0))

(my-define-foreign-library vulkan-loader
  (:linux "libvulkan.so.1")
  (:darwin "libvulkan.1.dylib")
  (:windows "vulkan-1.dll"))

(my-define-foreign-library shaderc
  (:darwin "libshaderc_shared.dylib"))
			  
(my-define-foreign-library glfw3
  (:darwin (asdf/system:system-relative-pathname :vktk "ifc/lib/libglfw.3.4.dylib"))
  (:windows (asdf/system:system-relative-pathname :vktk "ifc/lib/glfw3.dll"))
  (:linux "libglfw.so.3"))

(cffi:use-foreign-library vulkan-loader)

(cffi:use-foreign-library glfw3)
