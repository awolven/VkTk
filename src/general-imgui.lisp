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

(in-package :vktk)

(defmethod initialize-instance :after ((instance imgui) &rest initargs
				       &key application
					 pipeline-cache
					 descriptor-pool
					 (install-callbacks? t)
					 (style-colors :classic)
					 (shared-font-atlas +nullptr+)
					 &allow-other-keys)
  (declare (ignore initargs))

  (ig:igCreateContext shared-font-atlas)

  (let ((io (ig::igGetIO)))
    (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::ConfigFlags)
	  (logior (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::ConfigFlags)
		  ig::ImGuiConfigFlags_NavEnableKeyboard
		  ig::ImGuiConfigFlags_DockingEnable
		  ig::ImGuiConfigFlags_ViewportsEnable))
      
    (let ((style (ig::igGetStyle)))
      (case style-colors
	(:light (ig::igStyleColorsLight style))
	(:dark (ig::igStyleColorsDark style))
	(t (ig::igStyleColorsClassic style)))

      (when (not (zerop (logand (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::ConfigFlags)
				ig::ImGuiConfigFlags_ViewportsEnable)))
	(setf (foreign-slot-value style '(:struct ig::ImGuiStyle) 'ig::WindowRounding) 0.0f0))))

  
  (let* ((window (first (last (window-registry application))))
	 (swapchain (swapchain window)))

    (imgui-glfw-init instance window :install-callbacks? install-callbacks?)
    
    (setf (render-pass instance) (render-pass swapchain)
	  (pipeline-cache instance) pipeline-cache
	  (descriptor-pool instance) descriptor-pool
	  (frame-count instance) (number-of-images swapchain))

    (imgui-vulkan-init instance (render-pass swapchain)))
  
  (values))

(defclass imgui-frame-data ()
  ((vertex-buffer :initform nil :accessor vertex-buffer)
   (index-buffer :initform nil :accessor index-buffer)
   (vertex-buffer-size :initform nil :accessor vertex-buffer-size)
   (index-buffer-size :initform nil :accessor index-buffer-size)
   (vertex-buffer-memory :initform nil :accessor vertex-buffer-memory)
   (index-buffer-memory :initform nil :accessor index-buffer-memory)))

(defun maybe-init-frame-data (window frame-count)
  (unless (window-frame-data window)
    (setf (window-frame-data window) (make-array frame-count :initial-element nil))))


    
(defun imgui-shutdown (imgui)
  (imgui-vulkan-destroy-device-objects imgui)
  (ig::igDestroyContext (imgui-context imgui))
  (setf (imgui-context imgui) nil)
  (values))


      

