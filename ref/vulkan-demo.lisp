(in-package :cl-user)

#|
extensions:
*instance-extensions*
VK_KHR_surface
VK_KHR_display
VK_KHR_xlib_surface
VK_KHR_xcb_surface
VK_KHR_wayland_surface
VK_KHR_mir_surface
VK_KHR_android_surface
VK_KHR_win32_surface
VK_EXT_debug_report

*device-extensions*
VK_KHR_swapchain
VK_KHR_display_swapchain
VK_NV_glsl_shader
VK_KHR_sampler_mirror_clamp_to_edge
VK_IMG_filter_cubic
VK_AMD_rasterization_order
VK_AMD_shader_trinary_minmax
VK_AMD_shader_explicit_vertex_parameter

(defparameter *instance-extension-registry*
  `((:VK_KHR_surface )
    (:VK_KHR_display )
    (:VK_KHR_xlib_surface )
    (:VK_KHR_xcb_surface )
    (:VK_KHR_wayland_surface )
    (:VK_KHR_mir_surface )
    (:VK_KHR_android_surface )
    (:VK_KHR_win32_surface )
    (:VK_EXT_debug_report )))

(defclass instance ()
  ((handle :initarg :handle)
   (extensions :initform nil)))

(defclass device ()
  ((handle :initarg :handle)
   (extensions :initform nil)))

(defun create-graphics-device (gpu &key flags queue-initargs layers extensions features)
  (cffi:with-foreign-objects (p :pointer)
    (%vk:create-device gpu dci +nullptr+ p)
    (make-instance 'device
		   :handle (cffi:mem-aref p :pointer))))

(defun make-queue-flags (&key (graphics t) (compute nil) (transfer nil) (sparse-binding nil))
  (logior (if graphics #x1 0) (if compute #x2 0) (if transfer #x4 0) (if sparse-binding #x8 0)))

(defun register-device-extension (device cname)
  (push (cons (make-keyword cname) (make-instance 'extension
						  :device device
						  :cname cname))
	(slot-value device 'extensions)))		 

(defclass extension ()
  ((device :initarg :device)
   (cname :initarg :cname)
   (functions :initform nil)))

(defclass extension-function ()
  ((extension)
   (cname)
   (address)
   (retval)
   (arguments)))

(defclass instance-extension-function (extension-function)
  ((instance)))

(defclass device-extension-function (extension-function)
  ((device)))

|#

(defpackage :vk-demo
  (:use :cl :vk))

(in-package :vk-demo)

(defparameter +nullptr+ (cffi-sys:null-pointer))

(defclass renderer ()
  ((instance :reader get-vulkan-instance :writer (setf vulkan-instance) :initarg :vk-instance)
   (gpu :reader get-vulkan-physical-device :writer (setf vulkan-physical-device) :initarg :gpu)
   (device :reader get-vulkan-device :writer (setf vulkan-device) :initarg :vk-device)
   (queue :reader get-vulkan-queue :writer (setf vulkan-queue) :initarg :vk-queue)
   (gpu-props :reader get-vulkan-physical-device-properties
	      :writer (setf vulkan-physical-device-properties) :initarg :vk-phys-props)
   (gpu-memory-props :reader get-vulkan-physical-device-memory-properties
		     :writer (setf vulkan-physical-device-memory-properties) :initarg :vk-mem-props)
   (graphics-family-index :initform nil
			  :reader get-vulkan-graphics-queue-family-index
			  :writer (setf vulkan-graphics-queue-family-index))
   (window)
   (instance-layers :accessor instance-layers :initform ())
   (instance-extensions :accessor instance-extensions :initform ())
   (device-layers)
   (device-extensions :accessor device-extensions :initform nil)
   (debug-report)
   (debug-callback-create-info)
   (preferred-gpu-name :initform "nomatch"
		       ;;"GeForce GTX 1060 6GB" ;;"AMD Radeon Pro 455"
		       :accessor preferred-gpu-name)))

(defun %setup-layers-and-extensions (renderer)
  (add-required-platform-instance-extensions renderer)
  #+MACOS
  (push "MoltenVK" (instance-layers renderer))
  (push "VK_KHR_swapchain" (device-extensions renderer))
  (values))

;; functions which will need to be fast since they are in the render loop:
;; %update-os-window, begin-render and end-render

#+WINDOWS
(defun init-platform ()
  (values))

#+WINDOWS
(defun de-init-platform ()
  (values))

(defun add-required-platform-instance-extensions (renderer)
  #+MACOS
  (push :mvk-macos-surface (instance-extensions renderer))
  #+WINDOWS
  (push :khr-win32-surface (instance-extensions renderer))
  (values))



(defun %initialize-vk-instance (renderer)
  (setf (vulkan-instance renderer)
	(simple-create-instance)
	#+NIL
	(setf %vk::*instance*
	      (vk::create-instance
	       :exts (instance-extensions renderer)
	       :layers (instance-layers renderer))))
  (values))

(defun %shutdown-vk-instance (renderer)
  (%vk::destroy-instance (get-vulkan-instance renderer) +nullptr+)
  (setf (vulkan-instance renderer) nil)
  (values))

(defvar %vk::*device*)

(defun create-vk-device (renderer)
  (%vk::with-vk-structs ((device-create-info %vk:device-create-info
					     `(:queue-create-info-count
					       1
					       :p-queue-create-infos
					       ((:queue-family-index
						 ,(get-vulkan-graphics-queue-family-index renderer)
						 :p-queue-priorities (1.0f0)
						 :queue-count 1
						 ))
					       :enabled-extension-count
					       ,(length (device-extensions renderer))
					       :pp-enabled-extension-names
					       ,(device-extensions renderer))))
    (cffi:with-foreign-object (p-device :pointer)
      (%vk:create-device (get-vulkan-physical-device renderer)
			 device-create-info +nullptr+ p-device)
      (setf (vulkan-device renderer) (cffi:mem-aref p-device :pointer 0))
      (values))))

(defun setup-device-queue (renderer)
  (let ((queue (vk::get-device-queue (get-vulkan-device renderer)
				     (get-vulkan-graphics-queue-family-index renderer)
				     0)))
    (setf (vulkan-queue renderer) queue))
  (values))

(defun %initialize-device (renderer)
  (let ((devices (enumerate-physical-devices (get-vulkan-instance renderer)))
	(gpu)
	(preferred-gpu (preferred-gpu-name renderer)))
    (unless devices
      (error "No Vulkan devices found."))
    (flet ((bind-physical-device (device props)
	     (setf (vulkan-physical-device renderer) device
		   (vulkan-physical-device-properties renderer) props
		   (vulkan-physical-device-memory-properties renderer)
		   (get-physical-device-memory-properties device))
	     device))
      (setq gpu
	    (loop for device in devices
	       do (let ((props (get-physical-device-properties device)))
		    (when (string= (getf props :device-name) preferred-gpu)
		      (return (bind-physical-device device props))))
	       finally (return (bind-physical-device
				(first devices)
				(get-physical-device-properties (first devices))))))

      (format t "~%gpu: ~A" gpu)
      (finish-output *standard-output*)
      (let ((queue-family-props (get-physical-device-queue-family-properties gpu)))
	(loop for queue-props in queue-family-props
	     for i from 0
	   do (when (member :graphics (getf queue-props :queue-flags))
		(setf (vulkan-graphics-queue-family-index renderer) i)
		(return))))
      (unless (get-vulkan-graphics-queue-family-index renderer)
	(error "queue family supporting graphics not found")))
    #+OLD(create-vk-device renderer)
    (setf (slot-value renderer 'device) (slot-value (simple-create-device gpu) 'handle))
    (setup-device-queue renderer))
  (values))

(cffi:defcstruct VkQueueFamilyProperties
  (queueFlags :int)
  (queueCount :uint32)
  (timestampValidBits :uint32)
  (minImageTransferGranularity (:struct %vk::extent-3d)))

(defun simple-choose-queue-family (gpu &key (graphics t) (compute nil) (transfer nil) (sparse-binding nil))
  (cffi:with-foreign-objects ((p-queue-family-property-count :uint32)
			      (p-queue-family-properties '(:struct VkQueueFamilyProperties)))
    (let ((queue-props-desired-flags
	   (logior (if graphics #x1 0) (if compute #x2 0) (if transfer #x4 0) (if sparse-binding #x8 0))))
    
    (%vk:get-physical-device-queue-family-properties gpu
						     p-queue-family-property-count
						     p-queue-family-properties)
    (loop for index from 0 below (cffi:mem-aref p-queue-family-property-count :uint)
       do (when (zerop (logandc2 queue-props-desired-flags
				 (cffi:foreign-slot-value p-queue-family-properties '(:struct VkQueueFamilyProperties) 'queueFlags)))
	    (return-from simple-choose-queue-family index))))))

(defclass device ()
  ((handle :initarg :handle)
   (extensions :initform nil)))

(defun simple-create-instance (&key (extensions (list "VK_KHR_surface" "VK_KHR_win32_surface"))
				 (layers nil #+NIL(list "VK_LAYER_LUNARG_api_dump" "VK_LAYER_LUNARG_core_validation"
					       "VK_LAYER_LUNARG_object_tracker" "VK_LAYER_LUNARG_parameter_validation"
					       "VK_LAYER_LUNARG_standard_validation" "VK_LAYER_GOOGLE_threading"
					       "VK_LAYER_GOOGLE_unique_objects")))
						
  (cffi:with-foreign-object (p-instance '%vk::instance)
    (cffi:with-foreign-object (p-instance-create-info '(:struct %vk::instance-create-info))
      (let ((extension-count (length extensions))
	    (layer-count (length layers)))
	(cffi:with-foreign-objects ((aExtensionNames :pointer extension-count)
				    (aLayerNames :pointer layer-count))
	  (unwind-protect
	       (progn
		 (loop for i from 0
		    for name in extensions
		    do (setf (cffi:mem-aref aExtensionNames :pointer i) (cffi:foreign-string-alloc name)))
		 (loop for i from 0
		    for name in layers
		    do (setf (cffi:mem-aref aLayerNames :pointer i) (cffi:foreign-string-alloc name)))
		 (setf (cffi:foreign-slot-value p-instance-create-info
						'(:struct %vk::instance-create-info) :s-type)
		       :instance-create-info
		       (cffi:foreign-slot-value p-instance-create-info
						'(:struct %vk::instance-create-info) :p-next)
		       +nullptr+
		       (cffi:foreign-slot-value p-instance-create-info
						'(:struct %vk::instance-create-info) :flags)
		       0
		       (cffi:foreign-slot-value p-instance-create-info
						'(:struct %vk::instance-create-info)
						:p-application-info)
		       +nullptr+
		       (cffi:foreign-slot-value p-instance-create-info
						'(:struct %vk::instance-create-info)
						:enabled-layer-count)
		       (length layers)
		       (cffi:foreign-slot-value p-instance-create-info
						'(:struct %vk::instance-create-info)
						:pp-enabled-layer-names)
		       aLayerNames
		       (cffi:foreign-slot-value p-instance-create-info
						'(:struct %vk::instance-create-info)
						:enabled-extension-count)
		       extension-count
		       (cffi:foreign-slot-value p-instance-create-info
						'(:struct %vk::instance-create-info)
						:pp-enabled-extension-names)
		       aExtensionNames)

		 (%vk::create-instance p-instance-create-info +nullptr+ p-instance))
	  
	    (loop for i from 0 below extension-count
	       do (cffi::foreign-string-free (cffi:mem-aref aExtensionNames :pointer i)))))))
    (format t "~%instance: ~A" (cffi:mem-aref p-instance '%vk::instance))
    (finish-output *standard-output*)
    (cffi:mem-aref p-instance '%vk::instance)))
	
	  
(defun simple-create-device (gpu &key (extensions (list "VK_KHR_swapchain"))
				   features
				   queue-family-index)

  (declare (ignore features))
  (let ((device))
    (or queue-family-index (setq queue-family-index (simple-choose-queue-family gpu :graphics t)))
    (format t "~%queue-family-index: ~A" queue-family-index)
    (when (not queue-family-index)
      (error "Could not find a queue family index."))
    (cffi:with-foreign-objects ((pQueueCreateInfos '(:struct %vk::device-queue-create-info))
				(pDeviceCreateInfo '(:struct %vk::device-create-info))
				(pQueuePriorities :float))
      
      (setf (cffi:foreign-slot-value pQueueCreateInfos '(:struct %vk::device-queue-create-info) :s-type)
	    :device-queue-create-info
	    (cffi:foreign-slot-value pQueueCreateInfos '(:struct %vk::device-queue-create-info) :p-next)
	    +nullptr+
	    (cffi:foreign-slot-value pQueueCreateInfos '(:struct %vk::device-queue-create-info) :flags)
	    0
	    (cffi:foreign-slot-value pQueueCreateInfos '(:struct %vk::device-queue-create-info) :queue-family-index)
	    queue-family-index
	    (cffi:foreign-slot-value pQueueCreateInfos '(:struct %vk::device-queue-create-info) :queue-count)
	    1
	    (cffi:mem-aref pQueuePriorities :float) 1.0f0
	    (cffi:foreign-slot-value pQueueCreateInfos '(:struct %vk::device-queue-create-info) :p-queue-priorities)
	    pQueuePriorities)
      (let ((extension-count (length extensions)))
	(cffi:with-foreign-object (aExtensionNames :pointer (length extensions))
	  (unwind-protect
	       (progn
		 (loop for i from 0
		    for name in extensions
		    do (setf (cffi:mem-aref aExtensionNames :pointer i) (cffi:foreign-string-alloc name)))
		 
		 (setf (cffi:foreign-slot-value pDeviceCreateInfo '(:struct %vk::device-create-info) :s-type)
		       :device-create-info
		       (cffi:foreign-slot-value pDeviceCreateInfo '(:struct %vk::device-create-info) :p-next)
		       +nullptr+
		       (cffi:foreign-slot-value pDeviceCreateInfo '(:struct %vk::device-create-info) :flags)
		       0
		       (cffi:foreign-slot-value pDeviceCreateInfo '(:struct %vk::device-create-info) :queue-create-info-count)
		       1
		       (cffi:foreign-slot-value pDeviceCreateInfo '(:struct %vk::device-create-info) :p-queue-create-infos)
		       pQueueCreateInfos
		       (cffi:foreign-slot-value pDeviceCreateInfo '(:struct %vk::device-create-info) :enabled-layer-count)
		       0
		       (cffi:foreign-slot-value pDeviceCreateInfo '(:struct %vk::device-create-info) :pp-enabled-layer-names)
		       +nullptr+
		       (cffi:foreign-slot-value pDeviceCreateInfo '(:struct %vk::device-create-info) :enabled-extension-count)
		       extension-count
		       (cffi:foreign-slot-value pDeviceCreateInfo '(:struct %vk::device-create-info) :pp-enabled-extension-names)
		       aExtensionNames
		       (cffi:foreign-slot-value pDeviceCreateInfo '(:struct %vk::device-create-info) :p-enabled-features)
		       +nullptr+)
	   
		 (cffi:with-foreign-object (p-device :pointer)
		   (%vk:create-device
		    gpu pDeviceCreateInfo +nullptr+ p-device)
		   (setq device (make-instance 'device :handle (cffi:mem-aref p-device :pointer 0)))))

	    (loop for i from 0 below extension-count
	       do (cffi::foreign-string-free (cffi:mem-aref aExtensionNames :pointer i)))))))

    device))
	  
(defun %shutdown-device (renderer)
  (declare (ignorable renderer))
  (values)  )

(defun %setup-debug (renderer)
  (declare (ignorable renderer))
  (values)  )

(defun %initialize-debug (renderer)
  (declare (ignorable renderer))
  (values)  )

(defun %shutdown-debug (renderer)
  (declare (ignorable renderer))
  (values)
  )

(defun %initialize-platform ())
(defun %shutdown-platform ())

(defmethod initialize-instance :after ((self renderer) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (%initialize-platform)
  (%setup-layers-and-extensions self)
  (%setup-debug self)
  (%initialize-vk-instance self)
  (%initialize-debug self)
  (%initialize-device self))

(defun shutdown-renderer (renderer)
  (%shutdown-device renderer)
  (%shutdown-debug renderer)
  (%shutdown-vk-instance renderer)
  (%shutdown-platform)
  (values))

(defclass window ()
  ((renderer :reader get-renderer :writer (setf renderer))
   (surface )
   (swapchain :accessor swapchain)
   (render-pass :reader get-vulkan-render-pass :writer (setf vulkan-render-pass))
   (surface-size-x)
   (surface-size-y)
   (window-name )
   (swapchain-image-count :initform 2)
   (active-swapchain-image-id )
   (swapchain-image-available :initform +nullptr+ :accessor swapchain-image-available)
   (swapchain-images :initform nil)
   (swapchain-image-views :initform nil)
   (framebuffers )
   (depth-stencil-image )
   (depth-stencil-image-memory )
   (depth-stencil-image-view )
   (surface-format :reader surface-format)
   (surface-capabilities )
   (depth-stencil-format :initform :undefined)
   (stencil-available :initform nil)
   (window-should-run :initform t :accessor window-should-run)
   #+win32
   (win32-instance)
   #+win32
   (win32-window)
   #+win32
   (win32-class-name)
   #+win32
   (win32-class-id-counter)
   (render-callback )))



(defun open-window (renderer size-x size-y name)
  (setf (slot-value renderer 'window) (make-instance 'window
						     :renderer renderer
						     :size-x size-x
						     :size-y size-y
						     :name name)))

(defun run (renderer)
  (if (or (not (slot-boundp renderer 'window))
	  (null (slot-value renderer 'window)))
      (update (slot-value renderer 'window))
      t))

(defun close-window (window)
  (setf (window-should-run window) nil)
  (values))

(defun %update-os-window (window)
  (win32::test-update-os-window (slot-value window 'win32-window))
  (values))

(defun update (window)
  (%update-os-window window)
  (window-should-run window))

#+MACOS
(cffi:defcfun ("vkCreateMacOSSurfaceMVK" vkCreateMacOSSurfaceMVK) %vk::result
  (instance :pointer)
  (p-create-info :pointer)
  (p-allocator :pointer)
  (p-surface :pointer))

#+MACOS
(cffi:defcstruct  VkMacOSSurfaceCreateInfoMVK
  (:stype %vk::structure-type)
  (:pnext (:pointer :void))
  (:flags :int)
  (:pview (:pointer :void)))

#+MACOS
(defconstant VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK 1000123000)

#+MACOS
(defun get-cocoa-view ()
  cl-user::*-view-*)

(defun %init-os-window (window width height title)
  #+windows
  (let ((hWnd (win32::test-make-window :width width :height height :name title)))
    (setf (slot-value window 'win32-window) hWnd
	  (slot-value window 'win32-instance) (win32::GetModuleHandle 0)))
  (values))

(defun %init-os-surface (window)
  #+MACOS
  (cffi:with-foreign-object (p-create-info '(:struct vkmacossurfacecreateinfomvk))
    (setf (cffi:foreign-slot-value p-create-info '(:struct vkmacossurfacecreateinfomvk) :stype)
	  VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
	  (cffi:foreign-slot-value p-create-info '(:struct vkmacossurfacecreateinfomvk) :pnext)
	  +nullptr+
	  (cffi:foreign-slot-value p-create-info '(:struct vkmacossurfacecreateinfomvk) :flags)
	  0
	  (cffi:foreign-slot-value p-create-info '(:struct vkmacossurfacecreateinfomvk) :pview)
	  (get-cocoa-view))
    (cffi:with-foreign-object (p-surface :pointer)
      (vkCreateMacOSSurfaceMVK (get-vulkan-instance (get-renderer window))
			    p-create-info +nullptr+ p-surface)
      (setf (slot-value window 'surface) (cffi:mem-aref p-surface :pointer 0))))
  
  #+windows
  (let ((surface (vk::create-win32-surface-khr 
		  (get-vulkan-instance (slot-value window 'renderer))
		  (slot-value window 'win32-instance)
		  (slot-value window 'win32-window))))
    (setf (slot-value window 'surface) surface))
  #+NIL
  (cffi:with-foreign-object (p-create-info '(:struct %vk:win32-surface-create-info-khr))
    (setf (cffi:foreign-slot-value p-create-info '(:struct %vk:win32-surface-create-info-khr) :s-type)
	  :win32-surface-create-info-khr

	  (cffi:foreign-slot-value p-create-info '(:struct %vk:win32-surface-create-info-khr) :hinstance)
	  (slot-value window 'win32-instance)

	  (cffi:foreign-slot-value p-create-info '(:struct %vk:win32-surface-create-info-khr) :hwnd)
	  (slot-value window 'win32-window))
    
    (cffi:with-foreign-object (pp-surface '%vk::surface-khr)
      (%vk::create-win32-surface-khr (get-vulkan-instance (slot-value window 'renderer))
				     p-create-info
				     +nullptr+
				     pp-surface)
      (setf (slot-value window 'surface) (cffi:mem-aref pp-surface '%vk::surface-khr))))
	  
  (values))

(defconstant VK_FORMAT_B8G8R8A8_UNORM 37)
(defconstant VK_FORMAT_UNDEFINED 0)
(defconstant VK_COLORSPACE_SRGB_NONLINEAR_KHR 0)

(cffi:defcfun ("vkGetPhysicalDeviceSurfaceSupportKHR" vkGetPhysicalDeviceSurfaceSupportKHR
	       :convention :stdcall)
    %vk::checked-result
  (gpu %vk::physical-device)
  (queue-family-index :uint32)
  (surface %vk::surface-khr)
  (p-supported (:pointer %vk::bool32)))

(cffi:defcfun ("vkGetPhysicalDeviceSurfaceCapabilitiesKHR" vkGetPhysicalDeviceSurfaceCapabilitiesKHR)
    %vk::checked-result
  (gpu %vk::physical-device)
  (surface %vk::surface-khr)
  (p-surface-capabilities (:pointer (:struct %vk::surface-capabilities-khr))))

(cffi:defcfun ("vkGetPhysicalDeviceSurfaceFormatsKHR" vkGetPhysicalDeviceSurfaceFormatsKHR)
    %vk::checked-result
  (gpu %vk::physical-device)
  (surface %vk::surface-khr)
  (p-surface-format-count (:pointer :uint32))
  (p-surface-formats (:pointer (:struct %vk::surface-format-khr))))

(cffi:defcfun ("vkGetPhysicalDeviceSurfacePresentModesKHR" vkGetPhysicalDeviceSurfacePresentModesKHR)
    %vk::checked-result
  (gpu %vk::physical-device)
  (surface %vk::surface-khr)
  (p-present-mode-count (:pointer :uint32))
  (p-present-modes (:pointer %vk:present-mode-khr)))

(cffi:defcfun ("vkCreateSwapchainKHR" vkCreateSwapchainKHR) %vk::checked-result
  (device %vk::device)
  (p-create-info (:pointer (:struct %vk::swapchain-create-info-khr)))
  (p-allocator (:pointer (:struct %vk::allocation-callbacks)))
  (p-swapchain (:pointer %vk::swapchain-khr)))

(cffi:defcfun ("vkGetSwapchainImagesKHR" vkGetSwapchainImagesKHR) %vk::checked-result
  (device %vk::device)
  (swapchain %vk::swapchain-khr)
  (p-swapchain-image-count (:pointer :uint32))
  (p-swapchain-images (:pointer %vk::image)))

(defun get-physical-device-surface-support-khr (device gpu queue-family-index surface)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (let ((addr (cffi:with-foreign-string (name "vkGetPhysicalDeviceSurfaceSupportKHR")
		(%vk::get-device-proc-addr (print device) name))))

    (cffi::with-foreign-object (p-supported '%vk::bool32)
      (print (cffi:mem-aref p-supported '%vk::bool32))
      (cffi:foreign-funcall-pointer
       (print addr) nil
       %vk::physical-device (print gpu) :uint32 (print queue-family-index) %vk::surface-khr (print surface)
       %vk::bool32 p-supported :int #+NIL %vk::checked-result)

      (cffi:mem-aref p-supported '%vk::bool32))))

(defvar *surf*)

(defun %init-surface (window)
  (%init-os-surface window)
  (let ((device (get-vulkan-device (get-renderer window))))
    (declare (ignore device))
    (let* ((gpu (get-vulkan-physical-device (get-renderer window)))
	   (surface-supported-p
	    (cffi::with-foreign-object (p-supported '%vk::bool32)
	      (print p-supported)
	      (vkGetPhysicalDeviceSurfaceSupportKHR
	       gpu (get-vulkan-graphics-queue-family-index (get-renderer window))
	       (slot-value window 'surface) p-supported)
	      #+NIL
	      (get-physical-device-surface-support-khr device
						       gpu
						       0
						       (slot-value window 'surface))
	      (cffi:mem-aref p-supported '%vk::bool32))))
      (unless surface-supported-p
	(error "WSI not supported"))
      (let ((p-surface-capabilities (cffi:foreign-alloc '(:struct %vk::surface-capabilities-khr))))
	(vkGetPhysicalDeviceSurfaceCapabilitiesKHR
	 gpu
	 (slot-value window 'surface)
	 p-surface-capabilities)
	(setf (slot-value window 'surface-capabilities) p-surface-capabilities)
	(let* ((current-extent (cffi:foreign-slot-pointer p-surface-capabilities
							  '(:struct %vk::surface-capabilities-khr)
							  :current-extent))
	       (width (cffi:foreign-slot-value
		       current-extent '(:struct %vk::extent-2d)
		       :width))
	       (height (cffi:foreign-slot-value
			current-extent '(:struct %vk::extent-2d)
			:height)))
	  (print width) (print height)
	  (when (< width most-positive-fixnum)
	    (setf (slot-value window 'surface-size-x) width
		  (slot-value window 'surface-size-y) height))))
      (cffi:with-foreign-objects ((p-format-count :uint32))
	(setf (cffi:mem-aref p-format-count :uint32) 0)
	(vkGetPhysicalDeviceSurfaceFormatsKHR gpu (print (slot-value window 'surface)) p-format-count +nullptr+)
	(let ((count (cffi:mem-aref p-format-count :uint32 0)))
	  (when (= 0 (print count))
	    (error "surface formats missing"))
	  (let ((p-formats (cffi:foreign-alloc '(:struct %vk::surface-format-khr) :count (cffi:mem-aref p-format-count :uint32))))
	    ;;(cffi:with-foreign-object (p-formats '(:struct %vk::surface-format-khr) count)
	    (vkGetPhysicalDeviceSurfaceFormatsKHR gpu (slot-value window 'surface) p-format-count
						  p-formats)
	    (setf (slot-value window 'surface-format) p-formats)
	    (print p-formats)
	    (when (eq :undefined (print (cffi:foreign-slot-value p-formats '(:struct %vk::surface-format-khr) :format)))
	      (setf (cffi:foreign-slot-value (slot-value window 'surface-format)
					     '(:struct %vk::surface-format-khr)
					     :format)
		    :b8g8r8a8-unorm
		    (cffi:foreign-slot-value (slot-value window 'surface-format)
					     '(:struct %vk::surface-format-khr)
					     :color-space)
		    :SRGB-NONLINEAR-KHR)))
	  ))))
  (values))

(defparameter *uint64-max* (1- (expt 2 64)))
(defparameter *uint32-max* (1- (expt 2 32)))
(defparameter *vk-result-max-enum* 2147483647)

(defun begin-render (window)
  (cffi:with-foreign-object (p-asii :int)
    (setf (cffi:mem-aref p-asii :uint32 0) *uint32-max*)
    (%vk::acquire-next-image-khr (get-vulkan-device (get-renderer window))
				 (swapchain window)
				 *uint64-max*
				 +nullptr+
				 (swapchain-image-available window)
				 p-asii)
    (setf (slot-value window 'active-swapchain-image-id)
	  (cffi:mem-aref p-asii :int 0))
    (cffi:with-foreign-object (pp-swapchain-image-available '%vk::fence)
      (setf (cffi:mem-aref pp-swapchain-image-available '%vk::fence)
	    (swapchain-image-available window))
      (%vk::wait-for-fences (get-vulkan-device (get-renderer window))
			    1
			    pp-swapchain-image-available
			    t *uint64-max*)
      (%vk::reset-fences (get-vulkan-device (get-renderer window))
			 1 pp-swapchain-image-available)
      (setf (slot-value window 'swapchain-image-available)
	    (cffi:mem-aref pp-swapchain-image-available '%vk::fence))
      (%vk::queue-wait-idle (get-vulkan-queue (get-renderer window)))))
  (values))  

(defun end-render (window wait-semaphores wait-semaphore-count)
  (cffi:with-foreign-objects ((p-present-info '(:struct %vk::present-info-khr))
			      (p-present-result :int)
			      (p-swapchains '%vk::swapchain-khr 1)
			      (p-asii :uint32))
    (setf 
     (cffi:mem-aref p-present-result :int 0) *vk-result-max-enum*

     (cffi:mem-aref p-swapchains '%vk::swapchain-khr 0)
     (slot-value window 'swapchain)

     (cffi:mem-aref p-asii :uint32)
     (slot-value window 'active-swapchain-image-id))
    
    (setf (cffi:foreign-slot-value p-present-info '(:struct %vk::present-info-khr) :s-type)
	  :present-info-khr
	  (cffi:foreign-slot-value p-present-info '(:struct %vk::present-info-khr) :p-next)
	  +nullptr+
	  (cffi:foreign-slot-value p-present-info '(:struct %vk::present-info-khr) :wait-semaphore-count)
	  wait-semaphore-count
	  (cffi:foreign-slot-value p-present-info '(:struct %vk::present-info-khr) :p-wait-semaphores)
	  wait-semaphores
	  (cffi:foreign-slot-value p-present-info '(:struct %vk::present-info-khr) :swapchain-count)
	  1
	  (cffi:foreign-slot-value p-present-info '(:struct %vk::present-info-khr) :p-swapchains)
	  p-swapchains
	  (cffi:foreign-slot-value p-present-info '(:struct %vk::present-info-khr) :p-image-indices)
	  p-asii
	  (cffi:foreign-slot-value p-present-info '(:struct %vk::present-info-khr) :p-results)
	  p-present-result)
    (%vk:queue-present-khr (get-vulkan-queue (slot-value window 'renderer))
			   p-present-info))
  (values))

(defun get-vulkan-active-framebuffer (window)
  (aref (slot-value window 'framebuffers) (slot-value window 'active-swapchain-image-id)))

(defun get-vulkan-surface-size (window)
  (values (slot-value window 'surface-size-x) (slot-value window 'surface-size-y)))



(defun %init-swapchain (window)
  (when (< (slot-value window 'swapchain-image-count)
	   (1+ (cffi:foreign-slot-value
		(slot-value window 'surface-capabilities)
		'(:struct %vk::surface-capabilities-khr) :min-image-count)))
    (setf (slot-value window 'swapchain-image-count)
	  (1+ (cffi:foreign-slot-value (slot-value window 'surface-capabilities) '(:struct %vk::surface-capabilities-khr) :min-image-count))))
  (when (> (cffi:foreign-slot-value (slot-value window 'surface-capabilities) '(:struct %vk::surface-capabilities-khr) :max-Image-Count) 0)
    (when (> (slot-value window 'swapchain-image-count)
	     (cffi:foreign-slot-value (slot-value window 'surface-capabilities) '(:struct %vk::surface-capabilities-khr) :max-Image-Count))
      (setf (slot-value window 'swapchain-image-count)
	    (cffi:foreign-slot-value (slot-value window 'surface-capabilities) '(:struct %vk::surface-capabilities-khr) :max-Image-Count))))
  (let ((present-mode :PRESENT-MODE-FIFO-KHR)
	(renderer (get-renderer window)))
    (cffi:with-foreign-object (p-present-mode-count :uint32)
      (vkGetPhysicalDeviceSurfacePresentModesKHR (get-vulkan-physical-device renderer)
						 (slot-value window 'surface)
						 p-present-mode-count
						 +nullptr+)
      (let ((present-mode-count (cffi:mem-aref p-present-mode-count :uint32)))
	(cffi:with-foreign-object (p-present-modes '%vk::present-mode-khr present-mode-count)
	  (vkGetPhysicalDeviceSurfacePresentModesKHR (get-vulkan-physical-device renderer)
						     (slot-value window 'surface)
						     p-present-mode-count
						     p-present-modes)
	  
	  (loop for i from 0 below present-mode-count
	     do (when (eq (setq present-mode (cffi:mem-aref p-present-modes '%vk::present-mode-khr i))
			  :mailbox-khr)
		  (return))))))
    (print present-mode)
    (cffi:with-foreign-object (swapchain-create-info '(:struct %vk::swapchain-create-info-khr))
      (setf (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr) :s-type)
	    :swapchain-create-info-khr
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr) :p-next)
	    +nullptr+
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr) :flags)
	    0
	    
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr) :surface)
	    (slot-value window 'surface)
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr) :min-image-count)
	    (slot-value window 'swapchain-image-count)
	    
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr) :image-format)
	    (let ((f (cffi:foreign-slot-value (slot-value window 'surface-format)
					      '(:struct %vk::surface-format-khr)
					      :format)))
	      (format t "~%(slot-value window 'surface-format) :format ~A" f)
	      f)

	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr) :image-color-space)
	    (print (cffi:foreign-slot-value (slot-value window 'surface-format)
					     '(:struct %vk::surface-format-khr)
					     :color-space))
	    (cffi:foreign-slot-value (cffi:foreign-slot-pointer
				      swapchain-create-info
				      '(:struct %vk::swapchain-create-info-khr)
				      :image-extent)
				     '(:struct %vk::extent-2d) :width)
	    (slot-value window 'surface-size-x)
	    (cffi:foreign-slot-value (cffi:foreign-slot-pointer
				      swapchain-create-info
				      '(:struct %vk::swapchain-create-info-khr)
				      :image-extent)
				     '(:struct %vk::extent-2d) :height)
	    (slot-value window 'surface-size-y)
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr)
				     :image-array-layers)
	    1
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr)
				     :image-usage)
	    :color-attachment
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr)
				     :image-sharing-mode)
	    :exclusive
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr)
				     :queue-family-index-count)
	    (get-vulkan-graphics-queue-family-index (get-renderer window))
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr)
				     :p-queue-family-indices)
	    +nullptr+
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr)
				     :pre-transform)
	    :identity
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr)
				     :composite-alpha)
	    :opaque
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr)
				     :present-mode)
	    present-mode
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr)
				     :clipped)
	    t
	    (cffi:foreign-slot-value swapchain-create-info
				     '(:struct %vk::swapchain-create-info-khr)
				     :old-swapchain)
	    +nullptr+)

      (setf (slot-value window 'swapchain)
	    (cffi:with-foreign-object (p-swapchain '%vk::swapchain-khr)
	      (VkCreateSwapchainKHR (get-vulkan-device (slot-value window 'renderer))
				    swapchain-create-info +nullptr+ p-swapchain)
	      (cffi:mem-aref p-swapchain '%vk::swapchain-khr)))
      (cffi:with-foreign-object (p-swapchain-image-count :uint32)
	(VkGetSwapchainImagesKHR (get-vulkan-device (slot-value window 'renderer))
				 (swapchain window)
				 p-swapchain-image-count
				 +nullptr+)
	(setf (slot-value window 'swapchain-image-count)
	      (cffi:mem-aref p-swapchain-image-count :uint32)))))

  (values))

(cffi:defcfun ("vkCreateImageView" vkCreateImageView) %vk::checked-result
  (device %vk::device)
  (p-create-info (:pointer (:struct %vk::image-view-create-info)))
  (p-allocator (:pointer (:struct %vk::allocation-callbacks)))
  (p-view (:pointer %vk::image-view)))

(defun %init-swapchain-images (window)

  (let* ((count (slot-value window 'swapchain-image-count))
	 (swapchain-images (setf (slot-value window 'swapchain-images)
				 (make-array count :initial-element +nullptr+)))
	 (swapchain-image-views (make-array count :initial-element +nullptr+))
	 (p-components)
	 (p-subresource-range))

    (cffi:with-foreign-objects ((p-swapchain-images '%vk::image count)
				(p-swapchain-image-views '%vk::image-view count)
				(p-swapchain-image-count :uint32))
	
      (setf (cffi:mem-aref p-swapchain-image-count :uint32) count)
	
      (VkGetSwapchainImagesKHR (get-vulkan-device (slot-value window 'renderer))
			       (slot-value window 'swapchain)
			       p-swapchain-image-count
			       p-swapchain-images)

      (loop for i below (cffi::mem-aref p-swapchain-image-count :uint32)
	 do (setf (aref swapchain-images i) (cffi:mem-aref p-swapchain-images '%vk::image i)))
  
      (loop for i from 0 below count
	 do (cffi:with-foreign-objects ((p-image-view-create-info '(:struct %vk::image-view-create-info)))
		
	      (setf (cffi:foreign-slot-value
		     p-image-view-create-info '(:struct %vk::image-view-create-info) :s-type)
		    :image-view-create-info

		    (cffi:foreign-slot-value
		     p-image-view-create-info '(:struct %vk::image-view-create-info) :p-next)
		    +nullptr+

		    (cffi:foreign-slot-value
		     p-image-view-create-info '(:struct %vk::image-view-create-info) :flags)
		    0
		    
		    (cffi:foreign-slot-value
		     p-image-view-create-info '(:struct %vk::image-view-create-info) :image)
		    (progn (format t "~%(aref swapchain-images ~A) : ~A" i (aref swapchain-images i))
			   (aref swapchain-images i))
		      
		    (cffi:foreign-slot-value
		     p-image-view-create-info '(:struct %vk::image-view-create-info) :view-type)
		    :2d
		      
		    (cffi:foreign-slot-value
		     p-image-view-create-info '(:struct %vk::image-view-create-info) :format)
		    (print (cffi:foreign-slot-value (slot-value window 'surface-format)
						    '(:struct %vk::surface-format-khr) :format)
			   )

		    p-components (cffi:foreign-slot-pointer
				  p-image-view-create-info '(:struct %vk::image-view-create-info) :components)

		    p-subresource-range (cffi:foreign-slot-pointer
					 p-image-view-create-info '(:struct %vk::image-view-create-info) :subresource-range)

		    (cffi:foreign-slot-value p-components '(:struct %vk::component-mapping) :r)
		    :identity
		
		    (cffi:foreign-slot-value p-components '(:struct %vk::component-mapping) :g)
		    :identity
		 
		    (cffi:foreign-slot-value p-components '(:struct %vk::component-mapping) :b)
		    :identity
		 
		    (cffi:foreign-slot-value p-components '(:struct %vk::component-mapping) :a)
		    :identity
		 
		    (cffi:foreign-slot-value p-subresource-range '(:struct %vk::image-subresource-range) :aspect-mask)
		    :color
		 
		    (cffi:foreign-slot-value p-subresource-range '(:struct %vk::image-subresource-range) :base-mip-level)
		    0
		      
		    (cffi:foreign-slot-value p-subresource-range '(:struct %vk::image-subresource-range) :level-count)
		    1
		      
		    (cffi:foreign-slot-value p-subresource-range '(:struct %vk::image-subresource-range) :base-array-layer)
		    0

		    (cffi:foreign-slot-value p-subresource-range '(:struct %vk::image-subresource-range) :layer-count)
		    1)
	      ;(format t "~%swapchain-images: ~A" swapchain-images)
	      (format t "~%p-image-view-create-info: ~A" p-image-view-create-info)
	      ;(print (cffi:foreign-slot-value
	;	      p-image-view-create-info '(:struct %vk::image-view-create-info) :image))
	 ;     (print (cffi:foreign-slot-value
	;	     p-image-view-create-info '(:struct %vk::image-view-create-info) :format))
	      (finish-output *standard-output*)

	      (vkCreateImageView (get-vulkan-device (slot-value window 'renderer))
				 p-image-view-create-info
				 +nullptr+
				 (cffi:mem-aptr p-swapchain-image-views '%vk::image-view i))

	      (loop for i from 0 below count
		 do (setf (aref swapchain-image-views i) (cffi:mem-aref p-swapchain-image-views '%vk::image-view i))
		 finally (setf (slot-value window 'swapchain-images) swapchain-images
			       (slot-value window 'swapchain-image-views) swapchain-image-views))))))
	      (values))
					  
(defun %init-depth-stencil-image (window)
  (let ((try-formats (list :d32-sfloat-s8-uint
			   :d24-unorm-s8-uint
			   :d16-unorm-s8-uint
			   :s8-uint
			   :d32-sfloat
			   :d16-unorm)))
    (loop for f in try-formats
       do (cffi:with-foreign-object (p-format-properties '(:struct %vk::format-properties))
	    (format t "~%p-format-properties: ~A" p-format-properties)
	    (finish-output *standard-output*)
	    
	    (%vk::get-physical-device-format-properties (get-vulkan-physical-device (get-renderer window))
							f ;;(cffi:foreign-enum-value '%vk::format f)
							p-format-properties)
	    (when (member :depth-stencil-attachment
			  (cffi:foreign-slot-value p-format-properties '(:struct %vk::format-properties) :optimal-tiling-features))
	      (setf (slot-value window 'depth-stencil-format) f)
	      (return))))
    (when (eq (slot-value window 'depth-stencil-format) :undefined)
      (error "Depth stencil format not selected."))
    (when (member (slot-value window 'depth-stencil-format)
		  (list :d32-sfloat-s8-uint :d24-unorm-s8-uint :d16-unorm-s8-uint :s8-uint))
      (setf (slot-value window 'stencil-available) t))

    (let ((p-extent)
	  (p-components)
	  (p-subresource-range))
      
      (cffi:with-foreign-object (p-image-create-info '(:struct %vk::image-create-info))
	(setf (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :s-type)
	      :image-create-info

	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :p-next)
	      +nullptr+
	      
	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :flags)
	      0
	      
	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :image-type)
	      :2d
	      
	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :format)
	      (print (slot-value window 'depth-stencil-format))

	      p-extent
	      (cffi:foreign-slot-pointer p-image-create-info '(:struct %vk::image-create-info) :extent)

	      (cffi:foreign-slot-value p-extent '(:struct %vk::extent-3d) :width)
	      (slot-value window 'surface-size-x)

	      (cffi:foreign-slot-value p-extent '(:struct %vk::extent-3d) :height)
	      (slot-value window 'surface-size-y)

	      (cffi:foreign-slot-value p-extent '(:struct %vk::extent-3d) :depth)
	      1
	    
	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :mip-levels)
	      1

	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :array-layers)
	      1

	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :samples)
	      :1

	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :tiling)
	      :optimal

	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :usage)
	      :depth-stencil-attachment

	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :sharing-mode)
	      :exclusive

	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :queue-family-index-count)
	      #xffffffff ;;VK_QUEUE_FAMILY_IGNORED

	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :p-queue-family-indices)
	      +nullptr+

	      (cffi:foreign-slot-value p-image-create-info '(:struct %vk::image-create-info) :initial-layout)
	      :undefined)

	(cffi:with-foreign-object (pp-depth-stencil-image '%vk::image)
	  
	  (%vk::create-image (get-vulkan-device (slot-value window 'renderer))
			     p-image-create-info
			     +nullptr+
			     pp-depth-stencil-image)

	  (cffi:with-foreign-object (p-image-memory-requirements '(:struct %vk::memory-requirements))

	    (%vk::get-image-memory-requirements (get-vulkan-device (slot-value window 'renderer))
						(setf (slot-value window 'depth-stencil-image)
						      (cffi:mem-aref pp-depth-stencil-image '%vk::image))
						p-image-memory-requirements)

	    (let ((memory-index (find-memory-type-index-2
				 (get-vulkan-physical-device-memory-properties (slot-value window 'renderer))
				 p-image-memory-requirements
				 (list :device-local))))

	      (format t "~%memory-index: ~A" memory-index)

	      (cffi:with-foreign-object (p-memory-allocate-info '(:struct %vk::memory-allocate-info))
		
		(setf (cffi:foreign-slot-value p-memory-allocate-info '(:struct %vk::memory-allocate-info) :s-type)
		      :memory-allocate-info

		      (cffi:foreign-slot-value p-memory-allocate-info '(:struct %vk::memory-allocate-info) :p-next)
		      +nullptr+

		      ;;(cffi:foreign-slot-value p-memory-allocate-info '(:struct %vk::memory-allocate-info) :flags)
		      ;;0
		      
		      (cffi:foreign-slot-value p-memory-allocate-info '(:struct %vk::memory-allocate-info) :allocation-size)
		      (cffi:foreign-slot-value p-image-memory-requirements '(:struct %vk::memory-requirements) :size)

		      (cffi:foreign-slot-value p-memory-allocate-info '(:struct %vk::memory-allocate-info) :memory-type-index)
		      memory-index)

		(cffi:with-foreign-object (pp-depth-stencil-image-memory '%vk::device-memory)
		  (%vk::allocate-memory (get-vulkan-device (slot-value window 'renderer))
					p-memory-allocate-info
					+nullptr+
					pp-depth-stencil-image-memory)
		  (let ((p-depth-stencil-image-memory
			 (cffi:mem-aref pp-depth-stencil-image-memory '%vk::device-memory)))
		    (%vk::bind-image-memory (get-vulkan-device (slot-value window 'renderer))
					    (cffi:mem-aref pp-depth-stencil-image '%vk::image)
					    p-depth-stencil-image-memory
					    0)
		    (setf (slot-value window 'depth-stencil-image-memory) p-depth-stencil-image-memory)))

		(cffi:with-foreign-object (p-image-view-create-info '(:struct %vk::image-view-create-info))
		  
		  (setf (cffi:foreign-slot-value p-image-view-create-info '(:struct %vk::image-view-create-info) :s-type)
			:image-view-create-info

			(cffi:foreign-slot-value p-image-view-create-info '(:struct %vk::image-view-create-info) :p-next)
			+nullptr+

			(cffi:foreign-slot-value p-image-view-create-info '(:struct %vk::image-view-create-info) :flags)
			0
			
			(cffi:foreign-slot-value p-image-view-create-info '(:struct %vk::image-view-create-info) :image)
			(cffi:mem-aref pp-depth-stencil-image '%vk::image)
			
			(cffi:foreign-slot-value p-image-view-create-info '(:struct %vk::image-view-create-info) :view-type)
			:2d
			
			(cffi:foreign-slot-value p-image-view-create-info '(:struct %vk::image-view-create-info) :format)
			(print (slot-value window 'depth-stencil-format))
			
			p-components
			(cffi:foreign-slot-pointer p-image-view-create-info '(:struct %vk::image-view-create-info) :components)

			(cffi:foreign-slot-value p-components '(:struct %vk::component-mapping) :r)
			:identity
		      
			(cffi:foreign-slot-value p-components '(:struct %vk::component-mapping) :g)
			:identity
		      
			(cffi:foreign-slot-value p-components '(:struct %vk::component-mapping) :b)
			:identity
			
			(cffi:foreign-slot-value p-components '(:struct %vk::component-mapping) :a)
			:identity

			p-subresource-range (cffi:foreign-slot-pointer
					     p-image-view-create-info '(:struct %vk::image-view-create-info) :subresource-range)

			(cffi:foreign-slot-value p-subresource-range '(:struct %vk::image-subresource-range) :aspect-mask)
			(if (slot-value window 'stencil-available)
			    (list :depth :stencil)
			    :depth)
			
			(cffi:foreign-slot-value p-subresource-range '(:struct %vk::image-subresource-range) :base-mip-level)
			0
			
			(cffi:foreign-slot-value p-subresource-range '(:struct %vk::image-subresource-range) :level-count)
			1
			
			(cffi:foreign-slot-value p-subresource-range '(:struct %vk::image-subresource-range) :base-array-layer)
			0
			
			(cffi:foreign-slot-value p-subresource-range '(:struct %vk::image-subresource-range) :layer-count)
			1)

		(cffi:with-foreign-object (pp-depth-stencil-image-view '%vk::image-view)
		  (setf (cffi:mem-aref pp-depth-stencil-image-view '%vk::image-view) +nullptr+)
		  
		  (%vk::create-image-view (get-vulkan-device (slot-value window 'renderer))
					  p-image-view-create-info
					  +nullptr+
					  pp-depth-stencil-image-view)
		  
		  (setf (slot-value window 'depth-stencil-image-view) (cffi:mem-aref pp-depth-stencil-image-view '%vk::image-view)))))))))))
  (values))



(defun find-memory-type-index (p-gpu-memory-properties p-memory-requirements required-properties)
  (loop for i from 0 below (cffi:foreign-slot-value p-gpu-memory-properties '(:struct %vk::physical-device-memory-properties) :memory-type-count)
     do (when (logand (cffi:foreign-slot-value p-memory-requirements '(:struct %vk::memory-requirements) :memory-type-bits)
		      (ash 1 i))
	  (when (eq (logand (cffi:foreign-slot-value
			     (cffi:mem-aref
			      (cffi:foreign-slot-pointer p-gpu-memory-properties '(:struct %vk::physical-device-memory-properties) :memory-types)
			      '(:struct %vk::memory-type) i)
			     '(:struct %vk::memory-type)
			     :property-flags)
			    required-properties)
		    required-properties)
	    (return-from find-memory-type-index i))))
  (error "Couldn't find proper memory type."))

(defun find-memory-type-index-2 (p-gpu-memory-properties p-memory-requirements required-properties)
  (loop for i from 0 below (getf p-gpu-memory-properties :memory-type-count)
     do (when (logand (cffi:foreign-slot-value p-memory-requirements '(:struct %vk::memory-requirements) :memory-type-bits)
		      (ash 1 i))
	  (loop for property in required-properties
	     do (unless (member property (getf (nth i (getf p-gpu-memory-properties :memory-types)) :property-flags))
		  (return))
	     finally (return-from find-memory-type-index-2 i))))
  (error "Couldn't find proper memory type."))
  

(defun %init-render-pass (window)
  (cffi:with-foreign-objects ((p-attachments '(:struct %vk::attachment-description) 2)
			      (p-sub-pass-0-depth-stencil-attachment '(:struct %vk::attachment-reference) 1)
			      (p-sub-pass-0-color-attachments '(:struct %vk::attachment-reference) 1)
			      (p-sub-passes '(:struct %vk::subpass-description) 1)
			      (p-render-pass-create-info '(:struct %vk::render-pass-create-info)))

    (loop for i from 0 below (cffi:foreign-type-size '(:struct %vk::render-pass-create-info))
	   do (setf (cffi:mem-aref p-render-pass-create-info :unsigned-char i) 0))
    
    ;;(cffi:foreign-slot-value p-attachments '(:struct %vk::attachment-description) :s-type)
	  ;;:attachment-description

	  ;;(cffi:foreign-slot-value p-attachments '(:struct %vk::attachment-description) :p-next)
	  ;;+nullptr+
    (setf (cffi:foreign-slot-value p-attachments '(:struct %vk::attachment-description) :flags)
	  0
				     
	  (cffi:foreign-slot-value p-attachments '(:struct %vk::attachment-description) :format)
	  (print (slot-value window 'depth-stencil-format))

	  (cffi:foreign-slot-value p-attachments '(:struct %vk::attachment-description) :samples)
	  :1

	  (cffi:foreign-slot-value p-attachments '(:struct %vk::attachment-description) :load-op)
	  :clear
	    
	  (cffi:foreign-slot-value p-attachments '(:struct %vk::attachment-description) :store-op)
	  :dont-care

	  (cffi:foreign-slot-value p-attachments '(:struct %vk::attachment-description) :stencil-load-op)
	  :dont-care

	  (cffi:foreign-slot-value p-attachments '(:struct %vk::attachment-description) :stencil-store-op)
	  :store

	  (cffi:foreign-slot-value p-attachments '(:struct %vk::attachment-description) :initial-layout)
	  :undefined
	    
	  (cffi:foreign-slot-value p-attachments '(:struct %vk::attachment-description) :final-layout)
	  :depth-stencil-attachment-optimal
	  ;;
	    
	  (cffi:foreign-slot-value (cffi:mem-aptr p-attachments '(:struct %vk::attachment-description) 1)
				   '(:struct %vk::attachment-description) :flags)
	  0

	  (cffi:foreign-slot-value (cffi:mem-aptr p-attachments '(:struct %vk::attachment-description) 1)
				   '(:struct %vk::attachment-description) :format)
	  (print (cffi:foreign-slot-value (slot-value window 'surface-format) '(:struct %vk::surface-format-khr) :format))

	  (cffi:foreign-slot-value (cffi:mem-aptr p-attachments '(:struct %vk::attachment-description) 1)
				   '(:struct %vk::attachment-description) :samples)
	  :1

	  (cffi:foreign-slot-value (cffi:mem-aptr p-attachments '(:struct %vk::attachment-description) 1)
				   '(:struct %vk::attachment-description) :load-op)
	  :clear

	  (cffi:foreign-slot-value (cffi:mem-aptr p-attachments '(:struct %vk::attachment-description) 1)
				   '(:struct %vk::attachment-description) :store-op)
	  :store

	  (cffi:foreign-slot-value (cffi:mem-aptr p-attachments '(:struct %vk::attachment-description) 1)
				   '(:struct %vk::attachment-description) :stencil-load-op)
	  :load

	  (cffi:foreign-slot-value (cffi:mem-aptr p-attachments '(:struct %vk::attachment-description) 1)
				   '(:struct %vk::attachment-description) :stencil-store-op)
	  :store
	  
	  (cffi:foreign-slot-value (cffi:mem-aptr p-attachments '(:struct %vk::attachment-description) 1)
				   '(:struct %vk::attachment-description) :initial-layout)
	  :undefined
	    
	  (cffi:foreign-slot-value (cffi:mem-aptr p-attachments '(:struct %vk::attachment-description) 1)
				   '(:struct %vk::attachment-description) :final-layout)
	  :present-src-khr

	  (cffi:foreign-slot-value p-sub-pass-0-depth-stencil-attachment '(:struct %vk::attachment-reference) :attachment)
	  0

	  (cffi:foreign-slot-value p-sub-pass-0-depth-stencil-attachment '(:struct %vk::attachment-reference) :layout)
	  :depth-stencil-attachment-optimal

	  (cffi:foreign-slot-value p-sub-pass-0-color-attachments '(:struct %vk::attachment-reference) :attachment)
	  1
	    
	  (cffi:foreign-slot-value p-sub-pass-0-color-attachments '(:struct %vk::attachment-reference) :layout)
	  :color-attachment-optimal

	  (cffi:foreign-slot-value p-sub-passes '(:struct %vk::subpass-description) :flags)
	  0
	  
	  (cffi:foreign-slot-value p-sub-passes '(:struct %vk::subpass-description) :pipeline-bind-point)
	  :graphics

	  (cffi:foreign-slot-value p-sub-passes '(:struct %vk::subpass-description) :input-attachment-count)
	  0

	  (cffi:foreign-slot-value p-sub-passes '(:struct %vk::subpass-description) :p-input-attachments)
	  +nullptr+
	  
	  (cffi:foreign-slot-value p-sub-passes '(:struct %vk::subpass-description) :color-attachment-count)
	  1

	  (cffi:foreign-slot-value p-sub-passes '(:struct %vk::subpass-description) :p-color-attachments)
	  p-sub-pass-0-color-attachments

	  (cffi:foreign-slot-value p-sub-passes '(:struct %vk::subpass-description) :p-resolve-attachments)
	  +nullptr+

	  (cffi:foreign-slot-value p-sub-passes '(:struct %vk::subpass-description) :p-depth-stencil-attachment)
	  p-sub-pass-0-depth-stencil-attachment

	  (cffi:foreign-slot-value p-sub-passes '(:struct %vk::subpass-description) :preserve-attachment-count)
	  0

	  (cffi:foreign-slot-value p-sub-passes '(:struct %vk::subpass-description) :p-preserve-attachments)
	  +nullptr+
	  
	  (cffi:foreign-slot-value p-render-pass-create-info '(:struct %vk::render-pass-create-info) :s-type)
	  :render-pass-create-info

	  (cffi:foreign-slot-value p-render-pass-create-info '(:struct %vk::render-pass-create-info) :p-next)
	  +nullptr+

	  (cffi:foreign-slot-value p-render-pass-create-info '(:struct %vk::render-pass-create-info) :flags)
	  0
	  
	  (cffi:foreign-slot-value p-render-pass-create-info '(:struct %vk::render-pass-create-info) :attachment-count)
	  2

	  (cffi:foreign-slot-value p-render-pass-create-info '(:struct %vk::render-pass-create-info) :p-attachments)
	  p-attachments

	  (cffi:foreign-slot-value p-render-pass-create-info '(:struct %vk::render-pass-create-info) :subpass-count)
	  1

	  (cffi:foreign-slot-value p-render-pass-create-info '(:struct %vk::render-pass-create-info) :p-subpasses)
	  p-sub-passes

	  (cffi:foreign-slot-value p-render-pass-create-info '(:struct %vk::render-pass-create-info) :dependency-count)
	  0

	  (cffi:foreign-slot-value p-render-pass-create-info '(:struct %vk::render-pass-create-info) :p-dependencies)
	  +nullptr+

	  )
    (format t "~%p-render-pass-create-info: ~A" p-render-pass-create-info)
    (finish-output *standard-output*)
    (cffi:with-foreign-object (pp-render-pass '%vk::render-pass)
      ;;(setf (cffi:mem-aref pp-render-pass '%vk::render-pass 0) +nullptr+)
      (%vk::create-render-pass (get-vulkan-device (slot-value window 'renderer))
			       p-render-pass-create-info
			       +nullptr+
			       pp-render-pass)

      (setf (slot-value window 'render-pass)
	    (cffi:mem-aref pp-render-pass '%vk::render-pass 0))))
  (values))

(defun %init-framebuffers (window)
  (let* ((count (slot-value window 'swapchain-image-count))
	 (framebuffers (make-array count :initial-element +nullptr+)))

    (loop for i from 0 below count
       do (cffi:with-foreign-objects ((p-attachments '%vk::image-view 2)
				      (p-framebuffer-create-info '(:struct %vk::framebuffer-create-info)))

	    (loop for i from 0 below (cffi:foreign-type-size '(:struct %vk::framebuffer-create-info))
	   do (setf (cffi:mem-aref p-framebuffer-create-info :unsigned-char i) 0))
	    
	    (let ((p-attachment-0 p-attachments)
		  (p-attachment-1 (cffi:mem-aptr p-attachments '%vk::image-view 1)))
	      
	      (setf (cffi:mem-aref p-attachment-0 '%vk::image-view)
		    (slot-value window 'depth-stencil-image-view)

		    (cffi:mem-aref p-attachment-1 '%vk::image-view)
		    (aref (slot-value window 'swapchain-image-views) i)

		    (cffi:foreign-slot-value p-framebuffer-create-info '(:struct %vk::framebuffer-create-info) :s-type)
		    :framebuffer-create-info

		    (cffi:foreign-slot-value p-framebuffer-create-info '(:struct %vk::framebuffer-create-info) :p-next)
		    +nullptr+

		    (cffi:foreign-slot-value p-framebuffer-create-info '(:struct %vk::framebuffer-create-info) :flags)
		    0

		    (cffi:foreign-slot-value p-framebuffer-create-info '(:struct %vk::framebuffer-create-info) :render-pass)
		    (slot-value window 'render-pass)		    

		    (cffi:foreign-slot-value p-framebuffer-create-info '(:struct %vk::framebuffer-create-info) :attachment-count)
		    2
		    
		    (cffi:foreign-slot-value p-framebuffer-create-info '(:struct %vk::framebuffer-create-info) :p-attachments)
		    p-attachments
		    
		    (cffi:foreign-slot-value p-framebuffer-create-info '(:struct %vk::framebuffer-create-info) :width)
		    (slot-value window 'surface-size-x)

		    (cffi:foreign-slot-value p-framebuffer-create-info '(:struct %vk::framebuffer-create-info) :height)
		    (slot-value window 'surface-size-y)

		    (cffi:foreign-slot-value p-framebuffer-create-info '(:struct %vk::framebuffer-create-info) :layers)
		    1)
	      (cffi:with-foreign-object (p-framebuffers '%vk::framebuffer count)
		
		(%vk::create-framebuffer (get-vulkan-device (slot-value window 'renderer))
					 p-framebuffer-create-info
					 +nullptr+
					 (cffi:mem-aptr p-framebuffers '%vk::framebuffer i))
		(setf (aref framebuffers i)
		      (cffi:mem-aref p-framebuffers '%vk::framebuffer i))
		(setf (slot-value window 'framebuffers) framebuffers))))))
  (values))
		
(defun %init-synchronizations (window)
  (cffi:with-foreign-object (p-fence-create-info '(:struct %vk::fence-create-info))
    (setf (cffi:foreign-slot-value p-fence-create-info '(:struct %vk::fence-create-info) :s-type)
	  :fence-create-info

	  (cffi:foreign-slot-value p-fence-create-info '(:struct %vk::fence-create-info) :p-next)
	  +nullptr+

	  (cffi:foreign-slot-value p-fence-create-info '(:struct %vk::fence-create-info) :flags)
	  0)
    
    (cffi:with-foreign-object (pp-swapchain-image-available '%vk:fence)

      (setf (cffi:mem-aref pp-swapchain-image-available '%vk:fence)
	    (slot-value window 'swapchain-image-available))
      
      (%vk::create-fence (get-vulkan-device (slot-value window 'renderer))
			 p-fence-create-info
			 +nullptr+
			 pp-swapchain-image-available)

      (setf (slot-value window 'swapchain-image-available) 
	    (cffi:mem-aref pp-swapchain-image-available '%vk:fence))))
      
  (values))
					  
	  


(defmethod initialize-instance :after ((self window) &rest initargs
				       &key renderer size-x size-y name &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value self 'renderer) renderer
	(slot-value self 'surface-size-x) size-x
	(slot-value self 'surface-size-y) size-y
	(slot-value self 'window-name) name)
  
  (%init-os-window self size-x size-y name)
  (%init-surface self)
  (%init-swapchain self)
  (%init-swapchain-images self)
  (%init-depth-stencil-image self)
  (%init-render-pass self)
  (%init-framebuffers self)
  (%init-synchronizations self)
  
  (values))

#+NIL (
[mvk-info] MoltenVK version 0.19.0. Vulkan version 1.0.64.
[mvk-info] GPU device Intel(R) HD Graphics 530 supports the following Metal Feature Sets:
	OSX GPU Family 1 v3
	OSX GPU Family 1 v2
	OSX GPU Family 1 v1
[mvk-info] GPU device AMD Radeon Pro 455 supports the following Metal Feature Sets:
	OSX GPU Family 1 v3
	OSX GPU Family 1 v2
	OSX GPU Family 1 v1
[mvk-info] You require a license for the MoltenVK Vulkan Core feature set. Reverting to evaluation mode.
[***MoltenVK ERROR***] VK_ERROR_EXTENSION_NOT_PRESENT: Vulkan extension VK_KHR_SWAPCHAIN is not supported.

)

(defparameter *circle-rad* (coerce (* pi 2.0d0) 'single-float))
(defparameter *circle-third* (/ *circle-rad* 3.0f0))
(defparameter *circle-third-1* 0.0f0)
(defparameter *circle-third-2* *circle-third*)
(defparameter *circle-third-3* (* *circle-third* 2.0f0))

(defvar *renderer*)

(defun run-demo ()

  (let* ((r (make-instance 'renderer))
	 (w (open-window r 800 600 "Vulkan API Tutorial 12"))
	 (h-command-pool +nullptr+))

    (setq *renderer* r)

    (cffi:with-foreign-objects ((ph-command-pool '%vk::command-pool)
				(p-pool-create-info '(:struct %vk::command-pool-create-info)))
      (setf (cffi:mem-aref ph-command-pool '%vk::command-pool) h-command-pool

	    (cffi:foreign-slot-value p-pool-create-info '(:struct %vk::command-pool-create-info) :s-type)
	    :command-pool-create-info

	    (cffi:foreign-slot-value p-pool-create-info '(:struct %vk::command-pool-create-info) :p-next)
	    +nullptr+

	    (cffi:foreign-slot-value p-pool-create-info '(:struct %vk::command-pool-create-info) :flags)
	    (list :transient :reset-command-buffer)

	    (cffi:foreign-slot-value p-pool-create-info '(:struct %vk::command-pool-create-info) :queue-family-index)
	    (get-vulkan-graphics-queue-family-index r))

      (%vk::create-command-pool (get-vulkan-device r)
				p-pool-create-info
				+nullptr+
				ph-command-pool)

      (setf h-command-pool (cffi:mem-aref ph-command-pool '%vk::command-pool))
      
      (cffi:with-foreign-objects ((rh-command-buffer '%vk::command-buffer)
				  (r-command-buffer-allocate-info '(:struct %vk::command-buffer-allocate-info)))

	(loop for i from 0 below (cffi:foreign-type-size '(:struct %vk::command-buffer-allocate-info))
	   do (setf (cffi:mem-aref r-command-buffer-allocate-info :unsigned-char i) 0))

	(setf (cffi:mem-aref rh-command-buffer '%vk::command-buffer) +nullptr+

	      (cffi:foreign-slot-value r-command-buffer-allocate-info '(:struct %vk::command-buffer-allocate-info) :s-type)
	      :command-buffer-allocate-info

	      (cffi:foreign-slot-value r-command-buffer-allocate-info '(:struct %vk::command-buffer-allocate-info) :p-next)
	      +nullptr+
	      
	      (cffi:foreign-slot-value r-command-buffer-allocate-info '(:struct %vk::command-buffer-allocate-info) :command-pool)
	      h-command-pool
    
	      (cffi:foreign-slot-value r-command-buffer-allocate-info '(:struct %vk::command-buffer-allocate-info) :level)
	      :primary

	      (cffi:foreign-slot-value r-command-buffer-allocate-info '(:struct %vk::command-buffer-allocate-info) :command-buffer-count)
	      1)

	(%vk::allocate-command-buffers (get-vulkan-device r)
				       r-command-buffer-allocate-info
				       rh-command-buffer)

	(cffi:with-foreign-objects ((rh-render-complete-semaphore '%vk::semaphore)
				    (r-semaphore-create-info '(:struct %vk::semaphore-create-info)))

	  (setf (cffi:mem-aref rh-render-complete-semaphore '%vk::semaphore) +nullptr+

		(cffi:foreign-slot-value r-semaphore-create-info '(:struct %vk::semaphore-create-info) :s-type)
		:semaphore-create-info

		(cffi:foreign-slot-value r-semaphore-create-info '(:struct %vk::semaphore-create-info) :p-next)
		+nullptr+

		(cffi:foreign-slot-value r-semaphore-create-info '(:struct %vk::semaphore-create-info) :flags)
		0)
		
	  (%vk::create-semaphore (get-vulkan-device r)
				 r-semaphore-create-info
				 +nullptr+
				 rh-render-complete-semaphore)

	  (let ((color-rotator 0.0f0)
		(last-time (get-internal-real-time))
		(frame-counter 0))

	    (loop while (run r)
		 
	       do (let ((temp))

		    (incf frame-counter)

		    (when (>= (- (setq temp (get-internal-real-time)) last-time) 1000)
		      (format t "~%FPS: ~S" (/ frame-counter (/ (- (get-internal-real-time) last-time) 1000.0f0)))
		      (setq frame-counter 0
			    last-time temp))

		    (begin-render w)

		    (cffi:with-foreign-object (r-command-buffer-begin-info '(:struct %vk::command-buffer-begin-info))

		      (loop for i from 0 below (cffi:foreign-type-size '(:struct %vk::command-buffer-begin-info))
			 do (setf (cffi:mem-aref r-command-buffer-begin-info :unsigned-char i) 0))

		      (setf (cffi:foreign-slot-value r-command-buffer-begin-info '(:struct %vk::command-buffer-begin-info) :s-type)
			    :command-buffer-begin-info

			    (cffi:foreign-slot-value r-command-buffer-begin-info '(:struct %vk::command-buffer-begin-info) :p-next)
			    +nullptr+
			    
			    (cffi:foreign-slot-value r-command-buffer-begin-info '(:struct %vk::command-buffer-begin-info) :flags)
			    :one-time-submit

			    (cffi:foreign-slot-value r-command-buffer-begin-info '(:struct %vk::command-buffer-begin-info) :p-inheritance-info)
			    +nullptr+
			    )

		      (%vk::begin-command-buffer (cffi:mem-aref rh-command-buffer '%vk::command-buffer)
						 r-command-buffer-begin-info)

		      (incf color-rotator 0.001)

		      (cffi:with-foreign-objects ((p-clear-values '(:union %vk::clear-value) 2)
						  (p-render-pass-begin-info '(:struct %vk::render-pass-begin-info)))


			(loop for i from 0 below 4
			   do (setf (cffi:mem-aref p-clear-values :uint32 i) 0))


			(loop for i from 0 below (cffi:foreign-type-size '(:struct %vk::render-pass-begin-info))
			   do (setf (cffi:mem-aref p-render-pass-begin-info :unsigned-char i) 0))

			
			(multiple-value-bind (width height) (get-vulkan-surface-size w)

			(setf (cffi:foreign-slot-value
			       (cffi:foreign-slot-pointer p-clear-values '(:union %vk::clear-value) :depth-stencil)
			       '(:struct %vk::clear-depth-stencil-value)
			       :depth)
			      0.0f0

			      (cffi:foreign-slot-value
			       (cffi:foreign-slot-pointer p-clear-values '(:union %vk::clear-value) :depth-stencil)
			       '(:struct %vk::clear-depth-stencil-value)
			       :stencil)
			      0

			      (cffi:mem-aref
			       (cffi:foreign-slot-pointer
				(cffi:foreign-slot-pointer (cffi:mem-aptr p-clear-values '(:union %vk::clear-value) 1)
							   '(:union %vk::clear-value)
							   :color)
				'(:union %vk::clear-color-value)
				:float-32)
			       :float
			       0)
			      (+ (/ (sin (+ color-rotator *circle-third-1*)) 2.0) 0.5)

			      (cffi:mem-aref
			       (cffi:foreign-slot-pointer
				(cffi:foreign-slot-pointer (cffi:mem-aptr p-clear-values '(:union %vk::clear-value) 1)
							   '(:union %vk::clear-value)
							   :color)
				'(:union %vk::clear-color-value)
				:float-32)
			       :float
			       1)
			      (+ (/ (sin (+ color-rotator *circle-third-2*)) 2.0) 0.5)

			      (cffi:mem-aref
			       (cffi:foreign-slot-pointer
				(cffi:foreign-slot-pointer (cffi:mem-aptr p-clear-values '(:union %vk::clear-value) 1)
							   '(:union %vk::clear-value)
							   :color)
				'(:union %vk::clear-color-value)
				:float-32)
			       :float
			       2)
			      (+ (/ (sin (+ color-rotator *circle-third-3*)) 2.0) 0.5)

			      (cffi:mem-aref
			       (cffi:foreign-slot-pointer
				(cffi:foreign-slot-pointer (cffi:mem-aptr p-clear-values '(:union %vk::clear-value) 1)
							   '(:union %vk::clear-value)
							   :color)
				'(:union %vk::clear-color-value)
				:float-32)
			       :float
			       3)
			      1.0f0
			      ;;
			      
			      (cffi:foreign-slot-value p-render-pass-begin-info '(:struct %vk::render-pass-begin-info) :s-type)
			      :render-pass-begin-info

			      (cffi:foreign-slot-value p-render-pass-begin-info '(:struct %vk::render-pass-begin-info) :p-next)
			      +nullptr+

			      (cffi:foreign-slot-value p-render-pass-begin-info '(:struct %vk::render-pass-begin-info) :render-pass)
			      (get-vulkan-render-pass w)

			      (cffi:foreign-slot-value p-render-pass-begin-info '(:struct %vk::render-pass-begin-info) :framebuffer)
			      (get-vulkan-active-framebuffer w)

			      (cffi:foreign-slot-value
			       (cffi:foreign-slot-pointer
				(cffi:foreign-slot-pointer p-render-pass-begin-info '(:struct %vk::render-pass-begin-info) :render-area)
				'(:struct %vk::rect-2d)
				:offset)
			       '(:struct %vk::offset-2d)
			       :x)
			      0

			      (cffi:foreign-slot-value
			       (cffi:foreign-slot-pointer
				(cffi:foreign-slot-pointer p-render-pass-begin-info '(:struct %vk::render-pass-begin-info) :render-area)
				'(:struct %vk::rect-2d)
				:offset)
			       '(:struct %vk::offset-2d)
			       :y)
			      0

			      (cffi:foreign-slot-value
			       (cffi:foreign-slot-pointer
				(cffi:foreign-slot-pointer p-render-pass-begin-info '(:struct %vk::render-pass-begin-info) :render-area)
				'(:struct %vk::rect-2d)
				:extent)
			       '(:struct %vk::extent-2d)
			       :width)
			      width
			      
			      (cffi:foreign-slot-value
			       (cffi:foreign-slot-pointer
				(cffi:foreign-slot-pointer p-render-pass-begin-info '(:struct %vk::render-pass-begin-info) :render-area)
				'(:struct %vk::rect-2d)
				:extent)
			       '(:struct %vk::extent-2d)
			       :height)
			      height			      
			      
			      (cffi:foreign-slot-value p-render-pass-begin-info '(:struct %vk::render-pass-begin-info) :clear-value-count)
			      2

			      (cffi:foreign-slot-value p-render-pass-begin-info '(:struct %vk::render-pass-begin-info) :p-clear-values)
			      p-clear-values))

			;;(print (cffi:mem-aref rh-command-buffer '%vk::command-buffer))

			;;(print p-clear-values)

			#+NIL
			(loop for i from 0 below (cffi:foreign-type-size '(:struct %vk::render-pass-begin-info))
			   do (let ((*print-base* 16))
				(format t "~%~A: ~A" i (cffi:mem-aref p-render-pass-begin-info :unsigned-char i))))
			
			(%vk::cmd-begin-render-pass (cffi:mem-aref rh-command-buffer '%vk::command-buffer)
						    p-render-pass-begin-info
						    :inline)

			(%vk::cmd-end-render-pass (cffi:mem-aref rh-command-buffer '%vk::command-buffer))

			(%vk::end-command-buffer (cffi:mem-aref rh-command-buffer '%vk::command-buffer))

			(cffi:with-foreign-object (p-submit-info '(:struct %vk::submit-info))

			  (setf (cffi:foreign-slot-value p-submit-info '(:struct %vk::submit-info) :s-type)
				:submit-info

				(cffi:foreign-slot-value p-submit-info '(:struct %vk::submit-info) :p-next)
				+nullptr+

				(cffi:foreign-slot-value p-submit-info '(:struct %vk::submit-info) :wait-semaphore-count)
				0

				(cffi:foreign-slot-value p-submit-info '(:struct %vk::submit-info) :p-wait-semaphores)
				+nullptr+

				(cffi:foreign-slot-value p-submit-info '(:struct %vk::submit-info) :p-wait-dst-stage-mask)
				+nullptr+

				(cffi:foreign-slot-value p-submit-info '(:struct %vk::submit-info) :command-buffer-count)
				1

				(cffi:foreign-slot-value p-submit-info '(:struct %vk::submit-info) :p-command-buffers)
				rh-command-buffer

				(cffi:foreign-slot-value p-submit-info '(:struct %vk::submit-info) :signal-semaphore-count)
				1

				(cffi:foreign-slot-value p-submit-info '(:struct %vk::submit-info) :p-signal-semaphores)
				rh-render-complete-semaphore)

			  (%vk:queue-submit (get-vulkan-queue r) 1 p-submit-info +nullptr+)

			  (end-render w rh-render-complete-semaphore 1))))))

	    (%vk::queue-wait-idle (get-vulkan-queue r))
			  
	    (%vk::destroy-semaphore (get-vulkan-device r)
				    (cffi:mem-aref rh-render-complete-semaphore '%vk::semaphore)
				    +nullptr+)
	    
	    (%vk::destroy-command-pool (get-vulkan-device r)
				       (cffi:mem-aref ph-command-pool '%vk::command-pool)
				       +nullptr+)

	    (values)))))))
						
			      
			      
			      
			      
			      
