(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :vkapp)


(defclass camera ()
  ((zoom :initform 1.0f0 :accessor zoom)
   (x-loc :initform 1.0f0 :accessor camera-x)
   (y-loc :initform 1.0f0 :accessor camera-y)
   (z-loc :initform 1.0f0 :accessor camera-z)
   (eye-x :initform 0.0f0 :accessor eye-x)
   (eye-y :initform 0.0f0 :accessor eye-y)
   (eye-z :initform 0.0f0 :accessor eye-z)))

(defun compute-picking-ray (app camera projection-matrix view-matrix)
  (with-slots (window) app
    (let ((viewport-width)
	  (viewport-height))
      (with-foreign-objects ((p-width :int)
			     (p-height :int))
	(glfwGetFramebufferSize window p-width p-height)
	(setq viewport-width (coerce (mem-aref p-width :int) 'single-float)
	      viewport-height (coerce (mem-aref p-height :int) 'single-float)))
    (let ((unprojmat (invert-4x4-matrix projection-matrix)))
      (multiple-value-bind (mouse-x mouse-y) (get-cursor-pos app)
	(let* ((ray-nds (vec3 (- (/ (* 2.0f0 mouse-x) viewport-width) 1.0f0) (- (/ (* 2.0f0 mouse-y) viewport-height) 1.0f0) 1.0f0))
	       (ray-clip1 (vec4 (vx ray-nds) (vy ray-nds) 0.0f0 1.0f0))
	       (ray-clip2 (vec4 (vx ray-nds) (vy ray-nds) 1.0f0 1.0f0)))
	  (let ((ray-eye1 (m* unprojmat ray-clip1))
		(ray-eye2 (m* unprojmat ray-clip2))
		(ray-world))
	    ;;(setq ray-eye (vec4 (vx ray-eye) (vy ray-eye) -1.0f0 0.0f0))
	    (setq ray-eye1 (m* (invert-4x4-matrix view-matrix) ray-eye1))
	    (setq ray-eye2 (m* (invert-4x4-matrix view-matrix) ray-eye2))
	    (igText "ray-eye1") (igText (format nil "~S" ray-eye1))
	    (igText "ray-eye2") (igText (format nil "~S" ray-eye2))
	    (setq ray-eye1 (vec3 (/ (vx ray-eye1) (vw ray-eye1)) (/ (vy ray-eye1) (vw ray-eye1)) (/ (vz ray-eye1) (vw ray-eye1))))
	    (setq ray-eye2 (vec3 (/ (vx ray-eye2) (vw ray-eye2)) (/ (vy ray-eye2) (vw ray-eye2)) (/ (vz ray-eye2) (vw ray-eye2))))
	    (setq ray-world (vunit (v- ray-eye1 ray-eye2)))

	    (if (not *ortho*)
		(values (vec3 (camera-x camera) (camera-y camera) (camera-z camera)) ray-world)
		(values ray-eye1 (v- (vec3 (camera-x camera) (camera-y camera) (camera-z camera))))))))))))
	    


(defvar *camera* (make-instance 'camera))

(defvar *ortho* t)   
(defvar *zoom* 1.0f0)
;;(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *camera-x* 5.0f0)
(defvar *camera-y* 5.0f0)
(defvar *camera-z* 5.0f0)
;;)

(defvar *debug* t)

(defvar *pipeline-created* nil)

(defun shader-binary-from-list (list)
  (values (foreign-alloc :uint32 :initial-contents list) (* 4 (length list))))

(defun read-shader-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
	  (byte))
      (loop while (setq byte (read-byte stream nil))
	 do (vector-push-extend byte buffer))
      (let* ((size (fill-pointer buffer))
	     (binary (foreign-alloc (list :array :unsigned-char size))))
	(loop for b across buffer for i from 0
	   do (setf (mem-aref binary :unsigned-char i) b))
	(values binary size)))))

(defcallback check-vk-result-callback :void ((result :int))
  (check-vk-result result))

(defun check-vk-result (result)
  (case result
    (#.VK_SUCCESS (values))
    (#.VK_NOT_READY   (warn "A fence or query has not yet completed.") (values))
    (#.VK_TIMEOUT     (warn "A wait operation has not completed in the specified time.") (values))
    (#.VK_EVENT_SET   (format *error-output* "An event is signaled.") (values))
    (#.VK_EVENT_RESET (format *error-output* "An even is unsignaled.") (values))
    (#.VK_INCOMPLETE  (warn "A return array was too small for the result.") (values))

    (#.VK_ERROR_OUT_OF_HOST_MEMORY
     (error "A host memory allocation has failed."))
    (#.VK_ERROR_OUT_OF_DEVICE_MEMORY
     (error "A device memory allocation has failed."))
    (#.VK_ERROR_INITIALIZATION_FAILED
     (error "Initialization of an object has failed."))
    (#.VK_ERROR_DEVICE_LOST
     (error "The logical device has been lost."))
    (#.VK_ERROR_MEMORY_MAP_FAILED
     (error "Mapping of a memory object has failed."))
    (#.VK_ERROR_LAYER_NOT_PRESENT
     (error "Layer specified does not exist."))
    (#.VK_ERROR_EXTENSION_NOT_PRESENT
     (error "Extension specified does not exist."))
    (#.VK_ERROR_FEATURE_NOT_PRESENT
     (error "Requested feature is not available on this device."))
    (#.VK_ERROR_INCOMPATIBLE_DRIVER
     (error "Unable to find vulkan driver."))
    (#.VK_ERROR_TOO_MANY_OBJECTS
     (error "Too many objects of this type have already been created."))
    (#.VK_ERROR_FORMAT_NOT_SUPPORTED
     (error "Requested format is not supported on this device."))
    (#.VK_ERROR_FRAGMENTED_POOL
     (error "A requested pool allocation has failed due to fragmentation of the pool's memory."))
    (#.VK_ERROR_SURFACE_LOST_KHR
     (error "VK_KHR_surface: VK_ERROR_SURFACE_LOST_KHR"))
    (#.VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
     (error "VK_KHR_surface: VK_ERROR_NATIVE_WINDOW_IN_USE"))
    (#.VK_SUBOPTIMAL_KHR
     (warn "VK_KHR_swapchain: VK_SUBOPTIMAL_KHR") (values))
    (#.VK_ERROR_OUT_OF_DATE_KHR
     (error "VK_KHR_swapchain: VK_ERROR_OUT_OF_DATE_KHR"))
    (#.VK_ERROR_INCOMPATIBLE_DISPLAY_KHR
     (error "VK_KHR_display_swapchain: VK_INCOMPATIBLE_DISPLAY"))
    (#.VK_ERROR_VALIDATION_FAILED_EXT
     (error "VK_EXT_debug_report: VK_ERROR_VALIDATION_FAILED"))
    (#.VK_ERROR_INVALID_SHADER_NV
     (error "VK_NV_glsl_shader: VK_ERROR_INVALID_SHADER_NV"))
    (#.VK_NV_EXTENSION_1_ERROR
     (error "VK_NV_extension_1: VK_NV_EXTENSION_1_ERROR"))
    (#.VK_ERROR_OUT_OF_POOL_MEMORY_KHR
     (error "VK_KHR_maintenance1: VK_ERROR_OUT_OF_POOL_MEMORY_KHR"))
    (#.VK_ERROR_INVALID_EXTERNAL_HANDLE_KHX
     (error "VK_KHX_external_memory: VK_ERROR_INVALID_EXTERNAL_HANDLE_KHX"))
    (t (error "Unknown VkResult: ~S" result))))

(defcallback resize-vulkan-callback :void ((window :pointer) (w :int) (h :int))
  (resize-vulkan-function window w h))

(defun resize-vulkan-function (window w h)
  (let* ((app-id (glfwGetWindowUserPointer window))
	 (app (find-app app-id)))
    (unless app
      (error "Could not find app from app-id: ~A while resizing." app-id))
    (resize-vulkan app w h)))

(defmethod get-cursor-pos ((app vkapp))
  (with-slots (window) app
    (with-foreign-objects ((p-x :double)
			   (p-y :double))
      (glfw:glfwgetcursorpos window p-x p-y)
      (values (mem-aref p-x :double)
	      (mem-aref p-y :double)))))

(defmethod create-swapchain ((app vkapp) &key (old-swapchain +nullptr+))
  (with-slots (device
	       allocator surface surface-format present-mode gpu window
	       swapchain fb-width fb-height back-buffer-count back-buffer) app

    (with-vk-struct (p-info VkSwapchainCreateInfoKHR)
      (with-foreign-slots ((vk::surface vk::imageFormat vk::imageColorSpace vk::imageArrayLayers
					vk::imageUsage vk::imageSharingMode vk::preTransform
					vk::compositeAlpha vk::presentMode vk::clipped vk::oldSwapchain
					vk::minImageCount vk::imageExtent)
			   p-info (:struct VkSwapchainCreateInfoKHR))
	(setf vk::surface surface
	      vk::imageFormat (foreign-slot-value surface-format '(:struct VkSurfaceFormatKHR) 'vk::format)
	      vk::imageColorSpace (foreign-slot-value surface-format '(:struct VkSurfaceFormatKHR) 'vk::colorSpace)
	      vk::imageArrayLayers 1
	      vk::imageUsage (logior vk::imageUsage VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
	      vk::imageSharingMode VK_SHARING_MODE_EXCLUSIVE
	      vk::preTransform VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
	      vk::compositeAlpha VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
	      vk::presentMode present-mode
	      vk::clipped VK_TRUE
	      vk::oldSwapchain old-swapchain)
	(with-vk-struct (p-cap VkSurfaceCapabilitiesKHR)
	  (check-vk-result (vkGetPhysicalDeviceSurfaceCapabilitiesKHR gpu surface p-cap))

	  (let ((cap-min-image-count
		 (foreign-slot-value p-cap '(:struct VkSurfaceCapabilitiesKHR) 'vk::minImageCount))
		(cap-max-image-count
		 (foreign-slot-value p-cap '(:struct VkSurfaceCapabilitiesKHR) 'vk::maxImageCount)))
	    (if (> (foreign-slot-value p-cap '(:struct VkSurfaceCapabilitiesKHR) 'vk::maxImageCount) 0)
		(setf vk::minImageCount (if (< (+ cap-min-image-count 2) cap-max-image-count)
					    (+ cap-min-image-count 2)
					    cap-max-image-count))
		(setf vk::minImageCount (+ cap-min-image-count 2)))
	    (let ((cap-current-extent-width
		   (foreign-slot-value
		    (foreign-slot-pointer p-cap '(:struct VkSurfaceCapabilitiesKHR) 'vk::currentExtent)
		    '(:struct VkExtent2D)
		    'vk::width)))
	      (if (eq cap-current-extent-width #xffffffff)

		  (with-foreign-objects ((p-w :int)
					 (p-h :int))
		    (glfwGetFrameBufferSize window p-w p-h)
		    (setf fb-width (mem-aref p-w :int)
			  fb-height (mem-aref p-h :int)))
		  
		  (setf fb-width cap-current-extent-width
			fb-height (foreign-slot-value
				   (foreign-slot-pointer p-cap '(:struct VkSurfaceCapabilitiesKHR) 'vk::currentExtent)
				   '(:struct VkExtent2D)
				   'vk::height)))
		
	      (setf (foreign-slot-value
		     (foreign-slot-pointer p-info '(:struct VkSwapchainCreateInfoKHR) 'vk::imageExtent)
		     '(:struct VkExtent2D)
		     'vk::width) fb-width
		       
		     (foreign-slot-value
		      (foreign-slot-pointer p-info '(:struct VkSwapchainCreateInfoKHR) 'vk::imageExtent)
		      '(:struct VkExtent2D)
		      'vk::height) fb-height)
		
	      (with-foreign-object (p-swapchain 'VkSwapchainKHR)
		(check-vk-result (vkCreateSwapchainKHR device p-info allocator p-swapchain))
		(setf swapchain (mem-aref p-swapchain 'VkSwapchainKHR)))
		
	      (with-foreign-objects ((p-back-buffer-count :uint32)
				     (p-back-buffer 'VkImage 4))
		(check-vk-result (vkGetSwapchainImagesKHR device swapchain p-back-buffer-count +nullptr+))
		(setf back-buffer-count (mem-aref p-back-buffer-count :uint32))
		(check-vk-result (vkGetSwapchainImagesKHR device swapchain p-back-buffer-count p-back-buffer))
		(loop for i from 0 below back-buffer-count
		   do (setf (elt back-buffer i) (mem-aref p-back-buffer 'VkImage i)))))))))
    (destroy-old-swapchain app old-swapchain))
  (values))

(defmethod destroy-framebuffers ((app vkapp))
  (with-slots (back-buffer-count framebuffer device allocator) app
    (loop for i from 0 below back-buffer-count
       when (not (null-pointer-p (elt framebuffer i)))
       do (vkDestroyFramebuffer device (elt framebuffer i) allocator)
	 (setf (elt framebuffer i) +nullptr+)))
  (values))

(defmethod free-command-buffers ((app vkapp))
  (with-slots (device command-pool command-buffer) app
    (loop for i from 0 below IMGUI_VK_QUEUED_FRAMES
       do (with-foreign-object (p-command-buffers-i 'VkCommandBuffer)
	    (setf (mem-aref p-command-buffers-i 'VkCommandBuffer) (elt command-buffer i))
	    (vkFreeCommandBuffers device (elt command-pool i) 1 p-command-buffers-i))
	 (setf (elt command-buffer i) nil)))
  (values))

(defmethod destroy-command-pools ((app vkapp))
  (with-slots (device command-pool allocator) app
    (loop for i from 0 below IMGUI_VK_QUEUED_FRAMES
       do (vkDestroyCommandPool device (elt command-pool i) allocator)
	 (setf (elt command-pool i) nil)))
  (values))  

(defmethod destroy-annotation-pipeline ((app vkapp))
  (with-slots (device annotation-pipeline allocator) app
    (vkDestroyPipeline device annotation-pipeline allocator)
    (setf annotation-pipeline nil))
  (values))

(defmethod destroy-selection-pipeline ((app vkapp))
  (with-slots (device selection-pipeline allocator) app
    (vkDestroyPipeline device selection-pipeline allocator)
    (setf selection-pipeline nil))
  (values))

(defmethod destroy-annotation-pipeline-layout ((app vkapp))
  (with-slots (device annotation-pipeline-layout allocator) app
    (vkDestroyPipelineLayout device annotation-pipeline-layout allocator)
    (setf annotation-pipeline-layout nil))
  (values))

(defmethod destroy-selection-pipeline-layout ((app vkapp))
  (with-slots (device selection-pipeline-layout allocator) app
    (vkDestroyPipelineLayout device selection-pipeline-layout allocator)
    (setf selection-pipeline-layout nil))
  (values))

(defmethod destroy-render-pass ((app vkapp))
  (with-slots (device render-pass allocator) app
    (when (not (null-pointer-p render-pass))
      (vkDestroyRenderPass device render-pass allocator)
      (setf render-pass +nullptr+)))
  (values))

(defmethod destroy-image-views ((app vkapp))
  (with-slots (device back-buffer-count back-buffer-view allocator) app
    (loop for i from 0 below back-buffer-count
       when (not (null-pointer-p (elt back-buffer-view i)))
       do (vkDestroyImageView device (elt back-buffer-view i) allocator)
	 (setf (elt back-buffer-view i) +nullptr+)))
  (values))

(defmethod destroy-old-swapchain ((app vkapp) old-swapchain)
  (with-slots (device allocator) app
    (unless (null-pointer-p old-swapchain)
      (vkDestroySwapchainKHR device old-swapchain allocator)))
  (values))

(defmethod destroy-descriptor-set-layout ((app vkapp))
  (with-slots (device descriptor-set-layout allocator) app
    (vkDestroyDescriptorSetLayout device descriptor-set-layout allocator))
  (values))

(defmethod destroy-selection-descriptor-set-layout ((app vkapp))
  (with-slots (device selection-descriptor-set-layout allocator) app
    (vkDestroyDescriptorSetLayout device selection-descriptor-set-layout allocator))
  (values))

(defmethod destroy-descriptor-pool ((app vkapp))
  (with-slots (device descriptor-pool allocator) app
    (vkDestroyDescriptorPool device descriptor-pool allocator))
  (values))

(defmethod destroy-uniform-buffer-vs ((app vkapp))
  (with-slots (device uniform-buffer-vs uniform-buffer-vs-memory allocator) app
    (vkDestroyBuffer device uniform-buffer-vs allocator)
    (setf uniform-buffer-vs nil)
    (vkFreeMemory device uniform-buffer-vs-memory allocator)
    (setf uniform-buffer-vs-memory nil))
  (values))

(defmethod destroy-index-buffer ((app vkapp))
  (with-slots (device index-buffer index-buffer-memory allocator index-data) app
    (vkDestroyBuffer device index-buffer allocator)
    (setf index-buffer nil)
    (vkFreeMemory device index-buffer-memory allocator)
    (setf index-buffer-memory nil)
    (foreign-free index-data))
  (values))

(defmethod destroy-vertex-buffer ((app vkapp))
  (with-slots (device vertex-buffer vertex-buffer-memory allocator vertex-data) app
    (vkDestroyBuffer device vertex-buffer allocator)
    (setf vertex-buffer nil)
    (vkFreeMemory device vertex-buffer-memory allocator)
    (setf vertex-buffer-memory nil)
    (foreign-free vertex-data)
    (setf vertex-data nil))
  (values))

(defmethod destroy-fences ((app vkapp))
  (with-slots (device fence allocator) app
    (loop for i from 0 below IMGUI_VK_QUEUED_FRAMES
       do
	 (vkDestroyFence device (elt fence i) allocator)
	 (setf (elt fence i) nil)))
  (values))  

(defmethod destroy-semaphores ((app vkapp))
  (with-slots (device
	       allocator
	       present-complete-semaphore
	       render-complete-semaphore) app
    
    (loop for i from 0 below IMGUI_VK_QUEUED_FRAMES
       do (vkDestroySemaphore device (elt present-complete-semaphore i) allocator)
	 (setf (elt present-complete-semaphore i) nil)
	 (vkDestroySemaphore device (elt render-complete-semaphore i) allocator)
	 (setf (elt render-complete-semaphore i) nil)))
  (values))

(defmethod destroy-device ((app vkapp))
  (with-slots (device allocator) app
    (vkDestroyDevice device allocator)
    (setf device nil))
  (values))

(defmethod destroy-surface ((app vkapp))
  (with-slots (instance surface allocator) app
    (vkDestroySurfaceKHR instance surface allocator)
    (setf surface nil))
  (values))

(defmethod destroy-instance ((app vkapp))
  (with-slots (instance allocator) app
    (vkDestroyInstance instance allocator)
    (setf instance nil))
  (values))

(defmethod destroy-window ((app vkapp))
  (with-slots (window) app
    (glfwDestroyWindow window)
    (setf *apps* (remove app *apps*)))
  (values))

(defmethod maybe-destroy-debug-report-callback ((app vkapp))
  (when *debug*
    (with-slots (instance debug-report allocator) app
      (vkDestroyDebugReportCallbackEXT instance instance debug-report allocator)))
  (values))

(defmethod recreate-swapchain ((app vkapp))
  (let (w h)
    (with-foreign-objects ((p-w :int)
			   (p-h :int))
      (setf (mem-aref p-w :int) 0
	    (mem-aref p-h :int) 0)
      (loop with w with h while (or (eq (mem-aref p-w :int) 0) (eq (mem-aref p-h :int) 0))
	 do (glfwGetFramebufferSize (slot-value app 'window) p-w p-h)
	   (setq w (mem-aref p-w :int)
		 h (mem-aref p-h :int))
	   (glfwWaitEvents)))

  (unless (or (eq w 0) (eq h 0))
    (with-slots (device swapchain) app
      
      (check-vk-result (vkDeviceWaitIdle device))

      (with-slots (device depth-image-view depth-image depth-image-memory allocator) app
	(vkDestroyImageView device depth-image-view allocator)
	(vkDestroyImage device depth-image allocator)
	(vkFreeMemory device depth-image-memory allocator))
      
      (destroy-framebuffers app)
      (free-command-buffers app)
      (destroy-selection-pipeline app)
      (destroy-selection-pipeline-layout app)
      (destroy-annotation-pipeline app)
      (destroy-annotation-pipeline-layout app)
      (destroy-render-pass app)
      (destroy-image-views app)

      (let ((old-swapchain swapchain))
	(setf swapchain nil)
	(create-swapchain app :old-swapchain old-swapchain))
  
      (create-image-views app)
      (create-render-pass app)
      ;;(create-selection-descriptor-set app)
      ;;(create-annotation-descriptor-set app)
      ;;(create-graphics-pipeline app)
      (create-annotation-pipeline app)
      (with-slots (selection-descriptor-set-layout) app
	(create-selection-pipeline app selection-descriptor-set-layout))
      ;;(create-selection-pipeline app)
      (create-depth-resources app)
      (create-framebuffers app)
      (create-command-buffers app)))
  (values)))

(defmethod cleanup-vulkan ((app vkapp))
  (destroy-framebuffers app)
  (free-command-buffers app)
  (destroy-command-pools app)
  (destroy-selection-pipeline app)
  (destroy-selection-pipeline-layout app)
  (destroy-render-pass app)
  (destroy-image-views app)

  (with-slots (swapchain) app
    (destroy-old-swapchain app swapchain)
    (setf swapchain nil))

  (destroy-descriptor-set-layout app)
  (destroy-selection-descriptor-set-layout app)
  (destroy-descriptor-pool app)
  (destroy-uniform-buffer-vs app)
  (destroy-index-buffer app)
  (destroy-vertex-buffer app)
  (destroy-fences app)
  (destroy-semaphores app)
  (destroy-device app)
  (destroy-surface app)
  (maybe-destroy-debug-report-callback app)
  (destroy-instance app)
  (destroy-window app)

  (deallocate-image-range app)
  (deallocate-surface-format app)

  (glfwTerminate))

(defmethod create-render-pass ((app vkapp))
  ;; Create the Render Pass:
  (with-slots (render-pass surface-format device allocator gpu) app
    (with-foreign-object (p-attachments '(:struct VkAttachmentDescription) 2)
      (let ((p-color-attachment (mem-aptr p-attachments '(:struct VkAttachmentDescription) 0))
	    (p-depth-attachment (mem-aptr p-attachments '(:struct VkAttachmentDescription) 1)))
	(vk::zero-struct p-color-attachment '(:struct VkAttachmentDescription))
	(vk::zero-struct p-depth-attachment '(:struct VkAttachmentDescription))
	(with-foreign-slots ((vk::format vk::samples vk::loadOp vk::storeOp vk::stencilLoadOp
					 vk::stencilStoreOp vk::initialLayout vk::finalLayout)
			     p-color-attachment (:struct VkAttachmentDescription))
	  (setf vk::format (foreign-slot-value surface-format '(:struct VkSurfaceFormatKHR) 'vk::format)
		vk::samples VK_SAMPLE_COUNT_1_BIT
		vk::loadOp VK_ATTACHMENT_LOAD_OP_CLEAR
		vk::storeOp VK_ATTACHMENT_STORE_OP_STORE
		vk::stencilLoadOp VK_ATTACHMENT_LOAD_OP_DONT_CARE
		vk::stencilStoreOp VK_ATTACHMENT_STORE_OP_DONT_CARE
		vk::initialLayout VK_IMAGE_LAYOUT_UNDEFINED
		vk::finalLayout VK_IMAGE_LAYOUT_PRESENT_SRC_KHR))
	(with-foreign-slots ((vk::format vk::samples vk::loadOp vk::storeOp vk::stencilLoadOp
					 vk::stencilStoreOp vk::initialLayout vk::finalLayout)
			     p-depth-attachment (:struct VkAttachmentDescription))
	  (setf vk::format (find-depth-format gpu)
		vk::samples VK_SAMPLE_COUNT_1_BIT
		vk::loadOp VK_ATTACHMENT_LOAD_OP_CLEAR
		vk::storeOp VK_ATTACHMENT_STORE_OP_DONT_CARE
		vk::stencilLoadOp VK_ATTACHMENT_LOAD_OP_DONT_CARE
		vk::stencilStoreOp VK_ATTACHMENT_STORE_OP_DONT_CARE
		vk::initialLayout VK_IMAGE_LAYOUT_UNDEFINED
		vk::finalLayout VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL))
	(with-foreign-object (p-attachment-refs '(:struct VkAttachmentReference) 2)
	  (let ((p-color-attachment-ref (mem-aptr p-attachment-refs '(:struct VkAttachmentReference) 0))
		(p-depth-attachment-ref (mem-aptr p-attachment-refs '(:struct VkAttachmentReference) 1)))
	    (vk::zero-struct p-color-attachment-ref '(:struct VkAttachmentReference))
	    (vk::zero-struct p-depth-attachment-ref '(:struct VkAttachmentReference))
	    (with-foreign-slots ((vk::attachment vk::layout)
				 p-color-attachment-ref (:struct VkAttachmentReference))
	      (setf vk::attachment 0
		    vk::layout VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL))

	    (with-foreign-slots ((vk::attachment vk::layout)
				 p-depth-attachment-ref (:struct VkAttachmentReference))
	      (setf vk::attachment 1
		    vk::layout VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL))
	  
	    
	    (with-foreign-object (p-subpasses '(:struct VkSubpassDescription) 2)
	  
	      (loop for i from 0 below 1
		 do (let ((p-subpass (mem-aptr p-subpasses '(:struct VkSubpassDescription) i)))
		      (zero-struct p-subpass '(:struct VkSubpassDescription))
		      (with-foreign-slots ((vk::pipelineBindPoint vk::colorAttachmentCount vk::pColorAttachments
								  vk::pDepthStencilAttachment)
					   p-subpasses
					   (:struct VkSubpassDescription))
			(setf vk::pipelineBindPoint VK_PIPELINE_BIND_POINT_GRAPHICS
			      vk::colorAttachmentCount 1
			      vk::pColorAttachments p-color-attachment-ref
			      vk::pDepthStencilAttachment p-depth-attachment-ref))))

	      (with-vk-struct (p-dependency VkSubpassDependency)
		(with-foreign-slots ((vk::srcSubpass
				      vk::dstSubpass
				      vk::srcStageMask
				      vk::srcAccessMask
				      vk::dstStageMask
				      vk::dstAccessMask)
				     p-dependency (:struct VkSubpassDependency))
		  (setf vk::srcSubpass VK_SUBPASS_EXTERNAL
			vk::dstSubpass 0
			vk::srcStageMask (logior VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
						 VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT)
			vk::srcAccessMask 0
			vk::dstStageMask (logior VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
						 VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT)
			vk::dstAccessMask (logior VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
						  VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)))
	      
		(with-vk-struct (p-info VkRenderPassCreateInfo)
		  (with-foreign-slots ((vk::attachmentCount vk::pAttachments vk::subpassCount vk::pSubpasses
							    vk::dependencyCount vk::pDependencies)
				       p-info (:struct VkRenderPassCreateInfo))
		    (setf vk::attachmentCount 2
			  vk::pAttachments p-attachments
			  vk::subpassCount 1
			  vk::pSubpasses p-subpasses
			  vk::dependencyCount 0
			  vk::pDependencies +nullptr+))
		  (with-foreign-object (p-render-pass 'VkRenderPass)
		    (check-vk-result (vkCreateRenderPass device p-info allocator p-render-pass))
		    (setf render-pass (mem-aref p-render-pass 'VkRenderPass)))))))))))
  (values))


(defmethod create-image-views ((app vkapp))
  ;; Create the image views:
  (with-slots (back-buffer-count back-buffer device allocator back-buffer-view surface-format
				 image-range) app
    (with-vk-struct (p-info VkImageViewCreateInfo)
      (with-foreign-slots ((vk::viewType vk::format vk::subresourceRange)
			   p-info (:struct VkImageViewCreateInfo))
	(with-foreign-slots ((vk::r vk::g vk::b vk::a)
			     (foreign-slot-pointer p-info '(:struct VkImageViewCreateInfo) 'vk::components)
			     (:struct VkComponentMapping))
		      
	  (setf vk::viewType VK_IMAGE_VIEW_TYPE_2D
		vk::format
		(foreign-slot-value surface-format '(:struct VkSurfaceFormatKHR) 'vk::format)
		vk::r VK_COMPONENT_SWIZZLE_R
		vk::g VK_COMPONENT_SWIZZLE_G
		vk::b VK_COMPONENT_SWIZZLE_B
		vk::a VK_COMPONENT_SWIZZLE_A
		vk::subresourceRange image-range)
		      
	  (loop for i from 0 below back-buffer-count
	     do (setf (foreign-slot-value p-info '(:struct VkImageViewCreateInfo) 'vk::image)
		      (elt back-buffer i))
			   
	       (with-foreign-object (p-back-buffer-view 'VkImageView)
		 (check-vk-result
		  (vkCreateImageView device p-info allocator p-back-buffer-view))
		 (setf (elt back-buffer-view i) (mem-aref p-back-buffer-view 'VkImageView))))))))
  (values))
      
(defmethod create-framebuffers ((app vkapp))
  ;; Create Framebuffers:
  (with-slots (device
	       fb-width fb-height back-buffer-count back-buffer-view
	       allocator framebuffer render-pass window depth-image-view) app
    (glfwSetFramebufferSizeCallback window (get-callback 'resize-vulkan-callback))
    (loop for i from 0 below back-buffer-count
       do (with-foreign-object (p-attachments 'VkImageView 2)
	    (setf (mem-aref p-attachments 'VkImageView 0) (elt back-buffer-view i)
		  (mem-aref p-attachments 'VkImageView 1) depth-image-view)
	    
	    (with-vk-struct (p-info VkFramebufferCreateInfo)
	      (with-foreign-slots ((vk::renderPass vk::attachmentCount vk::pAttachments
						   vk::width vk::height vk::layers)
				   p-info (:struct VkFramebufferCreateInfo))
		(setf vk::renderPass render-pass
		      vk::attachmentCount 2
		      vk::pAttachments p-attachments
		      vk::width fb-width
		      vk::height fb-height
		      vk::layers 1)
		      
		(with-foreign-object (p-framebuffer 'VkFramebuffer)
		  (check-vk-result
		   (vkCreateFramebuffer device p-info allocator p-framebuffer))
		  (setf (elt framebuffer i) (mem-aref p-framebuffer 'VkFramebuffer))))))))
  (values))



(defmethod create-shader (filename type)
  (multiple-value-bind (binary size)
      (read-shader-file filename)
    (make-instance type
		   :code binary
		   :size size)))

#+NOTYET
(defmethod create-shader-module ((shader shader) device allocator)
  (with-vk-struct (p-create-info VkShaderModuleCreateInfo)
    (with-foreign-slots ((vk::codeSize vk::pCode)
			 p-create-info (:struct VkShaderModuleCreateInfo))
      (setf vk::codeSize (shader-size shader)
	    vk::pCode (shader-code shader))
      (with-foreign-object (p-shader-module 'VkShaderModule)
	(check-vk-result (vkCreateShaderModule device p-create-info allocator p-shader-module))
	(setf (shader-module shader)
	      (mem-aref p-shader-module 'VkShaderModule)))))
  (values))

  
#+NOTYET
(defmacro with-shader-modules (app &body body)
  `(with-slots (device
		allocator
		vertex-shader-code vertex-shader-code-size
		frag-shader-code frag-shader-code-size)
       ,app
     (let ((vertex-shader-module
	    (create-shader-module device allocator vertex-shader-code vertex-shader-code-size))
	   (frag-shader-module
	    (create-shader-module device allocator frag-shader-code frag-shader-code-size)))
       (with-foreign-string (p-name "main")
	 (with-foreign-object (p-shader-stage))))))


(defmethod create-graphics-pipeline ((app vkapp))
  (with-slots (device
	       allocator
	       pipeline-layout
	       render-pass
	       graphics-pipeline
	       fb-width fb-height
	       vertex-shader-code
	       vertex-shader-code-size
	       vert-shader-module
	       fragment-shader-code
	       fragment-shader-code-size
	       frag-shader-module
	       back-buffer-count
	       descriptor-set-layout
	       pipeline-cache
	       window) app
    
    (setf vert-shader-module (create-shader-module
			      device allocator vertex-shader-code vertex-shader-code-size)
	  frag-shader-module (create-shader-module
			      device allocator fragment-shader-code fragment-shader-code-size))
    (with-foreign-string (p-name "main")
      (with-foreign-string (p-name2 "main")
	(with-foreign-object (p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 2)
	  (let ((p-vert-shader-stage-create-info
		 (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 0))
		(p-frag-shader-stage-create-info
		 (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 1)))
	    (vk::zero-struct p-vert-shader-stage-create-info '(:struct VkPipelineShaderStageCreateInfo))
	    (vk::zero-struct p-frag-shader-stage-create-info '(:struct VkPipelineShaderStageCreateInfo))
	    ;; todo: update with-vk-struct to create arrays of structs
	    (with-foreign-slots ((vk::sType vk::pNext vk::flags vk::stage vk::module vk::pName)
				 p-vert-shader-stage-create-info
				 (:struct VkPipelineShaderStageCreateInfo))
	      (setf vk::sType VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
		    vk::pNext +nullptr+
		    vk::flags 0
		    vk::stage VK_SHADER_STAGE_VERTEX_BIT
		    vk::module vert-shader-module
		    vk::pName p-name))
      
	    (with-foreign-slots ((vk::sType vk::pNext vk::flags vk::stage vk::module vk::pName)
				 p-frag-shader-stage-create-info
				 (:struct VkPipelineShaderStageCreateInfo))
	      (setf vk::sType VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
		    vk::pNext +nullptr+
		    vk::flags 0
		    vk::stage VK_SHADER_STAGE_FRAGMENT_BIT
		    vk::module frag-shader-module
		    vk::pName p-name2))

	    (with-vk-struct (p-binding-description VkVertexInputBindingDescription)
	      (with-foreign-slots ((vk::binding
				    vk::stride
				    vk::inputRate)
				   p-binding-description
				   (:struct VkVertexInputBindingDescription))
		(setf vk::binding 0
		      vk::stride (foreign-type-size '(:struct Vertex))
		      vk::inputRate VK_VERTEX_INPUT_RATE_VERTEX))
	      
	      (with-foreign-object (p-attribute-descriptions '(:struct VkVertexInputAttributeDescription) 2)
		(let ((p-attribute-description-0
		       (mem-aptr p-attribute-descriptions '(:struct VkVertexInputAttributeDescription) 0))
		      (p-attribute-description-1
		       (mem-aptr p-attribute-descriptions '(:struct VkVertexInputAttributeDescription) 1)))
		  (zero-struct p-attribute-description-0 '(:struct VkVertexInputAttributeDescription))
		  (zero-struct p-attribute-description-1 '(:struct VkVertexInputAttributeDescription))
		  (with-foreign-slots ((vk::binding
					vk::location
					vk::format
					vk::offset)
				       p-attribute-description-0
				       (:struct VkVertexInputAttributeDescription))
		    (setf vk::binding 0
			  vk::location 0
			  vk::format VK_FORMAT_R32G32B32_SFLOAT
			  vk::offset (foreign-slot-offset '(:struct Vertex) 'pos)))
		  (with-foreign-slots ((vk::binding
					vk::location
					vk::format
					vk::offset)
				       p-attribute-description-1
				       (:struct VkVertexInputAttributeDescription))
		    (setf vk::binding 0
			  vk::location 1
			  vk::format VK_FORMAT_R32G32B32_SFLOAT
			  vk::offset (foreign-slot-offset '(:struct Vertex) 'color)))
		  
		  (with-vk-struct (p-vertex-input-info VkPipelineVertexInputStateCreateInfo)
		    (with-foreign-slots ((vk::vertexBindingDescriptionCount
					  vk::pVertexBindingDescriptions
					  vk::vertexAttributeDescriptionCount
					  vk::pVertexAttributeDescriptions)
					 p-vertex-input-info
					 (:struct VkPipelineVertexInputStateCreateInfo))
		      (setf vk::vertexBindingDescriptionCount 1
			    vk::pVertexBindingDescriptions p-binding-description
			    vk::vertexAttributeDescriptionCount 2
			    vk::pVertexAttributeDescriptions p-attribute-descriptions))
	    
		    (with-vk-struct (p-input-assembly VkPipelineInputAssemblyStateCreateInfo)
		      (with-foreign-slots ((vk::topology
					    vk::primitiveRestartEnable)
					   p-input-assembly
					   (:struct VkPipelineInputAssemblyStateCreateInfo))
			(setf vk::topology VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
			      vk::primitiveRestartEnable VK_FALSE))

		      (with-vk-struct (p-viewport VkViewport)
			(with-vk-struct (p-scissor VkRect2D)
			  (with-foreign-slots ((vk::x
						vk::y
						vk::width
						vk::height
						vk::minDepth
						vk::maxDepth)
					       p-viewport (:struct VkViewport))
			    (with-foreign-objects ((p-w :int)
						   (p-h :int))
			      (glfwGetWindowSize window p-w p-h)
			      (let ((w (mem-aref p-w :int))
				    (h (mem-aref p-h :int)))
				(setf vk::x 0.0f0
				      vk::y 0.0f0
				      vk::width (coerce w 'single-float)
				      vk::height (coerce h 'single-float)
				      vk::minDepth 0.0f0
				      vk::maxDepth 1.0f0)

				(setf (foreign-slot-value
				       (foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::offset)
				       '(:struct VkOffset2D)
				       'vk::x) 0
				 
				       (foreign-slot-value
					(foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::offset)
					'(:struct VkOffset2D)
					'vk::y) 0
				  
				       (foreign-slot-value
					(foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::extent)
					'(:struct VkExtent2D)
					'vk::width) w
				   
				       (foreign-slot-value
					(foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::extent)
					'(:struct VkExtent2D)
					'vk::height) h))))
			  
			  (with-vk-struct (p-viewport-state VkPipelineViewportStateCreateInfo)
			    (with-foreign-slots ((vk::viewportCount
						  vk::pViewports
						  vk::scissorCount
						  vk::pScissors)
						 p-viewport-state
						 (:struct VkPipelineViewportStateCreateInfo))
			      (setf vk::viewportCount 1
				    vk::pViewports p-viewport
				    vk::scissorCount 1
				    vk::pScissors p-scissor))

			    (with-vk-struct (p-rasterizer VkPipelineRasterizationStateCreateInfo)
			      (with-foreign-slots ((vk::depthClampEnable
						    vk::rasterizerDiscardEnable
						    vk::polygonMode
						    vk::lineWidth
						    vk::cullMode
						    vk::frontFace
						    vk::depthBiasEnable
						    vk::depthBiasConstantFactor
						    vk::depthBiasClamp
						    vk::depthBiasSlopeFactor)
						   p-rasterizer
						   (:struct VkPipelineRasterizationStateCreateInfo))
				(setf vk::depthClampEnable VK_FALSE
				      vk::rasterizerDiscardEnable VK_FALSE
				      vk::polygonMode VK_POLYGON_MODE_FILL
				      vk::lineWidth 1.0f0
				      vk::cullMode VK_CULL_MODE_BACK_BIT
				      vk::frontFace VK_FRONT_FACE_COUNTER_CLOCKWISE
				      vk::depthBiasEnable VK_FALSE
				      vk::depthBiasConstantFactor 0.0f0
				      vk::depthBiasClamp 0.0f0
				      vk::depthBiasSlopeFactor 0.0f0))

			      (with-vk-struct (p-multisampling VKPipelineMultisampleStateCreateInfo)
				(with-foreign-slots ((vk::sampleShadingEnable
						      vk::rasterizationSamples
						      vk::minSampleShading
						      vk::pSampleMask
						      vk::alphaToCoverageEnable
						      vk::alphaToOneEnable)
						     p-multisampling
						     (:struct VKPipelineMultisampleStateCreateInfo))
				  (setf vk::sampleShadingEnable VK_FALSE
					vk::rasterizationSamples VK_SAMPLE_COUNT_1_BIT
					vk::minSampleShading 1.0f0
					vk::pSampleMask +nullptr+
					vk::alphaToCoverageEnable VK_FALSE
					vk::alphaToOneEnable VK_FALSE))

				(with-foreign-object (p-color-blend-attachments
						      '(:struct VkPipelineColorBlendAttachmentState) back-buffer-count)
				  (loop for i from 0 below back-buffer-count
				     do (vk::zero-struct (mem-aptr p-color-blend-attachments
								   '(:struct VkPipelineColorBlendAttachmentState) i)
							 '(:struct VkPipelineColorBlendAttachmentState))
				       (with-foreign-slots ((vk::colorWriteMask
							     vk::blendEnable
							     vk::srcColorBlendFactor
							     vk::dstColorBlendFactor
							     vk::colorBlendOp
							     vk::srcAlphaBlendFactor
							     vk::dstAlphaBlendFactor
							     vk::alphaBlendOp)
							    (mem-aptr p-color-blend-attachments
								      '(:struct VkPipelineColorBlendAttachmentState) i)
							    (:struct VkPipelineColorBlendAttachmentState))
					 (setf vk::colorWriteMask
					       (logior VK_COLOR_COMPONENT_R_BIT VK_COLOR_COMPONENT_G_BIT
						       VK_COLOR_COMPONENT_B_BIT VK_COLOR_COMPONENT_A_BIT)
					       vk::blendEnable VK_FALSE
					       vk::srcColorBlendFactor VK_BLEND_FACTOR_SRC_ALPHA ;;VK_BLEND_FACTOR_ONE
					       vk::dstColorBlendFactor VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA ;;VK_BLEND_FACTOR_ZERO
					       vk::colorBlendOp VK_BLEND_OP_ADD
					       vk::srcAlphaBlendFactor VK_BLEND_FACTOR_ONE
					       vk::dstAlphaBlendFactor VK_BLEND_FACTOR_ZERO
					       vk::alphaBlendOp VK_BLEND_OP_ADD)))

				  (with-vk-struct (p-color-blending VkPipelineColorBlendStateCreateInfo)
				    (with-foreign-slots ((vk::logicOpEnable
							  vk::logicOp
							  vk::attachmentCount
							  vk::pAttachments)
							 p-color-blending
							 (:struct VkPipelineColorBlendStateCreateInfo))
				      (let ((p-blend-constants
					     (foreign-slot-pointer
					      p-color-blending
					      '(:struct VkPipelineColorBlendStateCreateInfo)
					      'vk::blendConstants)))
				
					(setf vk::logicOpEnable VK_FALSE
					      vk::logicOp VK_LOGIC_OP_COPY
					      vk::attachmentCount 1 ;;back-buffer-count
					      vk::pAttachments p-color-blend-attachments
					      (mem-aref p-blend-constants :float 0) 0.0f0
					      (mem-aref p-blend-constants :float 1) 0.0f0
					      (mem-aref p-blend-constants :float 2) 0.0f0
					      (mem-aref p-blend-constants :float 3) 0.0f0)))

				    (with-foreign-object (p-dynamic-states 'VkDynamicState 2)
				      (setf (mem-aref p-dynamic-states 'VkDynamicState 0) VK_DYNAMIC_STATE_VIEWPORT
					    (mem-aref p-dynamic-states 'VkDynamicState 1) VK_DYNAMIC_STATE_SCISSOR)
				      ;;(mem-aref p-dynamic-states 'VkDynamicState 1) VK_DYNAMIC_STATE_LINE_WIDTH)
			      
				      (with-vk-struct (p-dynamic-state VkPipelineDynamicStateCreateInfo)
					(with-foreign-slots ((vk::dynamicStateCount
							      vk::pDynamicStates)
							     p-dynamic-state
							     (:struct VkPipelineDynamicStateCreateInfo))
				  
					  (setf vk::dynamicStateCount 1
						vk::pDynamicStates p-dynamic-states))

					(with-foreign-object (p-descriptor-set-layouts 'VkDescriptorSetLayout)
					  (setf (mem-aref p-descriptor-set-layouts 'VkDescriptorSetLayout) descriptor-set-layout)
					  (with-vk-struct (p-pipeline-layout-info VkPipelineLayoutCreateInfo)
					    (with-foreign-slots ((vk::setLayoutCount
								  vk::pSetLayouts
								  vk::pushConstantRangeCount
								  vk::pPushConstantRanges)
								 p-pipeline-layout-info
								 (:struct VkPipelineLayoutCreateInfo))
					      
					      (setf vk::setLayoutCount 1
						    vk::pSetLayouts p-descriptor-set-layouts
						    vk::pushConstantRangeCount 0
						    vk::pPushConstantRanges +nullptr+))
				    
					  (with-foreign-object (p-pipeline-layout 'VkPipelineLayout)
					    (check-vk-result
					     (vkCreatePipelineLayout device
								     p-pipeline-layout-info
								     allocator
								     p-pipeline-layout))
					    (setf pipeline-layout
						  (mem-aref p-pipeline-layout 'VkPipelineLayout))))

					  (with-vk-struct (p-depth-stencil VkPipelineDepthStencilStateCreateInfo)
					    (with-foreign-slots ((vk::depthTestEnable
								  vk::depthWriteEnable
								  vk::depthCompareOp
								  vk::depthBoundsTestEnable
								  vk::stencilTestEnable)
								 p-depth-stencil (:struct VkPipelineDepthStencilStateCreateInfo))
					      (setf vk::depthTestEnable VK_TRUE
						    vk::depthWriteEnable VK_TRUE
						    vk::depthCompareOp VK_COMPARE_OP_LESS
						    vk::depthBoundsTestEnable VK_FALSE
						    vk::stencilTestEnable VK_FALSE))
				    
					  (with-vk-struct (p-pipeline-info VkGraphicsPipelineCreateInfo)
					    (with-foreign-slots ((vk::flags
								  vk::stageCount
								  vk::pStages
								  vk::pVertexInputState
								  vk::pInputAssemblyState
								  vk::pViewportState
								  vk::pRasterizationState
								  vk::pMultisampleState
								  vk::pDepthStencilState
								  vk::pColorBlendState
								  vk::pDynamicState
								  vk::layout
								  vk::renderPass
								  vk::subpass
								  vk::basePipelineHandle
								  vk::basePipelineIndex)
								 p-pipeline-info
								 (:struct VkGraphicsPipelineCreateInfo))
					      (setf vk::flags 0	;; VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
						    vk::stageCount 2
						    vk::pStages p-shader-stages
						    vk::pVertexInputState p-vertex-input-info
						    vk::pInputAssemblyState p-input-assembly
						    vk::pViewportState p-viewport-state
						    vk::pRasterizationState p-rasterizer
						    vk::pMultisampleState p-multisampling
						    vk::pDepthStencilState p-depth-stencil
						    vk::pColorBlendState p-color-blending
						    vk::pDynamicState p-dynamic-state
						    vk::layout pipeline-layout
						    vk::renderPass render-pass
						    vk::subpass 0 ;; 1
						    vk::basePipelineHandle +nullptr+
						    vk::basePipelineIndex -1))

					    (with-foreign-object (p-graphics-pipeline 'VkPipeline)
					      (check-vk-result
					       (vkCreateGraphicsPipelines device pipeline-cache 1
									  p-pipeline-info allocator
									  p-graphics-pipeline))
					      (setf graphics-pipeline (mem-aref p-graphics-pipeline 'VkPipeline))))))))))))))))))))))))
    (vkDestroyShaderModule device vert-shader-module allocator)
    (vkDestroyShaderModule device frag-shader-module allocator))
  (values))

(defmethod create-annotation-pipeline ((app vkapp))
  (with-slots (device allocator pipeline-cache render-pass back-buffer-count descriptor-set-layout window
		      annotation-pipeline annotation-pipeline-layout) app
    (with-foreign-objects ((p-w :int)
			   (p-h :int))
      (glfwGetFramebufferSize window p-w p-h)
      (let ((w (mem-aref p-w :int))
	    (h (mem-aref p-h :int)))
	(with-foreign-object (p-descriptor-set-layouts 'VkDescriptorSetLayout)
	  (setf (mem-aref p-descriptor-set-layouts 'VkDescriptorSetLayout) descriptor-set-layout)
	  (multiple-value-bind (ap apl)
	      (create-pipeline-1 device allocator pipeline-cache render-pass back-buffer-count w h
				 :line-width 3.0f0 :topology VK_PRIMITIVE_TOPOLOGY_LINE_LIST
				 :descriptor-set-layouts p-descriptor-set-layouts :descriptor-set-layout-count 1)
	    (setf annotation-pipeline ap
		  annotation-pipeline-layout apl)
	    ap))))))

(defmethod create-selection-pipeline ((app vkapp) &rest descriptor-set-layouts)
  (with-slots (device allocator pipeline-cache render-pass back-buffer-count
		      window selection-pipeline selection-pipeline-layout) app
    (with-foreign-objects ((p-w :int)
			  (p-h :int))
      (glfwGetFramebufferSize window p-w p-h)
      (let ((w (mem-aref p-w :int))
	    (h (mem-aref p-h :int))
	    (n (length descriptor-set-layouts)))
	(with-foreign-object (p-descriptor-set-layouts 'VkDescriptorSetLayout n)
	  (loop for i from 0 for dsl in descriptor-set-layouts
	     ;; remember: each layout corresponds to a (set = i) in shader
	     ;; todo: do this in create-pipeline-1, like it used to be.
	     do (setf (mem-aref p-descriptor-set-layouts 'VkDescriptorSetLayout i) dsl))
	  (multiple-value-bind (sp spl)
	      (create-selection-pipeline-1 device allocator pipeline-cache render-pass back-buffer-count w h
					   :descriptor-set-layouts p-descriptor-set-layouts
					   :descriptor-set-layout-count n)
	    (setf selection-pipeline sp selection-pipeline-layout spl)
	    sp))))))

	      

#+NIL
(defmethod create-annotation-pipeline ((app vkapp))
  (with-slots (device
	       allocator
	       pipeline-layout
	       render-pass
	       annotation-pipeline
	       fb-width fb-height
	       vertex-shader-code
	       vertex-shader-code-size
	       vert-shader-module
	       fragment-shader-code
	       fragment-shader-code-size
	       frag-shader-module
	       back-buffer-count
	       descriptor-set-layout
	       pipeline-cache
	       window) app
    ;;------------------------------------------------------------------
    (setf vert-shader-module (create-shader-module
			      device allocator vertex-shader-code vertex-shader-code-size)
	  frag-shader-module (create-shader-module
			      device allocator fragment-shader-code fragment-shader-code-size))
    ;;------------------------------------------------------------------
    (with-foreign-string (p-name "main")
      (with-foreign-string (p-name2 "main")
	(with-foreign-object (p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 2)
	  (let ((p-vert-shader-stage-create-info
		 (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 0))
		(p-frag-shader-stage-create-info
		 (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 1)))
	    ;;------------------------------------------------------------------
	    (vk::zero-struct p-vert-shader-stage-create-info '(:struct VkPipelineShaderStageCreateInfo))
	    (vk::zero-struct p-frag-shader-stage-create-info '(:struct VkPipelineShaderStageCreateInfo))
	    ;;------------------------------------------------------------------
	    ;; todo: update with-vk-struct to create arrays of structs
	    ;;------------------------------------------------------------------
	    (with-foreign-slots ((vk::sType vk::pNext vk::flags vk::stage vk::module vk::pName)
				 p-vert-shader-stage-create-info
				 (:struct VkPipelineShaderStageCreateInfo))
	      (setf vk::sType VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
		    vk::pNext +nullptr+
		    vk::flags 0
		    vk::stage VK_SHADER_STAGE_VERTEX_BIT
		    vk::module vert-shader-module
		    vk::pName p-name))
	    ;;------------------------------------------------------------------
	    (with-foreign-slots ((vk::sType vk::pNext vk::flags vk::stage vk::module vk::pName)
				 p-frag-shader-stage-create-info
				 (:struct VkPipelineShaderStageCreateInfo))
	      (setf vk::sType VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
		    vk::pNext +nullptr+
		    vk::flags 0
		    vk::stage VK_SHADER_STAGE_FRAGMENT_BIT
		    vk::module frag-shader-module
		    vk::pName p-name2))
	    ;;------------------------------------------------------------------
	    (with-vk-struct (p-binding-description VkVertexInputBindingDescription)
	      (with-foreign-slots ((vk::binding
				    vk::stride
				    vk::inputRate)
				   p-binding-description
				   (:struct VkVertexInputBindingDescription))
		(setf vk::binding 0
		      vk::stride (foreign-type-size '(:struct Vertex))
		      vk::inputRate VK_VERTEX_INPUT_RATE_VERTEX))
	      ;;------------------------------------------------------------------
	      (with-foreign-object (p-attribute-descriptions '(:struct VkVertexInputAttributeDescription) 2)
		(let ((p-attribute-description-0
		       (mem-aptr p-attribute-descriptions '(:struct VkVertexInputAttributeDescription) 0))
		      (p-attribute-description-1
		       (mem-aptr p-attribute-descriptions '(:struct VkVertexInputAttributeDescription) 1)))
		  (zero-struct p-attribute-description-0 '(:struct VkVertexInputAttributeDescription))
		  (zero-struct p-attribute-description-1 '(:struct VkVertexInputAttributeDescription))
		  (with-foreign-slots ((vk::binding
					vk::location
					vk::format
					vk::offset)
				       p-attribute-description-0
				       (:struct VkVertexInputAttributeDescription))
		    (setf vk::binding 0
			  vk::location 0
			  vk::format VK_FORMAT_R32G32B32_SFLOAT
			  vk::offset (foreign-slot-offset '(:struct Vertex) 'pos)))
		  (with-foreign-slots ((vk::binding
					vk::location
					vk::format
					vk::offset)
				       p-attribute-description-1
				       (:struct VkVertexInputAttributeDescription))
		    (setf vk::binding 0
			  vk::location 1
			  vk::format VK_FORMAT_R32G32B32_SFLOAT
			  vk::offset (foreign-slot-offset '(:struct Vertex) 'color)))
		  ;;------------------------------------------------------------------		  
		  (with-vk-struct (p-vertex-input-info VkPipelineVertexInputStateCreateInfo)
		    (with-foreign-slots ((vk::vertexBindingDescriptionCount
					  vk::pVertexBindingDescriptions
					  vk::vertexAttributeDescriptionCount
					  vk::pVertexAttributeDescriptions)
					 p-vertex-input-info
					 (:struct VkPipelineVertexInputStateCreateInfo))
		      (setf vk::vertexBindingDescriptionCount 1
			    vk::pVertexBindingDescriptions p-binding-description
			    vk::vertexAttributeDescriptionCount 2
			    vk::pVertexAttributeDescriptions p-attribute-descriptions))
		    ;;------------------------------------------------------------------
		    (with-vk-struct (p-input-assembly VkPipelineInputAssemblyStateCreateInfo)
		      (with-foreign-slots ((vk::topology
					    vk::primitiveRestartEnable)
					   p-input-assembly
					   (:struct VkPipelineInputAssemblyStateCreateInfo))
			(setf vk::topology VK_PRIMITIVE_TOPOLOGY_LINE_LIST
			      vk::primitiveRestartEnable VK_FALSE))
		      ;;------------------------------------------------------------------
		      (with-vk-struct (p-viewport VkViewport)
			(with-vk-struct (p-scissor VkRect2D)
			  (with-foreign-slots ((vk::x
						vk::y
						vk::width
						vk::height
						vk::minDepth
						vk::maxDepth)
					       p-viewport (:struct VkViewport))
			    (with-foreign-objects ((p-w :int)
						   (p-h :int))
			      (glfwGetFramebufferSize window p-w p-h)
			      (let ((w (mem-aref p-w :int))
				    (h (mem-aref p-h :int)))
				(setf vk::x 0.0f0
				      vk::y 0.0f0
				      vk::width (coerce w 'single-float)
				      vk::height (coerce h 'single-float)
				      vk::minDepth 0.0f0
				      vk::maxDepth 1.0f0)
				;;------------------------------------------------------------------
				(setf (foreign-slot-value
				       (foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::offset)
				       '(:struct VkOffset2D)
				       'vk::x) 0
				 
				       (foreign-slot-value
					(foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::offset)
					'(:struct VkOffset2D)
					'vk::y) 0
				  
				       (foreign-slot-value
					(foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::extent)
					'(:struct VkExtent2D)
					'vk::width) w
				   
				       (foreign-slot-value
					(foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::extent)
					'(:struct VkExtent2D)
					'vk::height) h))))
			  ;;------------------------------------------------------------------
			  
			  (with-vk-struct (p-viewport-state VkPipelineViewportStateCreateInfo)
			    (with-foreign-slots ((vk::viewportCount
						  vk::pViewports
						  vk::scissorCount
						  vk::pScissors)
						 p-viewport-state
						 (:struct VkPipelineViewportStateCreateInfo))
			      (setf vk::viewportCount 1
				    vk::pViewports p-viewport
				    vk::scissorCount 1
				    vk::pScissors p-scissor))
			    ;;------------------------------------------------------------------
			    (with-vk-struct (p-rasterizer VkPipelineRasterizationStateCreateInfo)
			      (with-foreign-slots ((vk::depthClampEnable
						    vk::rasterizerDiscardEnable
						    vk::polygonMode
						    vk::lineWidth
						    vk::cullMode
						    vk::frontFace
						    vk::depthBiasEnable
						    vk::depthBiasConstantFactor
						    vk::depthBiasClamp
						    vk::depthBiasSlopeFactor)
						   p-rasterizer
						   (:struct VkPipelineRasterizationStateCreateInfo))
				(setf vk::depthClampEnable VK_FALSE
				      vk::rasterizerDiscardEnable VK_FALSE
				      vk::polygonMode VK_POLYGON_MODE_FILL
				      vk::lineWidth 3.0f0
				      vk::cullMode VK_CULL_MODE_NONE
				      vk::frontFace VK_FRONT_FACE_COUNTER_CLOCKWISE
				      vk::depthBiasEnable VK_FALSE
				      vk::depthBiasConstantFactor 0.0f0
				      vk::depthBiasClamp 0.0f0
				      vk::depthBiasSlopeFactor 0.0f0))
			      ;;------------------------------------------------------------------
			      (with-vk-struct (p-multisampling VKPipelineMultisampleStateCreateInfo)
				(with-foreign-slots ((vk::sampleShadingEnable
						      vk::rasterizationSamples
						      vk::minSampleShading
						      vk::pSampleMask
						      vk::alphaToCoverageEnable
						      vk::alphaToOneEnable)
						     p-multisampling
						     (:struct VKPipelineMultisampleStateCreateInfo))
				  (setf vk::sampleShadingEnable VK_FALSE
					vk::rasterizationSamples VK_SAMPLE_COUNT_1_BIT
					vk::minSampleShading 1.0f0
					vk::pSampleMask +nullptr+
					vk::alphaToCoverageEnable VK_FALSE
					vk::alphaToOneEnable VK_FALSE))
				;;------------------------------------------------------------------
				(with-foreign-object (p-color-blend-attachments
						      '(:struct VkPipelineColorBlendAttachmentState) back-buffer-count)
				  (loop for i from 0 below back-buffer-count
				     do (vk::zero-struct (mem-aptr p-color-blend-attachments
								   '(:struct VkPipelineColorBlendAttachmentState) i)
							 '(:struct VkPipelineColorBlendAttachmentState))
				       (with-foreign-slots ((vk::colorWriteMask
							     vk::blendEnable
							     vk::srcColorBlendFactor
							     vk::dstColorBlendFactor
							     vk::colorBlendOp
							     vk::srcAlphaBlendFactor
							     vk::dstAlphaBlendFactor
							     vk::alphaBlendOp)
							    (mem-aptr p-color-blend-attachments
								      '(:struct VkPipelineColorBlendAttachmentState) i)
							    (:struct VkPipelineColorBlendAttachmentState))
					 (setf vk::colorWriteMask
					       (logior VK_COLOR_COMPONENT_R_BIT VK_COLOR_COMPONENT_G_BIT
						       VK_COLOR_COMPONENT_B_BIT VK_COLOR_COMPONENT_A_BIT)
					       vk::blendEnable VK_FALSE
					       vk::srcColorBlendFactor VK_BLEND_FACTOR_SRC_ALPHA ;;VK_BLEND_FACTOR_ONE
					       vk::dstColorBlendFactor VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA ;;VK_BLEND_FACTOR_ZERO
					       vk::colorBlendOp VK_BLEND_OP_ADD
					       vk::srcAlphaBlendFactor VK_BLEND_FACTOR_ONE
					       vk::dstAlphaBlendFactor VK_BLEND_FACTOR_ZERO
					       vk::alphaBlendOp VK_BLEND_OP_ADD)))
				  ;;------------------------------------------------------------------
				  (with-vk-struct (p-color-blending VkPipelineColorBlendStateCreateInfo)
				    (with-foreign-slots ((vk::logicOpEnable
							  vk::logicOp
							  vk::attachmentCount
							  vk::pAttachments)
							 p-color-blending
							 (:struct VkPipelineColorBlendStateCreateInfo))
				      (let ((p-blend-constants
					     (foreign-slot-pointer
					      p-color-blending
					      '(:struct VkPipelineColorBlendStateCreateInfo)
					      'vk::blendConstants)))
					;;------------------------------------------------------------------
					(setf vk::logicOpEnable VK_FALSE
					      vk::logicOp VK_LOGIC_OP_COPY
					      vk::attachmentCount 1 ;;back-buffer-count
					      vk::pAttachments p-color-blend-attachments
					      (mem-aref p-blend-constants :float 0) 0.0f0
					      (mem-aref p-blend-constants :float 1) 0.0f0
					      (mem-aref p-blend-constants :float 2) 0.0f0
					      (mem-aref p-blend-constants :float 3) 0.0f0)))
				    ;;------------------------------------------------------------------
				    (with-foreign-object (p-dynamic-states 'VkDynamicState 2)
				      (setf (mem-aref p-dynamic-states 'VkDynamicState 0) VK_DYNAMIC_STATE_VIEWPORT
					    (mem-aref p-dynamic-states 'VkDynamicState 1) VK_DYNAMIC_STATE_SCISSOR)
				      ;;(mem-aref p-dynamic-states 'VkDynamicState 1) VK_DYNAMIC_STATE_LINE_WIDTH)
				      ;;------------------------------------------------------------------			      
				      (with-vk-struct (p-dynamic-state VkPipelineDynamicStateCreateInfo)
					(with-foreign-slots ((vk::dynamicStateCount
							      vk::pDynamicStates)
							     p-dynamic-state
							     (:struct VkPipelineDynamicStateCreateInfo))
					  ;;------------------------------------------------------------------
					  (setf vk::dynamicStateCount 1
						vk::pDynamicStates p-dynamic-states))
					;;------------------------------------------------------------------
					(with-foreign-object (p-descriptor-set-layouts 'VkDescriptorSetLayout)
					  (setf (mem-aref p-descriptor-set-layouts 'VkDescriptorSetLayout) descriptor-set-layout)
					  (with-vk-struct (p-pipeline-layout-info VkPipelineLayoutCreateInfo)
					    (with-foreign-slots ((vk::setLayoutCount
								  vk::pSetLayouts
								  vk::pushConstantRangeCount
								  vk::pPushConstantRanges)
								 p-pipeline-layout-info
								 (:struct VkPipelineLayoutCreateInfo))
					      ;;------------------------------------------------------------------
					      (setf vk::setLayoutCount 1
						    vk::pSetLayouts p-descriptor-set-layouts
						    vk::pushConstantRangeCount 0
						    vk::pPushConstantRanges +nullptr+))
					    ;;------------------------------------------------------------------
					  (with-foreign-object (p-pipeline-layout 'VkPipelineLayout)
					    (check-vk-result
					     (vkCreatePipelineLayout device
								     p-pipeline-layout-info
								     allocator
								     p-pipeline-layout))
					    (setf pipeline-layout
						  (mem-aref p-pipeline-layout 'VkPipelineLayout))))
					  ;;------------------------------------------------------------------
					  (with-vk-struct (p-depth-stencil VkPipelineDepthStencilStateCreateInfo)
					    (with-foreign-slots ((vk::depthTestEnable
								  vk::depthWriteEnable
								  vk::depthCompareOp
								  vk::depthBoundsTestEnable
								  vk::stencilTestEnable)
								 p-depth-stencil (:struct VkPipelineDepthStencilStateCreateInfo))
					      (setf vk::depthTestEnable VK_TRUE
						    vk::depthWriteEnable VK_TRUE
						    vk::depthCompareOp VK_COMPARE_OP_LESS
						    vk::depthBoundsTestEnable VK_FALSE
						    vk::stencilTestEnable VK_FALSE))
					    ;;------------------------------------------------------------------
					  (with-vk-struct (p-pipeline-info VkGraphicsPipelineCreateInfo)
					    (with-foreign-slots ((vk::flags
								  vk::stageCount
								  vk::pStages
								  vk::pVertexInputState
								  vk::pInputAssemblyState
								  vk::pViewportState
								  vk::pRasterizationState
								  vk::pMultisampleState
								  vk::pDepthStencilState
								  vk::pColorBlendState
								  vk::pDynamicState
								  vk::layout
								  vk::renderPass
								  vk::subpass
								  vk::basePipelineHandle
								  vk::basePipelineIndex)
								 p-pipeline-info
								 (:struct VkGraphicsPipelineCreateInfo))
					      (setf vk::flags 0	;; VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
						    vk::stageCount 2
						    vk::pStages p-shader-stages
						    vk::pVertexInputState p-vertex-input-info
						    vk::pInputAssemblyState p-input-assembly
						    vk::pViewportState p-viewport-state
						    vk::pRasterizationState p-rasterizer
						    vk::pMultisampleState p-multisampling
						    vk::pDepthStencilState p-depth-stencil
						    vk::pColorBlendState p-color-blending
						    vk::pDynamicState p-dynamic-state
						    vk::layout pipeline-layout
						    vk::renderPass render-pass
						    vk::subpass 0 ;; 1
						    vk::basePipelineHandle +nullptr+
						    vk::basePipelineIndex -1))
					    ;;------------------------------------------------------------------
					    (with-foreign-object (p-graphics-pipeline 'VkPipeline)
					      (check-vk-result
					       (vkCreateGraphicsPipelines device pipeline-cache 1
									  p-pipeline-info allocator
									  p-graphics-pipeline))
					      (setf annotation-pipeline (mem-aref p-graphics-pipeline 'VkPipeline))))))))))))))))))))))))
    (vkDestroyShaderModule device vert-shader-module allocator)
    (vkDestroyShaderModule device frag-shader-module allocator))
  (values))

(defun create-shader-module (device allocator code size)
  (with-vk-struct (p-create-info VkShaderModuleCreateInfo)
    (with-foreign-slots ((vk::codeSize vk::pCode)
			 p-create-info (:struct VkShaderModuleCreateInfo))
      (setf vk::codeSize size
	    vk::pCode code)
      (with-foreign-object (p-shader-module 'VkShaderModule)
	(check-vk-result (vkCreateShaderModule device p-create-info allocator p-shader-module))
	(mem-aref p-shader-module 'VkShaderModule)))))  

(defmethod resize-vulkan ((app vkapp) w h)
  (declare (ignore w h))
    
  (with-slots (device) app

    (check-vk-result (vkDeviceWaitIdle device))
    
    ;; destroy the old Framebuffer
    (destroy-framebuffers app)
    
    (destroy-image-views app)
    
    (destroy-render-pass app)
    
    (recreate-swapchain app)
    
    ;;(create-image-views app)
    
    ;;(create-render-pass app)

    ;;(create-graphics-pipeline app)

    ;;(create-framebuffers app)
    )
  
  (values))

(defcallback debug-report-callback VkBool32 ((flags VkDebugReportFlagsEXT) (object-type VkDebugReportObjectTypeEXT)
					     (object :uint64) (location size-t) (message-code :int32)
					     (p-layer-prefix (:pointer :char)) (p-message (:pointer :char))
					     (p-user-data :pointer))
  (debug-report-function flags object-type object location message-code p-layer-prefix p-message p-user-data))

(defun debug-report-function (flags object-type object location message-code p-layer-prefix p-message p-user-data)
  (declare (ignore flags object location message-code p-layer-prefix p-user-data))
  (format *error-output* "[vulkan] ObjectType: ~A~%Message: ~A~%~%" object-type
	  (foreign-string-to-lisp p-message))
  (finish-output *error-output*)
  VK_FALSE)

(defun copy-vk-surface-format-khr (src dst)
  (with-foreign-slots ((vk::format vk::colorSpace) dst (:struct VkSurfaceFormatKHR))
    (setf vk::format (foreign-slot-value src '(:struct VkSurfaceFormatKHR) 'vk::format)
	  vk::colorSpace (foreign-slot-value src '(:struct VkSurfaceFormatKHR) 'vk::colorSpace)))
  (values))

(defmethod setup-instance ((app vkapp))
  (with-slots (instance allocator) app
    (with-foreign-object (p-extensions-count :uint32)
      ;; create vulkan instance
      ;; todo: conditionalize this for *debug*
      (let ((pp-glfw-extensions
	     (glfwGetRequiredInstanceExtensions p-extensions-count)))
	(with-vk-struct (p-create-info VkInstanceCreateInfo)
	  (with-foreign-slots ((vk::enabledExtensionCount vk::ppEnabledExtensionNames
							  vk::enabledLayerCount
							  vk::ppEnabledLayerNames)
			       p-create-info (:struct VkInstanceCreateInfo))
	    (setf vk::enabledExtensionCount (mem-aref p-extensions-count :uint32)
		  vk::ppEnabledExtensionNames pp-glfw-extensions)
	    
	    (with-foreign-strings ((p-validation-layer-string "VK_LAYER_LUNARG_standard_validation")
				   (p-api-dump-layer-string "VK_LAYER_LUNARG_api_dump")
				   (p-extension-name "VK_EXT_debug_report"))
	      (with-foreign-object (pp-layers '(:pointer :char) 2)
		
		(setf (mem-aref pp-layers '(:pointer :char) 0) p-validation-layer-string
		      (mem-aref pp-layers '(:pointer :char) 1) p-api-dump-layer-string
		      
		      vk::enabledLayerCount (if *debug* 1 0)
		      vk::ppEnabledLayerNames (if *debug* pp-layers +nullptr+))
		
		(let ((extensions-count (mem-aref p-extensions-count :uint32)))
		  (with-foreign-object (pp-extensions '(:pointer :char) (1+ extensions-count))
		    (loop for i from 0 below extensions-count
		       do (setf (mem-aref pp-extensions '(:pointer (:pointer :char)) i)
				(mem-aref pp-glfw-extensions '(:pointer (:pointer :char)) i)))
		    (setf (mem-aref pp-extensions '(:pointer (:pointer :char)) extensions-count)
			  p-extension-name
			  
			  vk::enabledExtensionCount (1+ extensions-count)
			  vk::ppEnabledExtensionNames pp-extensions)
		    
		    (with-foreign-object (p-instance 'VkInstance)
		      (check-vk-result (vkCreateInstance p-create-info allocator p-instance))
		      (setf instance (mem-aref p-instance 'VkInstance))))))))))))
  (values))

(defmethod setup-debug-callback ((app vkapp))
  (with-slots (instance allocator debug-report) app
    (when *debug*
      ;; create the debug report callback
      (with-vk-struct (p-debug-report-create-info VkDebugReportCallbackCreateInfoEXT)
	(with-foreign-slots ((vk::flags vk::pfnCallback vk::pUserData)
			     p-debug-report-create-info (:struct VkDebugReportCallbackCreateInfoEXT))
	  (setf vk::flags (logior VK_DEBUG_REPORT_ERROR_BIT_EXT
				  VK_DEBUG_REPORT_WARNING_BIT_EXT
				  VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT)
		vk::pfnCallback (get-callback 'debug-report-callback)
		vk::pUserData +nullptr+)
	  ;; this EXT function should query the instance for a proc address and it should use it and it should work
	  ;; (lot of `should's)
	  (with-foreign-object (p-debug-report 'VkDebugReportCallbackEXT)
	    (check-vk-result (vkCreateDebugReportCallbackEXT instance instance p-debug-report-create-info
							     allocator p-debug-report))
	    (setf debug-report (mem-aref p-debug-report 'VkDebugReportCallbackEXT)))))))
  (values))

(defmethod pick-gpu ((app vkapp))
  (with-slots (instance gpu) app
    ;; Get GPU
    (with-foreign-object (p-gpu-count :uint32)
      (check-vk-result (vkEnumeratePhysicalDevices instance p-gpu-count +nullptr+))
      (let ((gpu-count (mem-aref p-gpu-count :uint32)))
	(with-foreign-object (p-gpus 'VkPhysicalDevice gpu-count)
	  (check-vk-result (vkEnumeratePhysicalDevices instance p-gpu-count p-gpus))
	  ;; todo:
	  ;; If a number >1 of GPUs got reported, you should find the best fit GPU for your purpose
	  ;; e.g. VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU if available, or with the greatest memory available, etc.
	  ;; for sake of simplicity we'll just take the first one, assuming it has a graphics queue family.
	  (setf gpu (mem-aref p-gpus 'VkPhysicalDevice 0))))))
  (values))

(defmethod find-queue-families ((app vkapp))
  (with-slots (gpu queue-family) app
    ;; get queue:
    (with-foreign-object (p-count :uint32)
      (vkGetPhysicalDeviceQueueFamilyProperties gpu p-count +nullptr+)
      (let ((count (mem-aref p-count :uint32)))
	(with-foreign-object (p-queues '(:struct VkQueueFamilyProperties) count)
	  (vkGetPhysicalDeviceQueueFamilyProperties gpu p-count p-queues)
	  (loop for i from 0 below count
	     when (logand (foreign-slot-value (mem-aptr p-queues '(:struct VkQueueFamilyProperties) i)
					      '(:struct VkQueueFamilyProperties) 'vk::queueFlags)
			  VK_QUEUE_GRAPHICS_BIT)
	     do (setf queue-family i)
	       (return))))))
  (values))

(defmethod create-logical-device ((app vkapp) &key (device-extensions (list "VK_KHR_swapchain"))
						(enable-geometry-shaders t))
  ;; Create Logical Device:
  (with-slots (gpu queue-family queue device allocator) app
    (let ((device-extension-count (length device-extensions)))
      (with-foreign-object (p-device-extensions :pointer device-extension-count)
	(loop for i from 0 for extension in device-extensions
	   do (setf (mem-aref p-device-extensions :pointer i) (foreign-string-alloc extension)))
	(let ((queue-index 0)


	      
	      (queue-count 1))
	  (with-foreign-object (p-queue-priority :float)
	    (setf (mem-aref p-queue-priority :float) 1.0f0)
	    (with-vk-struct (p-queue-info VkDeviceQueueCreateInfo)
	      (with-foreign-slots ((vk::queueFamilyIndex vk::queueCount vk::pQueuePriorities)
				   p-queue-info (:struct VkDeviceQueueCreateInfo))
		(setf vk::queueFamilyIndex queue-family
		      vk::queueCount queue-count
		      vk::pQueuePriorities p-queue-priority))
	      (with-vk-struct (p-create-info VkDeviceCreateInfo)
		(with-foreign-slots ((vk::queueCreateInfoCount vk::pQueueCreateInfos
							       vk::enabledExtensionCount
							       vk::ppEnabledExtensionNames
							       vk::pEnabledFeatures)
				     p-create-info (:struct VkDeviceCreateInfo))
		  (with-vk-struct (p-enabled-features VkPhysicalDeviceFeatures)
		    (setf (foreign-slot-value p-enabled-features '(:struct VkPhysicalDeviceFeatures) 'vk::geometryShader)
			  (if enable-geometry-shaders VK_TRUE VK_FALSE))
		  (setf vk::queueCreateInfoCount 1
			vk::pQueueCreateInfos p-queue-info
			vk::enabledExtensionCount device-extension-count
			vk::ppEnabledExtensionNames p-device-extensions
			vk::pEnabledFeatures p-enabled-features))
		  
		  (with-foreign-object (p-device 'VkDevice)
		    (check-vk-result (vkCreateDevice gpu p-create-info allocator p-device))
		    (setf device (mem-aref p-device 'VkDevice))
		    (with-foreign-object (p-queue 'VkQueue)
		      (vkGetDeviceQueue device queue-family queue-index p-queue)
		      (setf queue (mem-aref p-queue 'VkQueue)))))))))
	(loop for i from 0 below device-extension-count
	   do (foreign-string-free (mem-aref p-device-extensions :pointer i))))))
  (values))

(defmethod create-surface ((app vkapp))
  (with-slots (instance window allocator surface) app
    ;; Create window surface
    (with-foreign-object (p-surface 'VkSurfaceKHR)
      (check-vk-result (vkappui::glfwCreateWindowSurface instance window allocator p-surface))
      (setf surface (mem-aref p-surface 'VkSurfaceKHR))))
  (values))

(defmethod create-descriptor-pool ((app vkapp))
  ;; create descriptor pool:
  (with-slots (device allocator descriptor-pool) app
    (with-foreign-object (p-pool-sizes '(:struct VkDescriptorPoolSize) 11)
      (setf (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 0)
				'(:struct VkDescriptorPoolSize) 'vk::type)
	    VK_DESCRIPTOR_TYPE_SAMPLER
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 0)
				'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	    1000
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 1)
				'(:struct VkDescriptorPoolSize) 'vk::type)
	    VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 1)
				'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	    1000
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 2)
				'(:struct VkDescriptorPoolSize) 'vk::type)
	    VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 2)
				'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	    1000
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 3)
				'(:struct VkDescriptorPoolSize) 'vk::type)
	    VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 3)
				'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	    1000
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 4)
				'(:struct VkDescriptorPoolSize) 'vk::type)
	    VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 4)
				'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	    1000
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 5)
				'(:struct VkDescriptorPoolSize) 'vk::type)
	    VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 5)
				'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	    1000
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 6)
				'(:struct VkDescriptorPoolSize) 'vk::type)
	    VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 6)
				'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	    1000
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 7)
				'(:struct VkDescriptorPoolSize) 'vk::type)
	    VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 7)
				'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	    1000
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 8)
				'(:struct VkDescriptorPoolSize) 'vk::type)
	    VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 8)
				'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	    1000
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 9)
				'(:struct VkDescriptorPoolSize) 'vk::type)
	    VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 9)
				'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	    1000
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 10)
				'(:struct VkDescriptorPoolSize) 'vk::type)
	    VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
	    (foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 10)
				'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	    1000
	    ;;(foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 11)
	;;			'(:struct VkDescriptorPoolSize) 'vk::type)
	  ;;  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
	    ;;(foreign-slot-value (mem-aptr p-pool-sizes '(:struct VkDescriptorPoolSize) 11)
	;;			'(:struct VkDescriptorPoolSize) 'vk::descriptorCount)
	  ;;  1
	    )

      (with-vk-struct (p-pool-info VkDescriptorPoolCreateInfo)
	(with-foreign-slots ((vk::flags vk::maxSets vk::poolSizeCount vk::pPoolSizes)
			     p-pool-info (:struct VkDescriptorPoolCreateInfo))
	  (setf vk::flags 0;;VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
		vk::maxSets (* 11 1000)
		vk::poolSizeCount 11
		vk::pPoolSizes p-pool-sizes)
	  (with-foreign-object (p-descriptor-pool 'VkDescriptorPool)
	    (check-vk-result (vkCreateDescriptorPool device p-pool-info allocator p-descriptor-pool))
	    (setf descriptor-pool (mem-aref p-descriptor-pool 'VkDescriptorPool)))))))

  (values))

(defmethod create-command-pools ((app vkapp))
  (with-slots (device allocator queue-family command-pool) app
    (loop for i from 0 below IMGUI_VK_QUEUED_FRAMES
       do (with-vk-struct (p-info VkCommandPoolCreateInfo)
	    (with-foreign-slots ((vk::flags vk::queueFamilyIndex)
				 p-info (:struct VkCommandPoolCreateInfo))
	      (setf vk::flags VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
		    vk::queueFamilyIndex queue-family)
	      (with-foreign-object (p-command-pool 'VkCommandPool)
		(check-vk-result (vkCreateCommandPool device p-info allocator p-command-pool))
		(setf (elt command-pool i) (mem-aref p-command-pool 'VkCommandPool)))))))
  (values))

(defmethod create-command-buffers ((app vkapp))
  ;; Create Command Buffers:
  (with-slots (device allocator command-pool command-buffer fence
		      present-complete-semaphore render-complete-semaphore) app
    (loop for i from 0 below IMGUI_VK_QUEUED_FRAMES
       do (with-vk-struct (p-info VkCommandBufferAllocateInfo)
	    (with-foreign-slots ((vk::commandPool vk::level vk::commandBufferCount)
				 p-info (:struct VkCommandBufferAllocateInfo))
	      (setf vk::commandPool (elt command-pool i)
		    vk::level VK_COMMAND_BUFFER_LEVEL_PRIMARY
		    vk::commandBufferCount 1)
	      (with-foreign-object (p-command-buffer 'VkCommandBuffer)
		(check-vk-result (vkAllocateCommandBuffers device p-info p-command-buffer))
		(setf (elt command-buffer i) (mem-aref p-command-buffer 'VkCommandBuffer)))))

	 (with-vk-struct (p-info VkFenceCreateInfo)
	   (with-foreign-slots ((vk::flags) p-info (:struct VkFenceCreateInfo))
	     (setf vk::flags VK_FENCE_CREATE_SIGNALED_BIT)
	     (with-foreign-object (p-fence 'VkFence)
	       (check-vk-result (vkCreateFence device p-info allocator p-fence))
	       (setf (elt fence i) (mem-aref p-fence 'VkFence)))))

	 (with-vk-struct (p-info VkSemaphoreCreateInfo)
	   (with-foreign-objects ((p-present-complete-semaphore 'VkSemaphore)
				  (p-render-complete-semaphore 'VkSemaphore))
	     (check-vk-result (vkCreateSemaphore device p-info allocator p-present-complete-semaphore))
	     (setf (elt present-complete-semaphore i) (mem-aref p-present-complete-semaphore 'VkSemaphore))
	     (check-vk-result (vkCreateSemaphore device p-info allocator p-render-complete-semaphore))
	     (setf (elt render-complete-semaphore i) (mem-aref p-render-complete-semaphore 'VkSemaphore))))))
  (values))

(defmethod check-for-wsi-support ((app vkapp))
  ;; Check for WSI support
  (with-slots (gpu queue-family surface) app
    (with-foreign-object (p-res 'VkBool32)
      (vkGetPhysicalDeviceSurfaceSupportKHR gpu queue-family surface p-res)
      (when (not (eq (mem-aref p-res 'VkBool32) VK_TRUE))
	(error "No WSI support on physical device 0")))))

(defmethod get-surface-format ((app vkapp))
  ;; Get Surface Format:
  ;; Per Spec Format and View Format are expected to be the same unless VK_IMAGE_CREATE_MUTABLE_BIT was set at image creation
  ;; Assuming that the default behavior is without setting this bit, there is no need for separate Spawchain image and image view format
  ;; additionally several new color spaces were introduced with Vulkan Spec v1.0.40
  ;; hence we must make sure that a format with the mostly available color space, VK_COLOR_SPACE_SRGB_NONLINEAR_KHR, is found and used
  (with-slots (gpu surface surface-format) app
    (with-foreign-object (p-count :uint32)
      (vkGetPhysicalDeviceSurfaceFormatsKHR gpu surface p-count +nullptr+)
      (let ((count (mem-aref p-count :uint32)))
	(with-foreign-object (p-formats '(:struct VkSurfaceFormatKHR) count)
	  (vkGetPhysicalDeviceSurfaceFormatsKHR gpu surface p-count p-formats)
	  ;; first check if only one format, VK_FORMAT_UNDEFINED, is available,
	  ;; which would imply that any format is available
	  (if (eq count 1)
	      (if (eq (foreign-slot-value (mem-aptr p-formats '(:struct VkSurfaceFormatKHR) 0)
					  '(:struct VkSurfaceFormatKHR) 'vk::format)
		      VK_FORMAT_UNDEFINED)
		  
		  (setf (foreign-slot-value surface-format '(:struct VkSurfaceFormatKHR) 'vk::format)
			VK_FORMAT_B8G8R8A8_UNORM
			(foreign-slot-value surface-format '(:struct VkSurfaceFormatKHR) 'vk::colorSpace)
			VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)
		  ;; no point in search for another format
		  (copy-vk-surface-format-khr (mem-aptr p-formats '(:struct VkSurfaceFormatKHR) 0)
					      surface-format))
	      ;; request several formats, the first found will be used
	      (let ((request-surface-image-format
		     (make-array 4 :initial-element VK_FORMAT_B8G8R8A8_UNORM))
		    (request-surface-color-space VK_COLOR_SPACE_SRGB_NONLINEAR_KHR))
		(block find-requested
		  (loop for i from 0 below 4
		     do (loop for j from 0 below count
			   do (let ((p-format-j (mem-aptr p-formats '(:struct VkSurfaceFormatKHR) j)))
				(when (and (eq (foreign-slot-value p-format-j
								   '(:struct VkSurfaceFormatKHR) 'vk::format)
					       (elt request-surface-image-format i))
					   (eq (foreign-slot-value p-format-j
								   '(:struct VkSurfaceFormatKHR) 'vk::colorSpace)
					       request-surface-color-space))
				  
				  (copy-vk-surface-format-khr p-format-j surface-format)
				  (return-from find-requested)))))
		  ;; if none of the requested image formats could be found, use the first available
		  (copy-vk-surface-format-khr (mem-aptr p-formats '(:struct VkSurfaceFormatsKHR) 0)
					      surface-format))))))))
  (values))

(defmethod get-present-mode ((app vkapp))
  ;; Get present mode
  ;; request a certain mode and confirm that it is available.  If not use VK_PRESENT_MODE_FIFO_KHR which is mandatory
  (with-slots (gpu surface present-mode) app
    (if *imgui-unlimited-frame-rate*
	(setf present-mode VK_PRESENT_MODE_IMMEDIATE_KHR)
	(setf present-mode VK_PRESENT_MODE_FIFO_KHR))

    (with-foreign-object (p-count :uint32)
      (vkGetPhysicalDeviceSurfacePresentModesKHR gpu surface p-count +nullptr+)
      (let ((count (mem-aref p-count :uint32)))
	(with-foreign-object (p-present-modes 'VkPresentModeKHR count)
	  (vkGetPhysicalDeviceSurfacePresentModesKHR gpu surface p-count p-present-modes)
	  (loop for i from 0 below count
	     when (eq (mem-aref p-present-modes 'VkPresentModeKHR i)
		      present-mode)
	     do (return)
	     finally (setf present-mode VK_PRESENT_MODE_FIFO_KHR)))))) ;; always available
  (values))



(defmethod create-descriptor-set-layout ((app vkapp))
  (with-slots (device
	       allocator
	       descriptor-set-layout) app
    (with-vk-struct (p-ubo-layout-binding VkDescriptorSetLayoutBinding)
      (with-foreign-slots ((vk::binding
			    vk::descriptorType
			    vk::descriptorCount
			    vk::stageFlags
			    vk::pImmutableSamplers)
			   p-ubo-layout-binding
			   (:struct VkDescriptorSetLayoutBinding))
	(setf vk::binding 0
	      vk::descriptorType VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
	      vk::descriptorCount 1
	      vk::stageFlags VK_SHADER_STAGE_VERTEX_BIT
	      vk::pImmutableSamplers +nullptr+)

	  (with-vk-struct (p-layout-info VkDescriptorSetLayoutCreateInfo)
	    (with-foreign-slots ((vk::bindingCount
				  vk::pBindings)
				 p-layout-info (:struct VkDescriptorSetLayoutCreateInfo))
	      (setf vk::bindingCount 1
		    vk::pBindings p-ubo-layout-binding)
	      
	      (with-foreign-object (p-descriptor-set-layout 'VkDescriptorSetLayout)
		(check-vk-result (vkCreateDescriptorSetLayout device p-layout-info allocator p-descriptor-set-layout))
		(setf descriptor-set-layout (mem-aref p-descriptor-set-layout 'VkDescriptorSetLayout))))))))
  (values))

(defmethod create-selection-descriptor-set-layouts ((app vkapp))
  (with-slots (device allocator selection-descriptor-set-layout) app
    (with-foreign-object (p-bindings '(:struct VkDescriptorSetLayoutBinding) 2)
      (let ((p-ubo-vs-layout-binding (mem-aptr p-bindings '(:struct VkDescriptorSetLayoutBinding) 0))
	    (p-ubo-gs-layout-binding (mem-aptr p-bindings '(:struct VkDescriptorSetLayoutBinding) 1))
	    #+NIL(p-ssbo-gs-layout-binding (mem-aptr p-bindings '(:struct VkDescriptorSetLayoutBinding) 2)))
	(zero-struct p-ubo-vs-layout-binding '(:struct VkDescriptorSetLayoutBinding))
	(zero-struct p-ubo-gs-layout-binding '(:struct VkDescriptorSetLayoutBinding))
	#+NIL(zero-struct p-ssbo-gs-layout-binding '(:struct VkDescriptorSetLayoutBinding))
	(with-foreign-slots ((vk::binding
			      vk::descriptorType
			      vk::descriptorCount
			      vk::stageFlags
			      vk::pImmutableSamplers)
			     p-ubo-vs-layout-binding
			     (:struct VkDescriptorSetLayoutBinding))
	  (setf vk::binding 0
		vk::descriptorType VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
		vk::descriptorCount 1
		vk::stageFlags VK_SHADER_STAGE_VERTEX_BIT
		vk::pImmutableSamplers +nullptr+))
	
	(with-foreign-slots ((vk::binding
			      vk::descriptorType
			      vk::descriptorCount
			      vk::stageFlags
			      vk::pImmutableSamplers)
			     p-ubo-gs-layout-binding
			     (:struct VkDescriptorSetLayoutBinding))
	  (setf vk::binding 1
		vk::descriptorType VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
		vk::descriptorCount 1
		vk::stageFlags VK_SHADER_STAGE_GEOMETRY_BIT
		vk::pImmutableSamplers +nullptr+))
	#+NIL
	(with-foreign-slots ((vk::binding
			      vk::descriptorType
			      vk::descriptorCount
			      vk::stageFlags
			      vk::pImmutableSamplers)
			     p-ssbo-gs-layout-binding
			     (:struct VkDescriptorSetLayoutBinding))
	  (setf vk::binding 2
		vk::descriptorType VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
		vk::descriptorCount 0
		vk::stageFlags VK_SHADER_STAGE_GEOMETRY_BIT
		vk::pImmutableSamplers +nullptr+))
	(with-vk-struct (p-layout-info VkDescriptorSetLayoutCreateInfo)
	  (with-foreign-slots ((vk::bindingCount
				vk::pBindings)
			       p-layout-info (:struct VkDescriptorSetLayoutCreateInfo))
	    (setf vk::bindingCount 2
		  vk::pBindings p-bindings))
	  
	  (with-foreign-object (p-descriptor-set-layout 'VkDescriptorSetLayout)
	    (check-vk-result (vkCreateDescriptorSetLayout device p-layout-info allocator p-descriptor-set-layout))
	    (setf selection-descriptor-set-layout (mem-aref p-descriptor-set-layout 'VkDescriptorSetLayout)))))))
  (values))

(defmethod create-selection-descriptor-set ((app vkapp))
  (with-slots (device
	       allocator
	       descriptor-set
	       descriptor-pool
	       uniform-buffer-vs
	       uniform-buffer-gs
	       descriptor-set-layout
	       selection-descriptor-set-layout) app
    (with-foreign-object (p-descriptor-set-layout 'VkDescriptorSetLayout 1)
      (setf ;;(mem-aref p-descriptor-set-layout 'VkDescriptorSetLayout 0) descriptor-set-layout
	    (mem-aref p-descriptor-set-layout 'VkDescriptorSetLayout 0) selection-descriptor-set-layout)
      (with-vk-struct (p-alloc-info VkDescriptorSetAllocateInfo)
	(with-foreign-slots ((vk::descriptorPool
			      vk::descriptorSetCount
			      vk::pSetLayouts)
			     p-alloc-info (:struct VkDescriptorSetAllocateInfo))
	  (setf vk::descriptorPool descriptor-pool
		vk::descriptorSetCount 1
		vk::pSetLayouts p-descriptor-set-layout)

	  (with-foreign-object (p-descriptor-set 'VkDescriptorSet)
	    (check-vk-result (vkAllocateDescriptorSets device p-alloc-info p-descriptor-set))
	    (setf descriptor-set (mem-aref p-descriptor-set 'VkDescriptorSet)))

	  (with-foreign-object (p-buffer-info '(:struct VkDescriptorBufferInfo) 2)
	    (let ((p-uniform-vs-buffer-info (mem-aptr p-buffer-info '(:struct VkDescriptorBufferInfo) 0))
		  (p-uniform-gs-buffer-info (mem-aptr p-buffer-info '(:struct VkDescriptorBufferInfo) 1)))
	      (zero-struct p-uniform-vs-buffer-info '(:struct VkDescriptorBufferInfo))
	      (zero-struct p-uniform-gs-buffer-info '(:struct VkDescriptorBufferInfo))
	      (with-foreign-slots ((vk::buffer
				    vk::uniform-buffer
				    vk::offset
				    vk::range)
				   p-uniform-vs-buffer-info (:struct VkDescriptorBufferInfo))
		(setf vk::buffer uniform-buffer-vs
		      vk::offset 0
		      vk::range (foreign-type-size '(:struct UniformBufferObjectVertexShader))))

	      (with-foreign-slots ((vk::buffer
				    vk::uniform-buffer
				    vk::offset
				    vk::range)
				   p-uniform-gs-buffer-info (:struct VkDescriptorBufferInfo))
		(setf vk::buffer uniform-buffer-gs
		      vk::offset 0
		      vk::range (foreign-type-size '(:struct UniformBufferObjectGeometryShader))))
	    
	    (with-foreign-object (p-descriptor-write '(:struct VkWriteDescriptorSet) 2)
	      (zero-struct (mem-aptr p-descriptor-write '(:struct VkWriteDescriptorSet) 0) '(:struct VkWriteDescriptorSet))
	      (zero-struct (mem-aptr p-descriptor-write '(:struct VkWriteDescriptorSet) 1) '(:struct VkWriteDescriptorSet))
	      (with-foreign-slots ((vk::sType
				    vk::dstSet
				    vk::dstBinding
				    vk::dstArrayElement
				    vk::descriptorType
				    vk::descriptorCount
				    vk::pBufferInfo
				    vk::pImageInfo
				    vk::pTexelBufferView)
				   (mem-aptr p-descriptor-write '(:struct VkWriteDescriptorSet) 0)
				   (:struct VkWriteDescriptorSet))
		(setf vk::sType VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
		      vk::dstSet descriptor-set
		      vk::dstBinding 0
		      vk::dstArrayElement 0
		      vk::descriptorType VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
		      vk::descriptorCount 1
		      vk::pBufferInfo p-uniform-vs-buffer-info
		      vk::pImageInfo +nullptr+
		      vk::pTexelBufferView +nullptr+))

	      (vkUpdateDescriptorSets device 1 p-descriptor-write 0 +nullptr+)
	      
	      (with-foreign-slots ((vk::sType
				    vk::dstSet
				    vk::dstBinding
				    vk::dstArrayElement
				    vk::descriptorType
				    vk::descriptorCount
				    vk::pBufferInfo
				    vk::pImageInfo
				    vk::pTexelBufferView)
				   (mem-aptr p-descriptor-write '(:struct VkWriteDescriptorSet) 1)
				   (:struct VkWriteDescriptorSet))
		(setf vk::sType VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
		      vk::dstSet descriptor-set
		      vk::dstBinding 1
		      vk::dstArrayElement 0
		      vk::descriptorType VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
		      vk::descriptorCount 1
		      vk::pBufferInfo p-uniform-gs-buffer-info
		      vk::pImageInfo +nullptr+
		      vk::pTexelBufferView +nullptr+))

	      (vkUpdateDescriptorSets device 1 (mem-aptr p-descriptor-write '(:struct VkWriteDescriptorSet) 1) 0 +nullptr+))
	    ))))))
  (values))

(defmethod create-descriptor-set ((app vkapp))
  (with-slots (device
	       allocator
	       descriptor-set
	       descriptor-pool
	       uniform-buffer-vs
	       descriptor-set-layout
	       selection-descriptor-set-layout) app
    (with-foreign-object (p-descriptor-set-layout 'VkDescriptorSetLayout 1)
      (setf ;;(mem-aref p-descriptor-set-layout 'VkDescriptorSetLayout 0) descriptor-set-layout
	    (mem-aref p-descriptor-set-layout 'VkDescriptorSetLayout 0) selection-descriptor-set-layout)
      (with-vk-struct (p-alloc-info VkDescriptorSetAllocateInfo)
	(with-foreign-slots ((vk::descriptorPool
			      vk::descriptorSetCount
			      vk::pSetLayouts)
			     p-alloc-info (:struct VkDescriptorSetAllocateInfo))
	  (setf vk::descriptorPool descriptor-pool
		vk::descriptorSetCount 1
		vk::pSetLayouts p-descriptor-set-layout)

	  (with-foreign-object (p-descriptor-set 'VkDescriptorSet)
	    (check-vk-result (vkAllocateDescriptorSets device p-alloc-info p-descriptor-set))
	    (setf descriptor-set (mem-aref p-descriptor-set 'VkDescriptorSet)))

	  (with-vk-struct (p-buffer-info VkDescriptorBufferInfo)
	    (with-foreign-slots ((vk::buffer
				  vk::uniform-buffer
				  vk::offset
				  vk::range)
				 p-buffer-info (:struct VkDescriptorBufferInfo))
	      (setf vk::buffer uniform-buffer-vs
		    vk::offset 0
		    vk::range (foreign-type-size '(:struct UniformBufferObjectVertexShader))))

	    (with-vk-struct (p-descriptor-write VkWriteDescriptorSet)
	      (with-foreign-slots ((vk::dstSet
				    vk::dstBinding
				    vk::dstArrayElement
				    vk::descriptorType
				    vk::descriptorCount
				    vk::pBufferInfo
				    vk::pImageInfo
				    vk::pTexelBufferView)
				   p-descriptor-write (:struct VkWriteDescriptorSet))
		(setf vk::dstSet descriptor-set
		      vk::dstBinding 0
		      vk::dstArrayElement 0
		      vk::descriptorType VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
		      vk::descriptorCount 1
		      vk::pBufferInfo p-buffer-info
		      vk::pImageInfo +nullptr+
		      vk::pTexelBufferView +nullptr+))

	      (vkUpdateDescriptorSets device 1 p-descriptor-write 0 +nullptr+)))))))
  (values))

(defmethod create-annotation-descriptor-set ((app vkapp))
  (with-slots (device
	       allocator
	       annotation-descriptor-set
	       descriptor-pool
	       annotation-uniform-buffer-vs
	       descriptor-set-layout) app
    (with-foreign-object (p-descriptor-set-layout 'VkDescriptorSetLayout)
      (setf (mem-aref p-descriptor-set-layout 'VkDescriptorSetLayout) descriptor-set-layout)
      (with-vk-struct (p-alloc-info VkDescriptorSetAllocateInfo)
	(with-foreign-slots ((vk::descriptorPool
			      vk::descriptorSetCount
			      vk::pSetLayouts)
			     p-alloc-info (:struct VkDescriptorSetAllocateInfo))
	  (setf vk::descriptorPool descriptor-pool
		vk::descriptorSetCount 1
		vk::pSetLayouts p-descriptor-set-layout)

	  (with-foreign-object (p-descriptor-set 'VkDescriptorSet)
	    (check-vk-result (vkAllocateDescriptorSets device p-alloc-info p-descriptor-set))
	    (setf annotation-descriptor-set (mem-aref p-descriptor-set 'VkDescriptorSet)))

	  (with-vk-struct (p-buffer-info VkDescriptorBufferInfo)
	    (with-foreign-slots ((vk::buffer
				  vk::uniform-buffer
				  vk::offset
				  vk::range)
				 p-buffer-info (:struct VkDescriptorBufferInfo))
	      (setf vk::buffer annotation-uniform-buffer-vs
		    vk::offset 0
		    vk::range (foreign-type-size '(:struct UniformBufferObjectVertexShader))))

	    (with-vk-struct (p-descriptor-write VkWriteDescriptorSet)
	      (with-foreign-slots ((vk::dstSet
				    vk::dstBinding
				    vk::dstArrayElement
				    vk::descriptorType
				    vk::descriptorCount
				    vk::pBufferInfo
				    vk::pImageInfo
				    vk::pTexelBufferView)
				   p-descriptor-write (:struct VkWriteDescriptorSet))
		(setf vk::dstSet annotation-descriptor-set
		      vk::dstBinding 0
		      vk::dstArrayElement 0
		      vk::descriptorType VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
		      vk::descriptorCount 1
		      vk::pBufferInfo p-buffer-info
		      vk::pImageInfo +nullptr+
		      vk::pTexelBufferView +nullptr+))

	      (vkUpdateDescriptorSets device 1 p-descriptor-write 0 +nullptr+)))))))
  (values))
	    
(defmethod setup-vulkan ((app vkapp) w h)

  (read-in-shaders app)

  (allocate-surface-format app)

  (allocate-image-range app)

  (setup-window app w h)

  (setup-instance app)

  (setup-debug-callback app)

  (create-surface app)

  (pick-gpu app)

  ;; todo: find queue families should be inside pick gpu:
  (find-queue-families app)

  (check-for-wsi-support app)

  (get-surface-format app)

  (get-present-mode app)

  (create-logical-device app)

  (create-swapchain app)
      
  (create-image-views app)
    
  (create-render-pass app)

  (create-descriptor-set-layout app)
  (create-selection-descriptor-set-layouts app)
    
  ;;(create-graphics-pipeline app)
  (with-slots (selection-descriptor-set-layout) app
    (create-selection-pipeline app selection-descriptor-set-layout))
  (create-annotation-pipeline app)
  
    
  (create-command-pools app)

  (create-depth-resources app)

  (create-framebuffers app)

  (create-vertex-buffer app)
  (create-index-buffer app)

  (create-axes-vertex-buffer app)
  (create-axes-index-buffer app)
  
  (create-uniform-buffer-vs app)
  (create-annotation-uniform-buffer-vs app)
  (create-selection-uniform-buffer-gs app)

  (create-descriptor-pool app)
    
  (create-selection-descriptor-set app)
  (create-annotation-descriptor-set app)
    
  (create-command-buffers app) ;; this also creates semaphores...todo: separate these out
    
  (values))

(defun find-supported-format (gpu candidates tiling features)
  (loop for format in candidates
     do (with-vk-struct (p-props VkFormatProperties)
	  (vkGetPhysicalDeviceFormatProperties gpu format p-props)
	  (with-foreign-slots ((vk::linearTilingFeatures
				vk::optimalTilingFeatures)
			       p-props (:struct VkFormatProperties))
	    (when (and (eq tiling VK_IMAGE_TILING_LINEAR)
		       (eq (logand vk::linearTilingFeatures features) features))
	      (return format))
	    (when (and (eq tiling VK_IMAGE_TILING_OPTIMAL)
		       (eq (logand vk::optimalTilingFeatures features) features))
	      (return format))))
     finally (error "Failed to find supported format.")))

(defun find-depth-format (gpu)
  (find-supported-format gpu (list VK_FORMAT_D32_SFLOAT VK_FORMAT_D32_SFLOAT_S8_UINT VK_FORMAT_D24_UNORM_S8_UINT)
			 VK_IMAGE_TILING_OPTIMAL VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT))

(defun has-stencil-component-p (format)
  (or (eq format VK_FORMAT_D32_SFLOAT_S8_UINT)
      (eq format VK_FORMAT_D24_UNORM_S8_UINT)))


(defmethod create-depth-resources ((app vkapp))
  (with-slots (gpu fb-width fb-height depth-image depth-image-memory) app
    (let ((depth-format (find-depth-format gpu)))
      (create-depth-image app
			  :format depth-format
			  :tiling VK_IMAGE_TILING_OPTIMAL
			  :usage VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
			  :properties VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
      (create-depth-image-view app
			       :format depth-format
			       :aspect-flags VK_IMAGE_ASPECT_DEPTH_BIT)

      (transition-image-layout app depth-image
			       :format depth-format
			       :old-layout VK_IMAGE_LAYOUT_UNDEFINED
			       :new-layout VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
				     ))))

(defmethod begin-single-time-commands ((app vkapp))
  (with-slots (device command-pool) app
    (with-vk-struct (p-alloc-info VkCommandBufferAllocateInfo)
      (with-foreign-slots ((vk::level
			    vk::commandPool
			    vk::commandBufferCount)
			   p-alloc-info (:struct VkCommandBufferAllocateInfo))
	(setf vk::level VK_COMMAND_BUFFER_LEVEL_PRIMARY
	      vk::commandPool (elt command-pool 0)
	      vk::commandBufferCount 1))
      (with-foreign-object (p-command-buffer 'VkCommandBuffer)
	(vkAllocateCommandBuffers device p-alloc-info p-command-buffer)
	(let ((command-buffer (mem-aref p-command-buffer 'VkCommandBuffer)))
	  (with-vk-struct (p-begin-info VkCommandBufferBeginInfo)
	    (setf (foreign-slot-value p-begin-info
				      '(:struct VkCommandBufferBeginInfo)
				      'vk::flags)
		  VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)

	    (vkBeginCommandBuffer command-buffer p-begin-info))
	  
	  command-buffer)))))



(defmethod transition-image-layout ((app vkapp) image &key format old-layout new-layout)
  (let ((command-buffer (begin-single-time-commands app)))
    (with-vk-struct (p-barrier VkImageMemoryBarrier)
      (with-foreign-slots ((vk::oldLayout
			    vk::newLayout
			    vk::srcQueueFamilyIndex
			    vk::dstQueueFamilyIndex
			    vk::srcAccessMask
			    vk::dstAccessMask
			    vk::image)
			   p-barrier (:struct VkImageMemoryBarrier))
	(setf vk::oldLayout old-layout
	      vk::newLayout new-layout
	      vk::srcQueueFamilyIndex VK_QUEUE_FAMILY_IGNORED
	      vk::dstQueueFamilyIndex VK_QUEUE_FAMILY_IGNORED
	      vk::image image)

	(if (eq new-layout VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL)
	    (progn
	      (setf (foreign-slot-value
		     (foreign-slot-pointer
		      p-barrier '(:struct VkImageMemoryBarrier)
		      'vk::subresourceRange)
		     '(:struct VKImageSubresourceRange)
		     'vk::aspectMask)
		    VK_IMAGE_ASPECT_DEPTH_BIT)
	      (when (has-stencil-component-p format)
		(setf (foreign-slot-value
		       (foreign-slot-pointer
			p-barrier '(:struct VkImageMemoryBarrier)
			'vk::subresourceRange)
		       '(:struct VKImageSubresourceRange)
		       'vk::aspectMask)
		      (logior (foreign-slot-value
			       (foreign-slot-pointer
				p-barrier '(:struct VkImageMemoryBarrier)
				'vk::subresourceRange)
			       '(:struct VKImageSubresourceRange)
			       'vk::aspectMask)
			      VK_IMAGE_ASPECT_STENCIL_BIT))))
	    (setf (foreign-slot-value
		   (foreign-slot-pointer
		    p-barrier '(:struct VkImageMemoryBarrier)
		    'vk::subresourceRange)
		   '(:struct VKImageSubresourceRange)
		   'vk::aspectMask)
		  VK_IMAGE_ASPECT_COLOR_BIT))

	(with-foreign-slots ((vk::baseMipLevel
			      vk::levelCount
			      vk::baseArrayLayer
			      vk::layerCount)
			     (foreign-slot-pointer
			      p-barrier '(:struct VkImageMemoryBarrier)
			      'vk::subresourceRange)
			     (:struct VkImageSubresourceRange))
	  (setf vk::baseMipLevel 0
		vk::levelCount 1
		vk::baseArrayLayer 0
		vk::layerCount 1))

	(let ((source-stage)
	      (destination-stage))
	  (if (and (eq old-layout VK_IMAGE_LAYOUT_UNDEFINED)
		   (eq new-layout VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL))
	      (setf vk::srcAccessMask 0
		    vk::dstAccessMask VK_ACCESS_TRANSFER_WRITE_BIT

		    source-stage VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
		    destination-stage VK_PIPELINE_STAGE_TRANSFER_BIT)
	      (if (and (eq old-layout VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL)
		       (eq new-layout VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL))
		  (setf vk::srcAccessMask VK_ACCESS_TRANSFER_WRITE_BIT
			vk::dstAccessMask VK_ACCESS_SHADER_READ_BIT
			
			source-stage VK_PIPELINE_STAGE_TRANSFER_BIT
			destination-stage VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)
		  (if (and (eq old-layout VK_IMAGE_LAYOUT_UNDEFINED)
			   (eq new-layout VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL))

		      (setf vk::srcAccessMask 0
			    vk::dstAccessMask (logior VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
						      VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT)
			    source-stage VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
			    destination-stage VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
		      (error "unsupported layout transition"))))
	  (vkCmdPipelineBarrier command-buffer source-stage
				destination-stage 0 0 +nullptr+ 0 +nullptr+ 1 p-barrier)
	  (end-single-time-commands app command-buffer))))))

(defmethod end-single-time-commands ((app vkapp) command-buffer)
  (with-slots (device queue command-pool) app
    (vkEndCommandBuffer command-buffer)

    (with-vk-struct (p-submit-info VkSubmitInfo)
      (with-foreign-slots ((vk::commandBufferCount
			    vk::pCommandBuffers)
			   p-submit-info (:struct VkSubmitInfo))
	(with-foreign-object (p-command-buffer 'VkCommandBuffer)
	  (setf (mem-aref p-command-buffer 'VkCommandBuffer) command-buffer)
	  (setf vk::commandBufferCount 1
		vk::pCommandBuffers p-command-buffer)

	  (vkQueueSubmit queue 1 p-submit-info VK_NULL_HANDLE)
	  (vkQueueWaitIdle queue)

	  (vkFreeCommandBuffers device (elt command-pool 0) 1 p-command-buffer)

	  (values))))))

;; todo: make this a general create-image function
(defmethod create-depth-image ((app vkapp) &key format tiling usage properties)
  (with-slots (device allocator fb-width fb-height depth-image depth-image-memory) app
    (with-vk-struct (p-image-info VkImageCreateInfo)
      (with-foreign-slots ((vk::imageType
			    vk::mipLevels vk::arrayLayers vk::format vk::tiling
			    vk::initialLayout vk::usage vk::samples vk::sharingMode)
			   p-image-info (:struct VkImageCreateInfo))
	(setf vk::imageType VK_IMAGE_TYPE_2D
	      vk::mipLevels 1
	      vk::arrayLayers 1
	      vk::format format
	      vk::tiling tiling
	      vk::initialLayout VK_IMAGE_LAYOUT_UNDEFINED
	      vk::usage usage
	      vk::samples VK_SAMPLE_COUNT_1_BIT
	      vk::sharingMode VK_SHARING_MODE_EXCLUSIVE
	      
	      (foreign-slot-value 
	       (foreign-slot-pointer p-image-info '(:struct VkImageCreateInfo) 'vk::extent)
	       '(:struct VKExtent3D) 'vk::width)
	      fb-width

	      (foreign-slot-value 
	       (foreign-slot-pointer p-image-info '(:struct VkImageCreateInfo) 'vk::extent)
	       '(:struct VKExtent3D) 'vk::height)
	      fb-height
	      
	      (foreign-slot-value 
	       (foreign-slot-pointer p-image-info '(:struct VkImageCreateInfo) 'vk::extent)
	       '(:struct VKExtent3D) 'vk::depth)
	      1))
      
      (with-foreign-object (p-image 'VkImage)
	(check-vk-result (vkCreateImage device p-image-info allocator p-image))
	(setf depth-image (mem-aref p-image 'VkImage))
	
	(with-vk-struct (p-mem-requirements VkMemoryRequirements)
	  (with-foreign-slots ((vk::size vk::memoryTypeBits)
			       p-mem-requirements (:struct VkMemoryRequirements))
	  
	    (vkGetImageMemoryRequirements device depth-image p-mem-requirements)
	    
	    (with-vk-struct (p-alloc-info VkMemoryAllocateinfo)
	      (with-foreign-slots ((vk::allocationSize vk::memoryTypeIndex)
				   p-alloc-info (:struct VkMemoryAllocateInfo))
		(setf vk::allocationSize vk::size
		      vk::memoryTypeIndex (find-memory-type app vk::memoryTypeBits properties)))
	      
	      (with-foreign-object (p-image-memory 'VkDeviceMemory)
		(check-vk-result (vkAllocateMemory device p-alloc-info allocator p-image-memory))
		(setf depth-image-memory (mem-aref p-image-memory 'VkDeviceMemory))))))))
    (vkBindImageMemory device depth-image depth-image-memory 0)
    (values)))

;; todo: roll this into a create-image-view
(defmethod create-depth-image-view ((app vkapp) &key format aspect-flags)
  (with-slots (device allocator depth-image depth-image-view) app
    (with-vk-struct (p-view-info VkImageViewCreateInfo)
      (with-foreign-slots ((vk::image
			    vk::viewType vk::format)
			   p-view-info (:struct VkImageViewCreateInfo))
	(with-foreign-slots ((vk::aspectMask
			      vk::baseMipLevel vk::levelCount
			      vk::baseArrayLayer vk::layerCount)
			     (foreign-slot-pointer p-view-info '(:struct VkImageViewCreateInfo) 'vk::subresourceRange)
			     (:struct VKImageSubresourceRange))
	  (setf vk::image depth-image
		vk::viewType VK_IMAGE_VIEW_TYPE_2D
		vk::format format
		
		vk::aspectMask aspect-flags
		vk::baseMipLevel 0
		vk::levelCount 1
		vk::baseArrayLayer 0
		vk::layerCount 1)

	  (with-foreign-object (p-image-view 'VkImageView)
	    (check-vk-result (vkCreateImageView device p-view-info allocator p-image-view))
	    (setf depth-image-view (mem-aref p-image-view 'VkImageView)))))))
  (values))
			 



(defmethod frame-begin ((app vkapp))
  (with-slots (device fence frame-index swapchain present-complete-semaphore back-buffer-indices
		      command-buffer render-pass clear-value command-pool fb-height fb-width
		      framebuffer) app

    (tagbody
     continue
       (with-foreign-object (p-fence 'VkFence)
	 (setf (mem-aref p-fence 'VkFence) (elt fence frame-index))
	 (let ((result (vkWaitForFences device 1 p-fence VK_TRUE 100)))
	   (when (eq result VK_SUCCESS)
	     (go break))
	   (when (eq result VK_TIMEOUT)
	     (go continue))
	   (check-vk-result result)))
     break)

    (with-foreign-object (p-back-buffer-index :uint32)

      (check-vk-result
       (vkAcquireNextImageKHR device swapchain UINT64_MAX (elt present-complete-semaphore frame-index)
			      VK_NULL_HANDLE p-back-buffer-index))
      (setf (elt back-buffer-indices frame-index) (mem-aref p-back-buffer-index :uint32)))

    (check-vk-result (vkResetCommandPool device (elt command-pool frame-index) 0))

    (with-vk-struct (p-info VkCommandBufferBeginInfo)
      (with-foreign-slots ((vk::flags) p-info (:struct VkCommandBufferBeginInfo))
	(setf vk::flags VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)

	(check-vk-result (vkBeginCommandBuffer (elt command-buffer frame-index) p-info))))

    (with-vk-struct (p-viewports VkViewport)
      (with-foreign-slots ((vk::x
			    vk::y
			    vk::width
			    vk::height
			    vk::minDepth
			    vk::maxDepth)
			   p-viewports (:struct VkViewport))
	(setf vk::x 0.0f0
	      vk::y 0.0f0
	      vk::width (coerce fb-width 'single-float)
	      vk::height (coerce fb-height 'single-float)
	      vk::minDepth 0.0f0
	      vk::maxDepth 1.0f0))

      (vkCmdSetViewport (elt command-buffer frame-index) 0 1 p-viewports))

    (with-vk-struct (p-scissor VkRect2D)
      (setf (foreign-slot-value
	     (foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::offset)
	     '(:struct VkOffset2D)
	     'vk::x) 0
	     
	     (foreign-slot-value
	      (foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::offset)
	      '(:struct VkOffset2D)
	      'vk::y) 0
	      
	      (foreign-slot-value
	       (foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::extent)
	       '(:struct VkExtent2D)
	       'vk::width) fb-width
	       
	       (foreign-slot-value
		(foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::extent)
		'(:struct VkExtent2D)
		'vk::height) fb-height)

      (vkCmdSetScissor (elt command-buffer frame-index) 0 1 p-scissor))


    (with-vk-struct (p-info VkRenderPassBeginInfo)
      (with-foreign-slots ((vk::renderPass vk::framebuffer vk::renderArea
					   vk::clearValueCount vk::pClearValues)
			   p-info (:struct VkRenderPassBeginInfo))

	(with-foreign-object (p-clear-values '(:union VkClearValue) 2)
	  (setf (mem-aref (mem-aptr p-clear-values '(:union VkClearValue) 0) :float 0) (elt clear-value 0)
		(mem-aref (mem-aptr p-clear-values '(:union VkClearValue) 0) :float 1) (elt clear-value 1)
		(mem-aref (mem-aptr p-clear-values '(:union VkClearValue) 0) :float 2) (elt clear-value 2)
		(mem-aref (mem-aptr p-clear-values '(:union VkClearValue) 0) :float 3) (elt clear-value 3)

		(foreign-slot-value
		 (foreign-slot-pointer (mem-aptr p-clear-values '(:union VkClearValue) 1)
				       '(:union VkClearValue)
				       'vk::depthStencil)
		 '(:struct VkClearDepthStencilValue)
		 'vk::depth) 1.0f0

		 (foreign-slot-value
		  (foreign-slot-pointer (mem-aptr p-clear-values '(:union VkClearValue) 1)
					'(:union VkClearValue)
					'vk::depthStencil)
		  '(:struct VkClearDepthStencilValue)
		  'vk::stencil) 0)

	  (setf vk::renderPass render-pass
		vk::framebuffer (elt framebuffer (elt back-buffer-indices frame-index))

		(foreign-slot-value
		 (foreign-slot-pointer
		  (foreign-slot-pointer p-info '(:struct VkRenderPassBeginInfo) 'vk::renderArea)
		  '(:struct VkRect2D) 'vk::extent)
		 '(:struct VkExtent2D)
		 'vk::width) fb-width

		 (foreign-slot-value
		 (foreign-slot-pointer
		  (foreign-slot-pointer p-info '(:struct VkRenderPassBeginInfo) 'vk::renderArea)
		  '(:struct VkRect2D) 'vk::extent)
		 '(:struct VkExtent2D)
		 'vk::height) fb-height
		 		  
		  vk::clearValueCount 2
		  vk::pClearValues p-clear-values)
	  (vkCmdBeginRenderPass (elt command-buffer frame-index) p-info VK_SUBPASS_CONTENTS_INLINE)))))
  (values))
					 
(defmethod frame-end ((app vkapp))
  (with-slots (command-buffer frame-index fence render-complete-semaphore present-complete-semaphore
			      device queue) app

    (vkCmdEndRenderPass (elt command-buffer frame-index))

    (let ((wait-stage VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT))
      (with-foreign-objects ((p-wait-stage :int)
			     (p-command-buffer 'VkCommandBuffer)
			     (p-present-complete-semaphore 'VkSemaphore)
			     (p-render-complete-semaphore 'VkSemaphore)
			     (p-fence 'VkFence))
	(setf (mem-aref p-wait-stage :int) wait-stage
	      (mem-aref p-command-buffer 'VkCommandBuffer) (elt command-buffer frame-index)
	      (mem-aref p-present-complete-semaphore 'VkSemaphore) (elt present-complete-semaphore frame-index)
	      (mem-aref p-render-complete-semaphore 'VkSemaphore) (elt render-complete-semaphore frame-index)
	      (mem-aref p-fence 'VkFence) (elt fence frame-index))
	(with-vk-struct (p-info VkSubmitInfo)
	  (with-foreign-slots ((vk::waitSemaphoreCount vk::pWaitSemaphores vk::pWaitDstStageMask vk::commandBufferCount
						       vk::pCommandBuffers vk::signalSemaphoreCount
						       vk::pSignalSemaphores)
			       p-info (:struct VkSubmitInfo))

	    (setf vk::waitSemaphoreCount 1
		  vk::pWaitSemaphores p-present-complete-semaphore
		  vk::pWaitDstStageMask p-wait-stage
		  vk::commandBufferCount 1
		  vk::pCommandBuffers p-command-buffer
		  vk::signalSemaphoreCount 1
		  vk::pSignalSemaphores p-render-complete-semaphore)
	    (check-vk-result (vkEndCommandBuffer (elt command-buffer frame-index)))
	    (check-vk-result (vkResetFences device 1 p-fence))
	    (check-vk-result (vkQueueSubmit queue 1 p-info (elt fence frame-index)))
	    (values)))))))

(defmethod frame-present ((app vkapp))
  (with-slots (frame-index swapchain render-complete-semaphore queue back-buffer-indices) app
    (let ((present-index (if *imgui-unlimited-frame-rate*
			     (mod (1- (+ frame-index IMGUI_VK_QUEUED_FRAMES)) IMGUI_VK_QUEUED_FRAMES)
			     frame-index)))

      (with-foreign-objects ((p-swapchains 'VkSwapchainKHR 1)
			     (p-render-complete-semaphore 'VkSemaphore))
	(setf (mem-aref p-swapchains 'VkSwapchainKHR 0) swapchain
	      (mem-aref p-render-complete-semaphore 'VkSemaphore) (elt render-complete-semaphore present-index))
	(with-vk-struct (p-info VkPresentInfoKHR)
	  (with-foreign-slots ((vk::waitSemaphoreCount vk::pWaitSemaphores vk::swapchainCount
						       vk::pSwapchains vk::pImageIndices)
			       p-info (:struct VkPresentInfoKHR))
	    (with-foreign-objects ((p-indices :uint32)
				   (p-swapchain 'VkSwapchainKHR))
	      
	      (setf (mem-aref p-indices :uint32) (elt back-buffer-indices present-index)
		    (mem-aref p-swapchain 'VkSwapchainKHR) swapchain)
	      
	      (setf vk::waitSemaphoreCount 1
		    vk::pWaitSemaphores p-render-complete-semaphore
		    vk::swapchainCount 1
		    vk::pSwapchains p-swapchain
		    vk::pImageIndices p-indices)

	      (check-vk-result (vkQueuePresentKHR queue p-info))

	      (setf frame-index (mod (1+ frame-index) IMGUI_VK_QUEUED_FRAMES))))))))

  (values))

(defcallback error-callback :void ((error :int) (description (:pointer :char)))
  (error-callback-function error description))

(defun error-callback-function (error description)
  (format *error-output* "Error: ~A: ~A~%" error description)
  (values))

(defmethod setup-window ((app vkapp) (w integer) (h integer))
  (with-slots (app-id window) app
    
    (setf app-id (make-pointer (incf *last-app-id*)))
    (push app *apps*)
    
    (glfwSetErrorCallback (get-callback 'error-callback))
    
    (when (zerop (glfwInit))
      (error "GLFW failed to initialize."))
    
    (glfwWindowHint GLFW_CLIENT_API GLFW_NO_API)
    (setf window (glfwCreateWindow w h "ImGui Vulkan example" +nullptr+ +nullptr+))
    
    (glfwSetWindowUserPointer window app-id)
    
    (when (zerop (glfwVulkanSupported))
      (error "GLFW: Vulkan Not Supported.")))
    
  (values))

(defmethod allocate-surface-format ((app vkapp))
  (with-slots (surface-format) app
    (setf surface-format (foreign-alloc '(:struct VkSurfaceFormatKHR))))
  (values))

(defmethod deallocate-surface-format ((app vkapp))
  (with-slots (surface-format) app
    (foreign-free surface-format)
    (setf surface-format nil))
  (values))

(defmethod allocate-image-range ((app vkapp))
  (with-slots (image-range) app
    (setf image-range (foreign-alloc '(:struct VkImageSubresourceRange)))
    
    (with-foreign-slots ((vk::aspectMask vk::baseMipLevel vk::levelCount
					 vk::baseArrayLayer vk::layerCount)
			 image-range (:struct VkImageSubresourceRange))

      (setf vk::aspectMask VK_IMAGE_ASPECT_COLOR_BIT
	    vk::baseMipLevel 0
	    vk::levelCount 1
	    vk::baseArrayLayer 0
	    vk::layerCount 1)))
  (values))

(defmethod deallocate-image-range ((app vkapp))
  (with-slots (image-range) app
    (foreign-free image-range)
    (setf image-range nil))
  (values))

#+NOTYET
(defmethod read-in-shaders ((app vkapp))
  (with-slots (vertex-shader
	       fragment-shader)
      (setf vertex-shader (create-shader "C:/Users/awolven/vkapp/shaders/vert.spv" 'vertex-shader)
	    frgament-shader (create-shader "C:/Users/awolven/vkapp/shaders/frag.spv" 'fragment-shader))
    (values)))

(defmethod read-in-shaders ((app vkapp))
  (with-slots (vertex-shader-code
	       vertex-shader-code-size
	       fragment-shader-code
	       fragment-shader-code-size) app
    (multiple-value-bind (code size) #+NIL(shader-binary-from-list vertex-shader-list)
			 (read-shader-file "C:/Users/awolven/vkapp/shaders/vert.spv")
      (setf vertex-shader-code code
	    vertex-shader-code-size size))
    (multiple-value-bind (code size) #+NIL(shader-binary-from-list fragment-shader-list)
			 (read-shader-file "C:/Users/awolven/vkapp/shaders/frag.spv")
      (setf fragment-shader-code code
	    fragment-shader-code-size size)))
  (values))

(defmethod init-imgui-glfw-vulkan-impl ((app vkapp))
  (with-slots (allocator gpu device render-pass pipeline-cache descriptor-pool window) app
    (with-foreign-object (p-init-data '(:struct ImGui_ImplGlfwVulkan_Init_Data))
      (with-foreign-slots ((vkappui::allocator vkappui::gpu vkappui::device vkappui::render_pass
			    vkappui::pipeline_cache vkappui::descriptor_pool vkappui::check_vk_result)
			   p-init-data (:struct ImGui_ImplGlfwVulkan_Init_Data))
	(setf vkappui::allocator allocator
	      vkappui::gpu gpu
	      vkappui::device device
	      vkappui::render_pass render-pass
	      vkappui::pipeline_cache pipeline-cache
	      vkappui::descriptor_pool descriptor-pool
	      vkappui::check_vk_result (get-callback 'check-vk-result-callback))
	(with-foreign-object (p-bool 'VkBool32)
	  (setf (mem-aref p-bool 'VkBool32) VK_TRUE)
	  (ImGui_ImplGlfwVulkan_Init window p-bool p-init-data)))))
  (values))

(defmethod main ((app vkapp) &rest args &key (w 1280) (h 720))
  (declare (ignore args))

  (with-slots (window allocator gpu device render-pass queue command-pool command-buffer graphics-pipeline vertex-data-size
		      image-range pipeline-cache descriptor-pool clear-value frame-index surface-format vertex-buffer fb-height fb-width
		      index-buffer index-data-size axes-index-data-size descriptor-set annotation-descriptor-set annotation-pipeline-layout selection-pipeline-layout
		      start-time annotation-pipeline selection-pipeline) app
    (setf start-time (get-internal-real-time))
    
    (setup-vulkan app w h)
    
    (init-imgui-glfw-vulkan-impl app)
    
    (with-foreign-object (p-style '(:struct ig::ImGuiStyle))
      (igStyleColorsClassic p-style))
    
    ;; upload fonts:
    (check-vk-result (vkResetCommandPool device (elt command-pool frame-index) 0))
    
    (with-vk-struct (p-begin-info VkCommandBufferBeginInfo)
      (with-foreign-slots ((vk::flags)
			   p-begin-info (:struct VkCommandBufferBeginInfo))
	(setf vk::flags (logior vk::flags VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT))
	
	(check-vk-result (vkBeginCommandBuffer (elt command-buffer frame-index) p-begin-info))))
    
    (ImGui_ImplGlfwVulkan_CreateFontsTexture (elt command-buffer frame-index))
    
    (with-foreign-objects ((p-command-buffer 'VkCommandBuffer))
      (setf (mem-aref p-command-buffer 'VkCommandBuffer) (elt command-buffer frame-index))
      (with-vk-struct (p-end-info VkSubmitInfo)
	(with-foreign-slots ((vk::commandBufferCount vk::pCommandBuffers)
			     p-end-info (:struct VkSubmitInfo))
	  (setf vk::commandBufferCount 1
		vk::pCommandBuffers p-command-buffer)
	  (check-vk-result (vkEndCommandBuffer (elt command-buffer frame-index)))
	  (check-vk-result (vkQueueSubmit queue 1 p-end-info VK_NULL_HANDLE)))))
    
    (check-vk-result (vkDeviceWaitIdle device))
    (ImGui_ImplGlfwVulkan_InvalidateFontUploadObjects)
      
    (when *imgui-unlimited-frame-rate*
      (ImGui_ImplGlfwVulkan_NewFrame)
      (frame-begin app)
      (ImGui_ImplGlfwVulkan_Render (elt command-buffer frame-index))
      (frame-end app)
      (setf frame-index (mod (1+ frame-index) IMGUI_VK_QUEUED_FRAMES)))


    (let ((last-time (get-internal-real-time))
	  (frame-counter 0)
	  (show-demo-window 1)
	  (show-another-window 0))
      
      (loop while (zerop (glfwWindowShouldClose window))
	 with temp with dt
	   
	 do (glfwPollEvents) 
	   (ImGUI_ImplGlfwVulkan_NewFrame)
	   
	   (incf frame-counter)
	   (igText "Hello, wworld!")
	       
	   (with-foreign-object (p-f :float)
	     (setf (mem-aref p-f :float) (zoom *camera*))
	     
	     (igSliderFloat "zoom" p-f 0.1f0 10.0f0 "%f" 1.0f0)
	     
	     (setf (zoom *camera*) (mem-aref p-f :float)))

	   (with-foreign-object (p-clear-color :float 4)
	     (setf (mem-aref p-clear-color :float 0) 0.45f0
		   (mem-aref p-clear-color :float 1) 0.55f0
		   (mem-aref p-clear-color :float 2) 0.60f0
		   (mem-aref p-clear-color :float 3) 1.00f0)
		 
	     (igColorEdit3 "clear color" p-clear-color 0)

	     (setf (elt clear-value 0) (mem-aref  p-clear-color :float 0)
		   (elt clear-value 1) (mem-aref  p-clear-color :float 1)
		   (elt clear-value 2) (mem-aref  p-clear-color :float 2)
		   (elt clear-value 3) (mem-aref  p-clear-color :float 3)))
		 
	   (with-foreign-object (p-camera-pos :float 3)
	     (setf (mem-aref p-camera-pos :float 0) (camera-x *camera*)
		   (mem-aref p-camera-pos :float 1) (camera-y *camera*)
		   (mem-aref p-camera-pos :float 2) (camera-z *camera*))
		   
	     (igInputFloat3 "camera position" p-camera-pos 2 0)
		 
	     (setf (camera-x *camera*) (mem-aref p-camera-pos :float 0)
		   (camera-y *camera*) (mem-aref p-camera-pos :float 1)
		   (camera-z *camera*) (mem-aref p-camera-pos :float 2)))

	   (when (not (null-pointer-p (igButton "Demo Window" (list 'ig::x 100f0 'ig::y 20f0))))
	     (setq show-demo-window (logxor show-demo-window 1)))

	   (when (not (null-pointer-p (igButton "Another Window" (list 'ig::x 120f0 'ig::y 20.0f0))))
	     (setq show-another-window (logxor show-another-window 1)))
	   
	     (setq temp (get-internal-real-time)
		   dt (- temp last-time))
	     (igText "%s ms/frame (%s FPS)"
		     :string (format nil "~A" dt)
		     :string (format nil "~A" (if (zerop dt)
						  0
						  (round (/ frame-counter (/ dt 1000.0f0)))))
		     #+NIL
		     (/ 1000.0f0 (foreign-slot-value (igGetIO) '(:struct ig::ImGuiIO) 'ig::Framerate))
		     #+NIL
		     (foreign-slot-value (igGetIO) '(:struct ig::ImGuiIO) 'ig::Framerate)
		     )
	     (setq frame-counter 0
		   last-time temp)
	
	   (with-foreign-objects ((p-show-demo-window :int)
				  (p-show-another-window :int))
	     (setf (mem-aref p-show-demo-window :int) show-demo-window
		   (mem-aref p-show-another-window :int) show-another-window)
	     
	     (when (not (zerop show-another-window))
	       
	       (igBegin "Another Window" p-show-another-window 0)
	       (setq show-another-window (mem-aref p-show-another-window :int))
	       (igText "Hello from another window!")
	       
	       (igEnd))
	     
	     (when (not (zerop show-demo-window))
		 
	       (igSetNextWindowPos (list 'ig::x 650.0f0 'ig::y 20.0f0)
				   ImGuiCond_FirstUseEver (list 'ig::x 0.0f0 'ig::y 0.0f0))
	       
	       (igShowDemoWindow p-show-demo-window)
	       (setq show-demo-window (mem-aref p-show-demo-window :int)))
	
	
	     (frame-begin app)

	     (let ((projection-matrix
		    (if (not *ortho*)
			(perspective-2 fb-width fb-height)
			(3d-matrices:mortho (* -2 (zoom *camera*) (/ fb-width fb-height))
					    (*  2 (zoom *camera*) (/ fb-width fb-height))
					    (* -2 (zoom *camera*))
					    (*  2 (zoom *camera*))
					    -10 10)))
		   (annotation-projection-matrix
		    (if (not *ortho*)
			(perspective-2 fb-width fb-height)
			(3d-matrices:mortho (* -2 (/ fb-width fb-height))
					    (*  2 (/ fb-width fb-height))
					    -2 2 -10 10))))
	       (when *ortho*
		 (setq projection-matrix (convert-projection-matrix-to-vulkan projection-matrix))
		 (setq annotation-projection-matrix (convert-projection-matrix-to-vulkan annotation-projection-matrix)))
	       ;;-------------------------------------------------
	       (vkCmdBindPipeline (elt command-buffer frame-index) VK_PIPELINE_BIND_POINT_GRAPHICS annotation-pipeline)
	       (update-annotation-uniform-buffer-vs app annotation-projection-matrix)
	       (with-foreign-objects ((p-vertex-buffers 'VkBuffer)
				      (p-offsets 'VkDeviceSize)
				      (p-descriptor-sets 'VkDescriptorSet))
		 (setf (mem-aref p-vertex-buffers 'VkBuffer) (slot-value app 'axes-vertex-buffer)
		       (mem-aref p-offsets 'VkDeviceSize) 0
		       (mem-aref p-descriptor-sets 'VkDescriptorSet) annotation-descriptor-set)
		 (vkCmdBindVertexBuffers (elt command-buffer frame-index) 0 1 p-vertex-buffers p-offsets)
		 (vkCmdBindIndexBuffer (elt command-buffer frame-index) (slot-value app 'axes-index-buffer) 0 VK_INDEX_TYPE_UINT16)
		 (vkCmdBindDescriptorSets (elt command-buffer frame-index) VK_PIPELINE_BIND_POINT_GRAPHICS annotation-pipeline-layout
					  0 1 p-descriptor-sets 0 +nullptr+))
	       (vkCmdDrawIndexed (elt command-buffer frame-index) axes-index-data-size 1 0 0 0)
	       ;;-------------------------------------------------
	       #|
	       (vkCmdBindPipeline (elt command-buffer frame-index) VK_PIPELINE_BIND_POINT_GRAPHICS graphics-pipeline)
	       (update-uniform-buffer-vs app projection-matrix)
	       (with-foreign-objects ((p-vertex-buffers 'VkBuffer)
				      (p-offsets 'VkDeviceSize)
				      (p-descriptor-sets 'VkDescriptorSet))
		 (setf (mem-aref p-vertex-buffers 'VkBuffer) vertex-buffer
		       (mem-aref p-offsets 'VkDeviceSize) 0
		       (mem-aref p-descriptor-sets 'VkDescriptorSet) descriptor-set)
		 (vkCmdBindVertexBuffers (elt command-buffer frame-index) 0 1 p-vertex-buffers p-offsets)
		 (vkCmdBindIndexBuffer (elt command-buffer frame-index) index-buffer 0 VK_INDEX_TYPE_UINT16)
		 (vkCmdBindDescriptorSets (elt command-buffer frame-index) VK_PIPELINE_BIND_POINT_GRAPHICS pipeline-layout
					  0 1 p-descriptor-sets 0 +nullptr+))
	       (vkCmdDrawIndexed (elt command-buffer frame-index) index-data-size 1 0 0 0) |#
		 ;;-------------------------------------------------
		   (vkCmdBindPipeline (elt command-buffer frame-index) VK_PIPELINE_BIND_POINT_GRAPHICS selection-pipeline)
		   (let ((view-matrix (update-uniform-buffer-vs app projection-matrix)))
		     (update-uniform-buffer-gs app projection-matrix view-matrix))
		 (with-foreign-objects ((p-vertex-buffers 'VkBuffer)
					(p-offsets 'VkDeviceSize)
					(p-descriptor-sets 'VkDescriptorSet))
		   (setf (mem-aref p-vertex-buffers 'VkBuffer) vertex-buffer
			 (mem-aref p-offsets 'VkDeviceSize) 0
			 (mem-aref p-descriptor-sets 'VkDescriptorSet) descriptor-set)
		   (vkCmdBindVertexBuffers (elt command-buffer frame-index) 0 1 p-vertex-buffers p-offsets)
		   (vkCmdBindIndexBuffer (elt command-buffer frame-index) index-buffer 0 VK_INDEX_TYPE_UINT16)
		   (vkCmdBindDescriptorSets (elt command-buffer frame-index) VK_PIPELINE_BIND_POINT_GRAPHICS selection-pipeline-layout
					    0 1 p-descriptor-sets 0 +nullptr+))
		 (vkCmdDrawIndexed (elt command-buffer frame-index) index-data-size 1 0 0 0)

	       (ImGui_ImplGlfwVulkan_Render (elt command-buffer frame-index)))
	
	     (frame-end app)
	     (frame-present app))))
	
    (check-vk-result (vkDeviceWaitIdle device))
    (ImGui_ImplGlfwVulkan_Shutdown)
    (cleanup-vulkan app)
    (glfwTerminate)
    
    t))

(defvar *app*)

(defun run-app (&optional (app (make-instance 'vkapp)))
  (declare (ignore app))
  (multiple-value-bind (vertex-data vertex-data-size index-data index-data-size)
      (oc::make-sphere)
    (let ((app (make-instance 'vkapp :vertex-data vertex-data :vertex-data-size vertex-data-size
	    :index-data index-data :index-data-size  index-data-size
	    )))
      (setq *app* app)
      (sb-thread:make-thread #'(lambda () (main app))))))

#+NIL
(defun run-app (&optional (app (make-instance 'vkapp)))
  (sb-thread:make-thread #'(lambda () (main app))))

(defmethod create-buffer ((app vkapp) size usage properties)
  (with-slots (device allocator) app
    (let ((buffer) (buffer-memory))
      
      (with-foreign-objects ((p-buffer 'VkBuffer)
			     (p-buffer-memory 'VkDeviceMemory))
      
	(with-vk-struct (p-info VkBufferCreateInfo)
	  (with-foreign-slots ((vk::size vk::usage vk::sharingMode)
			       p-info (:struct VkBufferCreateInfo))
	    (setf vk::size size
		  vk::usage usage
		  vk::sharingMode VK_SHARING_MODE_EXCLUSIVE)
	    
	    (check-vk-result (vkCreateBuffer device p-info allocator p-buffer))
	    (setq buffer (mem-aref p-buffer 'VkBuffer))
	  
	    (with-vk-struct (p-mem-requirements VkMemoryRequirements)
	      (vkGetBufferMemoryRequirements device buffer p-mem-requirements)
	      
	      (with-vk-struct (p-alloc-info VkMemoryAllocateInfo)
		(with-foreign-slots ((vk::allocationSize vk::memoryTypeIndex)
				     p-alloc-info (:struct VkMemoryAllocateInfo))
		  (setf vk::allocationSize (foreign-slot-value p-mem-requirements
							       '(:struct VkMemoryRequirements)
							       'vk::size)
			vk::memoryTypeIndex (find-memory-type
					     app (foreign-slot-value p-mem-requirements
								     '(:struct VkMemoryRequirements)
								     'vk::memoryTypeBits)
					     properties))
		
		  (check-vk-result (vkAllocateMemory device p-alloc-info +nullptr+ p-buffer-memory))
		  (setq buffer-memory (mem-aref p-buffer-memory 'VkDeviceMemory)))))
	  
	    (vkBindBufferMemory device buffer buffer-memory 0))))
      (values buffer buffer-memory))))

(defmethod create-vertex-buffer ((app vkapp))
  (with-slots (device allocator vertex-buffer vertex-buffer-memory vertex-data vertex-data-size) app
    (multiple-value-bind (staging-buffer staging-buffer-memory)
	(create-buffer app vertex-data-size VK_BUFFER_USAGE_TRANSFER_SRC_BIT
		       (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))

      (with-foreign-object (pp-data :pointer)
	(vkMapMemory device staging-buffer-memory 0 vertex-data-size 0 pp-data)
	(memcpy (mem-aref pp-data :pointer) vertex-data vertex-data-size)
	(vkUnmapMemory device staging-buffer-memory))

      (multiple-value-bind (buffer buffer-memory)
	  (create-buffer app vertex-data-size (logior VK_BUFFER_USAGE_TRANSFER_DST_BIT VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
			 VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
	(setf vertex-buffer buffer
	      vertex-buffer-memory buffer-memory)
	(copy-buffer app staging-buffer vertex-buffer vertex-data-size)
	(vkDestroyBuffer device staging-buffer +nullptr+)
	(vkFreeMemory device staging-buffer-memory +nullptr+))))
  (values))

(defmethod create-axes-vertex-buffer ((app vkapp))
  (with-slots (axes-vertex-data
	       axes-vertex-data-size
	       axes-vertex-buffer
	       axes-vertex-buffer-memory) app
				
    (multiple-value-bind (vertex-buffer vertex-buffer-memory)
	(create-vertex-buffer2 app axes-vertex-data axes-vertex-data-size)

      (setf axes-vertex-buffer vertex-buffer
	    axes-vertex-buffer-memory vertex-buffer-memory)
      (values))))

(defmethod create-vertex-buffer2 ((app vkapp) vertex-data vertex-data-size)
  (with-slots (device allocator) app
    (multiple-value-bind (staging-buffer staging-buffer-memory)
	(create-buffer
	 app vertex-data-size VK_BUFFER_USAGE_TRANSFER_SRC_BIT
	 (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))

      (with-foreign-object (pp-data :pointer)
	(vkMapMemory device staging-buffer-memory 0 vertex-data-size 0 pp-data)
	(memcpy (mem-aref pp-data :pointer) vertex-data vertex-data-size)
	(vkUnmapMemory device staging-buffer-memory))

      (multiple-value-bind (buffer buffer-memory)
	  (create-buffer
	   app vertex-data-size (logior VK_BUFFER_USAGE_TRANSFER_DST_BIT VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
	   VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
	(copy-buffer app staging-buffer buffer vertex-data-size)
	(vkDestroyBuffer device staging-buffer +nullptr+)
	(vkFreeMemory device staging-buffer-memory +nullptr+)
	(values buffer buffer-memory)))))

(defmethod create-index-buffer ((app vkapp))
  (with-slots (device allocator index-buffer index-buffer-memory index-data index-data-size) app
    (multiple-value-bind (staging-buffer staging-buffer-memory)
	(create-buffer app index-data-size VK_BUFFER_USAGE_TRANSFER_SRC_BIT
		       (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))

      (with-foreign-object (pp-data :pointer)
	(vkMapMemory device staging-buffer-memory 0 index-data-size 0 pp-data)
	(memcpy (mem-aref pp-data :pointer) index-data index-data-size)
	(vkUnmapMemory device staging-buffer-memory))

      (multiple-value-bind (buffer buffer-memory)
	  (create-buffer app (* (foreign-type-size :unsigned-short)
				index-data-size) (logior VK_BUFFER_USAGE_TRANSFER_DST_BIT VK_BUFFER_USAGE_INDEX_BUFFER_BIT)
			 VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
	(setf index-buffer buffer
	      index-buffer-memory buffer-memory)
	(copy-buffer app staging-buffer index-buffer index-data-size)
	(vkDestroyBuffer device staging-buffer +nullptr+)
	(vkFreeMemory device staging-buffer-memory +nullptr+))))
  (values))

(defmethod create-axes-index-buffer ((app vkapp))
  (with-slots (device allocator axes-index-buffer axes-index-buffer-memory axes-index-data axes-index-data-size) app
    (multiple-value-bind (staging-buffer staging-buffer-memory)
	(create-buffer app axes-index-data-size VK_BUFFER_USAGE_TRANSFER_SRC_BIT
		       (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))

      (with-foreign-object (pp-data :pointer)
	(vkMapMemory device staging-buffer-memory 0 axes-index-data-size 0 pp-data)
	(memcpy (mem-aref pp-data :pointer) axes-index-data axes-index-data-size)
	(vkUnmapMemory device staging-buffer-memory))

      (multiple-value-bind (buffer buffer-memory)
	  (create-buffer app (* (foreign-type-size :unsigned-short)
				axes-index-data-size) (logior VK_BUFFER_USAGE_TRANSFER_DST_BIT VK_BUFFER_USAGE_INDEX_BUFFER_BIT)
			 VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
	(setf axes-index-buffer buffer
	      axes-index-buffer-memory buffer-memory)
	(copy-buffer app staging-buffer axes-index-buffer axes-index-data-size)
	(vkDestroyBuffer device staging-buffer +nullptr+)
	(vkFreeMemory device staging-buffer-memory +nullptr+))))
  (values))

(defmethod create-uniform-buffer-vs ((app vkapp))
  (with-slots (device allocator uniform-buffer-vs uniform-buffer-vs-memory) app
    (let ((buffer-size (foreign-type-size '(:struct UniformBufferObjectVertexShader))))
      (multiple-value-bind (buffer buffer-memory)
	  (create-buffer app buffer-size VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
			 (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))
	(setf uniform-buffer-vs buffer
	      uniform-buffer-vs-memory buffer-memory))))
  (values))

(defmethod create-annotation-uniform-buffer-vs ((app vkapp))
  (with-slots (device allocator annotation-uniform-buffer-vs annotation-uniform-buffer-vs-memory) app
    (let ((buffer-size (foreign-type-size '(:struct UniformBufferObjectVertexShader))))
      (multiple-value-bind (buffer buffer-memory)
	  (create-buffer app buffer-size VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
			 (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))
	(setf annotation-uniform-buffer-vs buffer
	      annotation-uniform-buffer-vs-memory buffer-memory))))
  (values))

(defmethod create-selection-uniform-buffer-gs ((app vkapp))
  (with-slots (device allocator uniform-buffer-gs uniform-buffer-gs-memory) app
    (let ((buffer-size (foreign-type-size '(:struct UniformBufferObjectGeometryShader))))
      (multiple-value-bind (buffer buffer-memory)
	  (create-buffer app buffer-size VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
			 (logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))
	(setf uniform-buffer-gs buffer
	      uniform-buffer-gs-memory buffer-memory))))
  (values))

(defun convert-projection-matrix-to-vulkan (src)
  (let ((dest (3d-matrices:mcopy4 src)))
    (setf (3d-matrices:mcref dest 1 1) (- (3d-matrices:mcref dest 1 1))
	  (3d-matrices:mcref dest 2 0) (/ (+ (3d-matrices:mcref dest 2 0) (3d-matrices:mcref dest 3 0)) 2)
	  (3d-matrices:mcref dest 2 1) (/ (+ (3d-matrices:mcref dest 2 1) (3d-matrices:mcref dest 3 1)) 2)
	  (3d-matrices:mcref dest 2 2) (/ (+ (3d-matrices:mcref dest 2 2) (3d-matrices:mcref dest 3 2)) 2)
	  (3d-matrices:mcref dest 2 3) (/ (+ (3d-matrices:mcref dest 2 3) (3d-matrices:mcref dest 3 3)) 2))
    dest))

(defun convert-projection-matrix-to-vulkan2 (src)
  (let ((pre (mat4)))
    (setf (mcref pre 0 0) 1.0f0
	  (mcref pre 1 1) -1.0f0
	  (mcref pre 2 2) 0.5f0
	  (mcref pre 2 3) 0.5f0
	  (mcref pre 3 3) 1.0f0)
    (m* pre src)))

(defmethod update-uniform-buffer-gs ((app vkapp) projection-matrix view-matrix)
  (let ((pv-matrix (m* projection-matrix view-matrix)))
    (with-slots (device uniform-buffer-gs-memory) app
      (with-foreign-object (p-ubo '(:struct UniformBufferObjectGeometryShader))
	(let ((p-mxProj (foreign-slot-pointer p-ubo '(:struct UniformBufferObjectGeometryShader) 'mxProj)))
	  
	  (loop for i from 0 to 3
	     do (loop for j from 0 to 3
		   do (setf (mem-aref p-mxProj :float (+ i (* j 4))) (coerce (3d-matrices:mcref pv-matrix #+NIL mvp-matrix i j) 'single-float)))))
	(multiple-value-bind (picking-ray-origin picking-ray-dir) (compute-picking-ray app *camera* projection-matrix view-matrix)
	  (let ((p-RayOrigin (foreign-slot-pointer p-ubo '(:struct UniformBufferObjectGeometryShader) 'RayOrigin)))
	    (igText (format nil "RayOrigin: ~S" picking-ray-origin))
	    (setf (mem-aref p-RayOrigin :float 0) (coerce (vx picking-ray-origin) 'single-float))
	    (setf (mem-aref p-RayOrigin :float 1) (coerce (vy picking-ray-origin) 'single-float))
	    (setf (mem-aref p-RayOrigin :float 2) (coerce (vz picking-ray-origin) 'single-float))
	    (setf (mem-aref p-RayOrigin :float 3) 1.0f0))
	  (igText (format nil "RayDir: ~S" picking-ray-dir))
	  (let ((p-RayDir (foreign-slot-pointer p-ubo '(:struct UniformBufferObjectGeometryShader) 'RayDir)))
	    (setf (mem-aref p-RayDir :float 0) (coerce (vx picking-ray-dir) 'single-float))
	    (setf (mem-aref p-RayDir :float 1) (coerce (vy picking-ray-dir) 'single-float))
	    (setf (mem-aref p-RayDir :float 2) (coerce (vz picking-ray-dir) 'single-float))
	    (setf (mem-aref p-RayDir :float 3) 0.0f0))
	  )

	(with-foreign-object (pp-data :pointer)
	  (vkMapMemory device uniform-buffer-gs-memory 0 (foreign-type-size '(:struct UniformBufferObjectGeometryShader)) 0 pp-data)
	  (memcpy (mem-aref pp-data :pointer) p-ubo (foreign-type-size '(:struct UniformBufferObjectGeometryShader)))
	  (vkUnmapMemory device uniform-buffer-gs-memory)))))
  (values))


(defun 3d-vectors-subtract-3d-vectors (v1 v2)
  (3d-vectors:vec3 (- (3d-vectors:vx3 v1) (3d-vectors:vx3 v2)) (- (3d-vectors:vy3 v1) (3d-vectors:vy3 v2)) (- (3d-vectors:vz3 v1) (3d-vectors:vz3 v2))))

(defun look-at2 (eye target up-dir)
  (declare (type 3d-vectors:vec3 eye)
	   (type 3d-vectors:vec3 target)
	   (type 3d-vectors:vec3 up-dir))
	   
  (let* ((forward (3d-vectors:vunit (v- eye target)))
	 (left (3d-vectors:vunit (3d-vectors:vc up-dir forward)))
	 (up (3d-vectors:vunit (3d-vectors:vc forward left)))
	 (view-matrix (3d-matrices:mat4)))

    (setf (3d-matrices:mcref view-matrix 0 0) (3d-vectors:vx3 left)
	  (3d-matrices:mcref view-matrix 0 1) (3d-vectors:vy3 left)
	  (3d-matrices:mcref view-matrix 0 2) (3d-vectors:vz3 left)
	  (3d-matrices:mcref view-matrix 0 3) (- (3d-vectors:v. left eye))
	  (3d-matrices:mcref view-matrix 1 0) (3d-vectors:vx3 up)
	  (3d-matrices:mcref view-matrix 1 1) (3d-vectors:vy3 up)
	  (3d-matrices:mcref view-matrix 1 2) (3d-vectors:vz3 up)
	  (3d-matrices:mcref view-matrix 1 3) (- (3d-vectors:v. up eye))
	  (3d-matrices:mcref view-matrix 2 0) (3d-vectors:vx3 forward)
	  (3d-matrices:mcref view-matrix 2 1) (3d-vectors:vy3 forward)
	  (3d-matrices:mcref view-matrix 2 2) (3d-vectors:vz3 forward)
	  (3d-matrices:mcref view-matrix 2 3) (- (3d-vectors:v. forward eye))
	  (3d-matrices:mcref view-matrix 3 0) 0.0d0
	  (3d-matrices:mcref view-matrix 3 1) 0.0d0
	  (3d-matrices:mcref view-matrix 3 2) 0.0d0
	  (3d-matrices:mcref view-matrix 3 3) 1.0d0)
    view-matrix))

(defun perspective-2 (width height)
  (let* ((m (mat4))
	 (a (marr m))
	 (f (/ 1.0f0 (tan (/ pi 8)))))
    (setf (aref a 0) (/ f (/ width height))
	  (aref a 1) 0.0d0
	  (aref a 2) 0.0d0
	  (aref a 3) 0.0d0

	  (aref a 4) 0.0d0
	  (aref a 5) (- f)
	  (aref a 6) 0.0d0
	  (aref a 7) 0.0d0

	  (aref a 8) 0.0d0
	  (aref a 9) 0.0d0
	  (aref a 10) (/ -6 (- 6 -6.0d0))
	  (aref a 11) -1.0d0

	  (aref a 12) 0.0d0
	  (aref a 13) 0.0d0
	  (aref a 14) (/ (* 6 -6.0d0) (- 6 -6.0d0))
	  (aref a 15) 0.0d0)
    m))
    

(defmethod update-uniform-buffer-vs ((app vkapp) projection-matrix)
  (with-slots (fb-width fb-height device uniform-buffer-vs-memory start-time current-item) app
    (let* ((current-time (get-internal-real-time))
	   (elapsed-time (- current-time start-time))
	   (model-matrix
	    (3d-matrices:m*
	     (3d-matrices:mrotation (3d-vectors:vec 0.0 0.0 1.0) (rem (* (/ elapsed-time 1000.0d0) #.(/ pi 4.0d0)) #.(* pi 2.0d0)))
	     (3d-matrices:mtranslation (3d-vectors:vec3 1 0 0))))
	   (view-matrix
	    (look-at2 (3d-vectors:vec3 (camera-x *camera*)
				       (camera-y *camera*)
				       (camera-z *camera*))
		      (3d-vectors:vec3 0.0d0 0.0d0 0.0d0)
		      (3d-vectors:vec3 0.0d0 0.0d0 1.0d0))

	    #+NIL
	    (look-at2 (make-vector-3d (camera-x *camera*)
				      (camera-y *camera*)
				      (camera-z *camera*))
		      (make-vector-3d 0.0 0.0 0.0)
		      (make-vector-3d 0 0 1))))
      ;;(setq projection-matrix (convert-projection-matrix-to-vulkan projection-matrix))
      
      (let ((modelview model-matrix
	      #+NIL(3d-matrices:m* #+NIL projection-matrix view-matrix model-matrix)))

	    

      #+NIL
      (with-foreign-object (p-current-item :int)
	 (setf (mem-aref p-current-item :int) current-item)
	 (with-foreign-strings ((p-one "one") (p-two "two") (p-three "three") (p-four "four"))
	   (with-foreign-object (p-strings :pointer 4)
	     (setf (mem-aref p-strings :pointer 0) p-one
		   (mem-aref p-strings :pointer 1) p-two
		   (mem-aref p-strings :pointer 2) p-three
		   (mem-aref p-strings :pointer 3) p-four)
	     (igListBox "listbox foo" p-current-item p-strings 4 3)))
	 (setf current-item (mem-aref p-current-item :int)))
      #+NIL
      (igLabelText "Label" "")
      
      (with-foreign-object (p-ubo '(:struct UniformBufferObjectVertexShader))

	(let ((p-mxProj (foreign-slot-pointer p-ubo '(:struct UniformBufferObjectVertexShader) 'mxProj)))
		  
	  (loop for i from 0 to 3
	     do (loop for j from 0 to 3
		   do (setf (mem-aref p-mxProj :float (+ i (* j 4))) (coerce (3d-matrices:mcref modelview i j) 'single-float)))))


	#+ORIG
	(let ((p-model (foreign-slot-pointer p-ubo '(:struct UniformBufferObjectVertexShader) 'model))
	      (p-view (foreign-slot-pointer p-ubo '(:struct UniformBufferObjectVertexShader) 'view))
	      (p-proj (foreign-slot-pointer p-ubo '(:struct UniformBufferObjectVertexShader) 'proj)))
		  
	  (loop for i from 0 to 3
	     do (loop for j from 0 to 3
		   do (setf (mem-aref p-model :float (+ (* j 4) i)) (coerce (3d-matrices:mcref model-matrix i j) 'single-float))))

	  (loop for i from 0 to 3
	     do (loop for j from 0 to 3
		   do (setf (mem-aref p-view :float (+ (* j 4) i)) (coerce (3d-matrices:mcref view-matrix i j) 'single-float))))

	  (loop for i from 0 to 3
	     do (loop for j from 0 to 3
		   do (setf (mem-aref p-proj :float (+ (* j 4) i)) (coerce (3d-matrices:mcref projection-matrix i j) 'single-float)))))
	    
	(with-foreign-object (pp-data :pointer)
	  (vkMapMemory device uniform-buffer-vs-memory 0 (foreign-type-size '(:struct UniformBufferObjectVertexShader)) 0 pp-data)
	  (memcpy (mem-aref pp-data :pointer) p-ubo (foreign-type-size '(:struct UniformBufferObjectVertexShader)))
	  (vkUnmapMemory device uniform-buffer-vs-memory))))
      (values view-matrix model-matrix))))

(defmethod update-annotation-uniform-buffer-vs ((app vkapp) projection-matrix)
  (with-slots (fb-width fb-height device annotation-uniform-buffer-vs-memory) app
    (let* ((model-matrix (3d-matrices::meye 4))
	   (view-matrix
	    (look-at2 (3d-vectors:vec3 (camera-x *camera*)
				       (camera-y *camera*)
				       (camera-z *camera*))
		      (3d-vectors:vec3 0.0d0 0.0d0 0.0d0)
		      (3d-vectors:vec3 0.0d0 0.0d0 1.0d0))
	    #+NIL
	    (look-at2 (make-vector-3d (camera-x *camera*)
				      (camera-y *camera*)
				      (camera-z *camera*))
		      (make-vector-3d 0.0 0.0 0.0)
		      (make-vector-3d 0 0 1))))
      ;;(setq projection-matrix (convert-projection-matrix-to-vulkan projection-matrix))

      (let ((modelviewproj
	     (3d-matrices:m* projection-matrix view-matrix model-matrix)))
	
	(with-foreign-object (p-ubo '(:struct UniformBufferObjectVertexShader))
	  
	  (let ((p-mxProj (foreign-slot-pointer p-ubo '(:struct UniformBufferObjectVertexShader) 'mxProj)))
	    
	    (loop for i from 0 to 3
	       do (loop for j from 0 to 3
		     do (setf (mem-aref p-mxProj :float (+ (* j 4) i)) (coerce (3d-matrices:mcref modelviewproj i j) 'single-float)))))
	  
	  
	  (with-foreign-object (pp-data :pointer)
	    (vkMapMemory device annotation-uniform-buffer-vs-memory 0 (foreign-type-size '(:struct UniformBufferObjectVertexShader)) 0 pp-data)
	    (memcpy (mem-aref pp-data :pointer) p-ubo (foreign-type-size '(:struct UniformBufferObjectVertexShader)))
	    (vkUnmapMemory device annotation-uniform-buffer-vs-memory)))))))

(defmethod copy-buffer (app src-buffer dst-buffer size)
  (with-slots (device command-pool queue) app
    (with-vk-struct (p-alloc-info VkCommandBufferAllocateInfo)
      (with-foreign-slots ((vk::level
			    vk::commandPool
			    vk::commandBufferCount)
			   p-alloc-info (:struct VkCommandBufferAllocateInfo))
	(setf vk::level VK_COMMAND_BUFFER_LEVEL_PRIMARY
	      vk::commandPool (elt command-pool 0)
	      vk::commandBufferCount 1)

	(with-foreign-object (p-command-buffer 'VkCommandBuffer)
	  (vkAllocateCommandBuffers device p-alloc-info p-command-buffer)
	  (let ((command-buffer (mem-aref p-command-buffer 'VkCommandBuffer)))
	    (with-vk-struct (p-begin-info VkCommandBufferBeginInfo)
	      (with-foreign-slots ((vk::flags)
				   p-begin-info (:struct VkCommandBufferBeginInfo))
		(setf vk::flags VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
		(vkBeginCommandBuffer command-buffer p-begin-info)
		(with-vk-struct (p-copy-region VkBufferCopy)
		  (with-foreign-slots ((vk::srcOffset
					vk::dstOffset
					vk::size)
				       p-copy-region (:struct VkBufferCopy))
		    (setf vk::srcOffset 0
			  vk::dstOffset 0
			  vk::size size)
		    (vkCmdCopyBuffer command-buffer src-buffer dst-buffer 1 p-copy-region)
		    (vkEndCommandBuffer command-buffer)
		    (with-vk-struct (p-submit-info VkSubmitInfo)
		      (with-foreign-slots ((vk::commandBufferCount
					    vk::pCommandBuffers)
					   p-submit-info (:struct VkSubmitInfo))
			(setf vk::commandBufferCount 1
			      vk::pCommandBuffers p-command-buffer)
			(vkQueueSubmit queue 1 p-submit-info +nullptr+)
			(vkQueueWaitIdle queue)
			(vkFreeCommandBuffers device (elt command-pool 0) 1 p-command-buffer))))))))))))
  (values))

#+OLD
(defmethod destroy-vertex-buffer ((app vkapp) p-vertex-buffer)
  (with-slots (device) app
    (vkDestroyBuffer device p-vertex-buffer +nullptr+)))
	    
(cffi:defcfun ("memcpy" memcpy) :pointer
  (dest :pointer)
  (src :pointer)
  (count size-t))

(defmethod find-memory-type ((app vkapp) type-filter properties)
  (with-slots (gpu) app
    (with-vk-struct (p-mem-properties VkPhysicalDeviceMemoryProperties)
      (with-foreign-slots ((vk::memoryTypeCount)
			   p-mem-properties
			   (:struct VkPhysicalDeviceMemoryProperties))

	(vkGetPhysicalDeviceMemoryProperties gpu p-mem-properties)
	(loop for i from 0 below vk::memoryTypeCount
	     do (when (and (not (zerop (logand type-filter (ash 1 i))))
			   (not (zerop (logand
					(foreign-slot-value (mem-aptr (foreign-slot-pointer p-mem-properties '(:struct VkPhysicalDeviceMemoryProperties)
												   'vk::memoryTypes) '(:struct VkMemoryType) i)
								   '(:struct VkMemoryType) 'vk::propertyFlags)
					properties))))
		  (return i))
	     finally (error "Could not find suitable memory type!"))))))

