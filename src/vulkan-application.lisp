(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :vkapp)

(defvar *debug* t)

(defvar *pipeline-created* nil)

(defun shader-binary-from-list (list)
  (values (foreign-alloc :uint32 :initial-contents list) (* 4 (length list))))

#+NIL
(defun read-shader-file (filename)
  ;; the file is probably already little-endian.
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((buffer (make-array 1024 :element-type '(unsigned-byte 32) :adjustable t :fill-pointer 0))
	  (byte1) (byte2) (byte3) (byte4))
      (loop while (setq byte1 (read-byte stream nil))
	 do (setq byte2 (read-byte stream)
		  byte3 (read-byte stream)
		  byte4 (read-byte stream))
	   (vector-push-extend (let ((word (logior (ash byte4 24) (ash byte3 16) (ash byte2 8) byte1))) (format t "~8,'0,,X " word) word) buffer))
      (let* ((size (fill-pointer buffer))
	     (binary (foreign-alloc (list :array :uint32 size))))
	(loop for word across buffer for i from 0
	   do (setf (mem-aref binary :uint32 i) word))
	(values binary (* 4 size))))))

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

(defmethod create-swapchain ((app vkapp) &key width height (old-swapchain +nullptr+))
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

		  (if (and width height)
		      (setf fb-width width fb-height height)
		      (with-foreign-objects ((p-w :int)
					     (p-h :int))
			(glfwGetFramebufferSize window p-w p-h)
			(setf fb-width (mem-aref p-w :int)
			      fb-height (mem-aref p-h :int))))
		  
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
		   do (setf (elt back-buffer i) (mem-aref p-back-buffer 'VkImage i))))))))))
  (values))

(defmethod recreate-swapchain ((app vkapp) w h)
  ;; Create Swapchain
  (with-slots (device allocator swapchain) app
    (let ((old-swapchain swapchain))
      (create-swapchain app :width w :height h :old-swapchain old-swapchain)
      (unless (null-pointer-p old-swapchain)
	(vkDestroySwapchainKHR device old-swapchain allocator))))
  (values))

(defmethod create-render-pass ((app vkapp))
  ;; Create the Render Pass:
  (with-slots (render-pass surface-format device allocator) app
    (with-vk-struct (p-attachment VkAttachmentDescription)
      (with-foreign-slots ((vk::format vk::samples vk::loadOp vk::storeOp vk::stencilLoadOp
				       vk::stencilStoreOp vk::initialLayout vk::finalLayout)
			   p-attachment (:struct VkAttachmentDescription))
	(setf vk::format (foreign-slot-value surface-format '(:struct VkSurfaceFormatKHR) 'vk::format)
	      vk::samples VK_SAMPLE_COUNT_1_BIT
	      vk::loadOp VK_ATTACHMENT_LOAD_OP_CLEAR
	      vk::storeOp VK_ATTACHMENT_STORE_OP_STORE
	      vk::stencilLoadOp VK_ATTACHMENT_LOAD_OP_DONT_CARE
	      vk::stencilStoreOp VK_ATTACHMENT_STORE_OP_DONT_CARE
	      vk::initialLayout VK_IMAGE_LAYOUT_UNDEFINED
	      vk::finalLayout VK_IMAGE_LAYOUT_PRESENT_SRC_KHR))
      
      (with-vk-struct (p-color-attachment VkAttachmentReference)
	(with-foreign-slots ((vk::attachment vk::layout)
			     p-color-attachment (:struct VkAttachmentReference))
	  (setf vk::attachment 0
		vk::layout VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL))

	(with-foreign-object (p-subpasses '(:struct VkSubpassDescription) 2)
	  
	  (loop for i from 0 below 2
	     do (let ((p-subpass (mem-aptr p-subpasses '(:struct VkSubpassDescription) i)))
		  (zero-struct p-subpass '(:struct VkSubpassDescription))
		  (with-foreign-slots ((vk::pipelineBindPoint vk::colorAttachmentCount vk::pColorAttachments)
				       p-subpass
				       (:struct VkSubpassDescription))
		    (setf vk::pipelineBindPoint VK_PIPELINE_BIND_POINT_GRAPHICS
			  vk::colorAttachmentCount 1
			  vk::pColorAttachments p-color-attachment))))
	  
	  (with-vk-struct (p-info VkRenderPassCreateInfo)
	    (with-foreign-slots ((vk::attachmentCount vk::pAttachments vk::subpassCount vk::pSubpasses)
				 p-info (:struct VkRenderPassCreateInfo))
	      (setf vk::attachmentCount 1
		    vk::pAttachments p-attachment
		    vk::subpassCount 1
		    vk::pSubpasses p-subpasses))
	    (with-foreign-object (p-render-pass 'VkRenderPass)
	      (check-vk-result (vkCreateRenderPass device p-info allocator p-render-pass))
	      (setf render-pass (mem-aref p-render-pass 'VkRenderPass))))))))
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
	       allocator framebuffer render-pass window) app
    (glfwSetFramebufferSizeCallback window (get-callback 'resize-vulkan-callback))
    (with-foreign-object (p-attachment 'VkImageView 1)
      (with-vk-struct (p-info VkFramebufferCreateInfo)
	(with-foreign-slots ((vk::renderPass vk::attachmentCount vk::pAttachments
					     vk::width vk::height vk::layers)
			     p-info (:struct VkFramebufferCreateInfo))
	  (setf vk::renderPass render-pass
		vk::attachmentCount 1
		vk::pAttachments p-attachment
		vk::width fb-width
		vk::height fb-height
		vk::layers 1)
		      
	  (loop for i from 0 below back-buffer-count
	     do (setf (mem-aref p-attachment 'VkImageView 0) (elt back-buffer-view i))
	       (with-foreign-object (p-framebuffer 'VkFramebuffer)
		 (check-vk-result
		  (vkCreateFramebuffer device p-info allocator p-framebuffer))
		 (setf (elt framebuffer i) (mem-aref p-framebuffer 'VkFramebuffer))))))))
  (values))

(defmethod destroy-image-views ((app vkapp))
  (with-slots (back-buffer-count back-buffer-view device allocator) app
    (loop for i from 0 below back-buffer-count
       when (not (null-pointer-p (elt back-buffer-view i)))
       do (vkDestroyImageView device (elt back-buffer-view i) allocator)))
  (values))

(defmethod destroy-framebuffers ((app vkapp))
  (with-slots (back-buffer-count framebuffer device allocator) app
    (loop for i from 0 below back-buffer-count
       when (not (null-pointer-p (elt framebuffer i)))
       do (vkDestroyFramebuffer device (elt framebuffer i) allocator)))
  (values))

(defmethod destroy-render-pass ((app vkapp))
  (with-slots (device render-pass allocator) app
    (when (not (null-pointer-p render-pass))
      (vkDestroyRenderPass device render-pass allocator)))
  (values))

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
      
	    (with-vk-struct (p-vertex-input-info VkPipelineVertexInputStateCreateInfo)
	      (with-foreign-slots ((vk::vertexBindingDescriptionCount
				    vk::pVertexBindingDescriptions
				    vk::vertexAttributeDescriptionCount
				    vk::pVertexAttributeDescriptions)
				   p-vertex-input-info
				   (:struct VkPipelineVertexInputStateCreateInfo))
		(setf vk::vertexBindingDescriptionCount 0
		      vk::pVertexBindingDescriptions +nullptr+
		      vk::vertexAttributeDescriptionCount 0
		      vk::pVertexAttributeDescriptions +nullptr+))
	    
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
			(glfwGetFramebufferSize window p-w p-h)
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
				vk::frontFace VK_FRONT_FACE_CLOCKWISE
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

			      (with-foreign-object (p-dynamic-states 'VkDynamicState)
				(setf (mem-aref p-dynamic-states 'VkDynamicState 0) VK_DYNAMIC_STATE_VIEWPORT
				      (mem-aref p-dynamic-states 'VkDynamicState 1) VK_DYNAMIC_STATE_LINE_WIDTH)
			      
				(with-vk-struct (p-dynamic-state VkPipelineDynamicStateCreateInfo)
				  (with-foreign-slots ((vk::dynamicStateCount
							vk::pDynamicStates)
						       p-dynamic-state
						       (:struct VkPipelineDynamicStateCreateInfo))
				  
				    (setf vk::dynamicStateCount 2
					  vk::pDynamicStates p-dynamic-states))
				  
				  (with-vk-struct (p-pipeline-layout-info VkPipelineLayoutCreateInfo)
				    (with-foreign-slots ((vk::setLayoutCount
							  vk::pSetLayouts
							  vk::pushConstantRangeCount
							  vk::pPushConstantRanges)
							 p-pipeline-layout-info
							 (:struct VkPipelineLayoutCreateInfo))
				    
				      (setf vk::setLayoutCount 0
					    vk::pSetLayouts +nullptr+
					    vk::pushConstantRangeCount 0
					    vk::pPushConstantRanges +nullptr+))
				    
				    (with-foreign-object (p-pipeline-layout 'VkPipelineLayout)
				      (check-vk-result
				       (vkCreatePipelineLayout device
							       p-pipeline-layout-info
							       allocator
							       p-pipeline-layout))
				      (setf pipeline-layout
					    (mem-aref p-pipeline-layout 'VkPipelineLayout)))
				    
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
					(setf vk::flags 0;; VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
					      vk::stageCount 2
					      vk::pStages p-shader-stages
					      vk::pVertexInputState p-vertex-input-info
					      vk::pInputAssemblyState p-input-assembly
					      vk::pViewportState p-viewport-state
					      vk::pRasterizationState p-rasterizer
					      vk::pMultisampleState p-multisampling
					      vk::pDepthStencilState +nullptr+
					      vk::pColorBlendState p-color-blending
					      vk::pDynamicState +nullptr+ ;;p-dynamic-state
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
					(setf graphics-pipeline (mem-aref p-graphics-pipeline 'VkPipeline))))))))))))))))))))
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
    
    (recreate-swapchain app w h)
    
    (create-image-views app)
    
    (create-render-pass app)

    (create-framebuffers app))
  
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
		      
		      vk::enabledLayerCount (if *debug* 2 0)
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

(defmethod create-logical-device ((app vkapp))
  ;; Create Logical Device:
  (with-slots (gpu queue-family queue device allocator) app
    (let ((device-extension-count 1))
      (with-foreign-string (p-device-extensions "VK_KHR_swapchain")
	(with-foreign-object (pp-device-extensions :pointer)
	  (setf (mem-aref pp-device-extensions :pointer) p-device-extensions) 
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
								 vk::ppEnabledExtensionNames)
				       p-create-info (:struct VkDeviceCreateInfo))
		    (setf vk::queueCreateInfoCount 1
			  vk::pQueueCreateInfos p-queue-info
			  vk::enabledExtensionCount device-extension-count
			  vk::ppEnabledExtensionNames pp-device-extensions)

		    (with-foreign-object (p-device 'VkDevice)
		      (check-vk-result (vkCreateDevice gpu p-create-info allocator p-device))
		      (setf device (mem-aref p-device 'VkDevice))
		      (with-foreign-object (p-queue 'VkQueue)
			(vkGetDeviceQueue device queue-family queue-index p-queue)
			(setf queue (mem-aref p-queue 'VkQueue)))))))))))))
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
	    1000)

      (with-vk-struct (p-pool-info VkDescriptorPoolCreateInfo)
	(with-foreign-slots ((vk::flags vk::maxSets vk::poolSizeCount vk::pPoolSizes)
			     p-pool-info (:struct VkDescriptorPoolCreateInfo))
	  (setf vk::flags VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
		vk::maxSets (* 11 1000)
		vk::poolSizeCount 11
		vk::pPoolSizes p-pool-sizes)
	  (with-foreign-object (p-descriptor-pool 'VkDescriptorPool)
	    (check-vk-result (vkCreateDescriptorPool device p-pool-info allocator p-descriptor-pool))
	    (setf descriptor-pool (mem-aref p-descriptor-pool 'VkDescriptorPool)))))))

  (values))

(defmethod create-command-buffers ((app vkapp))
  ;; Create Command Buffers:
  (with-slots (queue-family command-pool device allocator command-buffer fence
			    present-complete-semaphore render-complete-semaphore) app
    (loop for i from 0 below IMGUI_VK_QUEUED_FRAMES
       do (with-vk-struct (p-info VkCommandPoolCreateInfo)
	    (with-foreign-slots ((vk::flags vk::queueFamilyIndex)
				 p-info (:struct VkCommandPoolCreateInfo))
	      (setf vk::flags VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
		    vk::queueFamilyIndex queue-family)
	      (with-foreign-object (p-command-pool 'VkCommandPool)
		(check-vk-result (vkCreateCommandPool device p-info allocator p-command-pool))
		(setf (elt command-pool i) (mem-aref p-command-pool 'VkCommandPool)))))

	 (with-vk-struct (p-info VkCommandBufferAllocateInfo)
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



#+NOTYET
(defmethod create-descriptor-set-layout ((app vkapp))
  (with-vk-struct (p-ubo-layout-binding VkDescriptorSetLayoutBinding)
    (with-foreign-slots ((vk::binding
			  vk::descriptorCount
			  vk::descriptorType
			  vk::pImmutableSamplers
			  vk::stageFlags))
      (setf vk::binding 0
	    vk::descriptorCount 1
	    vk::descriptorType VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
	    ;;later when we have vertex data
	    ))))

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
    
  (create-graphics-pipeline app)
    
  (create-framebuffers app)
    
  (create-command-buffers app) ;; this also creates semaphores...todo: separate these out
    
  (create-descriptor-pool app)
    
  (values))

(defmethod cleanup-vulkan ((app vkapp))
  (with-slots (device descriptor-pool allocator fence command-pool command-buffer debug-report
		      present-complete-semaphore render-complete-semaphore back-buffer-count
		      back-buffer-view framebuffer render-pass swapchain instance surface
		      vert-shader-module frag-shader-module pipeline-layout graphics-pipeline) app

    (vkDestroyDescriptorPool device descriptor-pool allocator)

    (loop for i from 0 below IMGUI_VK_QUEUED_FRAMES
       do 
	 (vkDestroyFence device (elt fence i) allocator)
	 
	 (with-foreign-object (p-command-buffers-i 'VkCommandBuffer)
	   (setf (mem-aref p-command-buffers-i 'VkCommandBuffer) (elt command-buffer i))
	   (vkFreeCommandBuffers device (elt command-pool i) 1 p-command-buffers-i))
	 
	 (vkDestroyCommandPool device (elt command-pool i) allocator)
	 
	 (vkDestroySemaphore device (elt present-complete-semaphore i) allocator)
	 
	 (vkDestroySemaphore device (elt render-complete-semaphore i) allocator))

    (destroy-image-views app)
    (destroy-framebuffers app)

    ;;(vkDestroyPipelineLayout device pipeline-layout allocator)
    ;;(vkDestroyPipeline device graphics-pipeline allocator)

    (destroy-render-pass app)
    
    (vkDestroySwapchainKHR device swapchain allocator)
    
    (vkDestroySurfaceKHR instance surface allocator)
    
    (when *debug*
      (vkDestroyDebugReportCallbackEXT instance instance debug-report allocator))
    
    (vkDestroyDevice device allocator)
    
    (vkDestroyInstance instance allocator)

    (deallocate-image-range app)

    (deallocate-surface-format app)
    
    (values)))

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

    (with-vk-struct (p-info VkRenderPassBeginInfo)
      (with-foreign-slots ((vk::renderPass vk::framebuffer vk::renderArea
					   vk::clearValueCount vk::pClearValues)
			   p-info (:struct VkRenderPassBeginInfo))

	(with-foreign-object (p-clear-value :float 4)
	  (setf (mem-aref p-clear-value :float 0) (elt clear-value 0)
		(mem-aref p-clear-value :float 1) (elt clear-value 1)
		(mem-aref p-clear-value :float 2) (elt clear-value 2)
		(mem-aref p-clear-value :float 3) (elt clear-value 3))

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
		 		  
		  vk::clearValueCount 1
		  vk::pClearValues p-clear-value)

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

  (with-slots (window allocator gpu device render-pass queue command-pool command-buffer graphics-pipeline
		      image-range pipeline-cache descriptor-pool clear-value frame-index surface-format) app

    
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

    (let ((show-demo-window 1)
	  (show-another-window 0))
      (with-foreign-object (p-clear-color :float 4)
	(setf (mem-aref p-clear-color :float 0) 0.45f0
	      (mem-aref p-clear-color :float 1) 0.55f0
	      (mem-aref p-clear-color :float 2) 0.60f0
	      (mem-aref p-clear-color :float 3) 1.00f0)

	(when *imgui-unlimited-frame-rate*
	  (ImGui_ImplGlfwVulkan_NewFrame)
	  (frame-begin app)
	  (ImGui_ImplGlfwVulkan_Render (elt command-buffer frame-index))
	  (frame-end app)
	  (setf frame-index (mod (1+ frame-index) IMGUI_VK_QUEUED_FRAMES)))


	(let ((last-time (get-internal-real-time))
	      (frame-counter 0))
	
	(loop while (zerop (glfwWindowShouldClose window))
	     
	   do (glfwPollEvents) 
	     (ImGUI_ImplGlfwVulkan_NewFrame)

	     (let ((f 0.1f0))
	       (let ((temp)
		     (dt))

		 (incf frame-counter)
	       (igText "Hello, wworld!")

	       (with-foreign-object (p-f :float)
		 (setf (mem-aref p-f :float) f)

		 (igSliderFloat "float" p-f 0.0f0 1.0f0 "" 1.0f0)

		 (igColorEdit3 "clear color" p-clear-color 0)

		 (when (not (null-pointer-p (igButton "Demo Window" (list 'ig::x 100f0 'ig::y 20.0f0))))
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
		       last-time temp))))

	     (with-foreign-objects ((p-show-demo-window :int)
				    (p-show-another-window :int))
	       (setf (mem-aref p-show-demo-window :int) show-demo-window
		     (mem-aref p-show-another-window :int) show-another-window)

	       (when (not (zerop show-another-window))

		 (igBegin "Another Window" p-show-another-window 0)

		 (igText "Hello from another window!")

		 (igEnd))
	       
	       (when (not (zerop show-demo-window))

		 (igSetNextWindowPos (list 'ig::x 650.0f0 'ig::y 20.0f0)
				     ImGuiCond_FirstUseEver (list 'ig::x 0.0f0 'ig::y 0.0f0))

		 (igShowDemoWindow p-show-demo-window)))
		 
	     (setf (elt clear-value 0) (mem-aref  p-clear-color :float 0)
		   (elt clear-value 1) (mem-aref  p-clear-color :float 1)
		   (elt clear-value 2) (mem-aref  p-clear-color :float 2)
		   (elt clear-value 3) (mem-aref  p-clear-color :float 3))

	     (frame-begin app)
	     (vkCmdBindPipeline (elt command-buffer frame-index) VK_PIPELINE_BIND_POINT_GRAPHICS graphics-pipeline)
	     (vkCmdDraw (elt command-buffer frame-index) 3 1 0 0)
	     (ImGui_ImplGlfwVulkan_Render (elt command-buffer frame-index))
	     (frame-end app)
	     (frame-present app)))


	(check-vk-result (vkDeviceWaitIdle device))
	(ImGui_ImplGlfwVulkan_Shutdown)
	(cleanup-vulkan app)
	(glfwTerminate)

	t))))
	     
(defun run-app (&optional (app (make-instance 'vkapp)))
  (sb-thread:make-thread #'(lambda () (main app))))
				   
(defmethod create-vertex-buffer ((app vkapp))
  (with-slots (device vertex-buffer vertex-buffer-memory vertex-data vertex-data-size) app
    (with-foreign-objects ((p-vertex-buffer 'VkBuffer)
			   (p-vertex-buffer-memory 'VkDeviceMemory))
	
      (with-vk-struct (p-info VkBufferCreateInfo)
	(with-foreign-slots ((vk::size vk::usage vk::sharingMode)
			     p-info (:struct VkBufferCreateInfo))
	  (setf vk::size vertex-data-size
		vk::usage VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
		vk::sharingMode VK_SHARING_MODE_EXCLUSIVE)
	    
	  (check-vk-result (vkCreateBuffer device p-info +nullptr+ p-vertex-buffer))
	  (setq vertex-buffer (mem-aref p-vertex-buffer 'VkBuffer))
	
	  (with-vk-struct (p-mem-requirements VkMemoryRequirements)
	    (vkGetBufferMemoryRequirements device vertex-buffer p-mem-requirements)

	    (with-vk-struct (p-alloc-info VkMemoryAllocateInfo)
	      (with-foreign-slots ((vk::allocationSize vk::memoryTypeIndex)
				   p-alloc-info (:struct VkMemoryAllocateInfo))
		(setf vk::allocationSize (foreign-slot-value p-mem-requirements '(:struct VkMemoryRequirements)
							      'vk::size)
		      vk::memoryTypeIndex
		      (find-memory-type app
					(foreign-slot-value p-mem-requirements '(:struct VkMemoryRequirements)
							    'vk::memoryTypeBits)
					(logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
						VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)))
		
		(check-vk-result (vkAllocateMemory device p-alloc-info +nullptr+ p-vertex-buffer-memory))
		(setq vertex-buffer-memory (mem-aref p-vertex-buffer-memory 'VkDeviceMemory)))))

	  (vkBindBufferMemory device vertex-buffer vertex-buffer-memory 0)
	  
	  (with-foreign-object (pp-data :pointer)
	    (vkMapMemory device vertex-buffer-memory 0 vertex-data-size 0 pp-data)
	    (memcpy (mem-aref pp-data :pointer) vertex-data vk::size)
	    (vkUnmapMemory device vertex-buffer-memory))))))
  (values))

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
      (with-foreign-slots ((vk::memoryTypeCount vk::memoryTypes)
			   p-mem-properties
			   (:struct VkPhysicalDeviceMemoryProperties))
			   
	(vkGetPhysicalDeviceMemoryProperties gpu p-mem-properties)
	(loop for i from 0 below vk::memoryTypeCount
	     do (when (and (not (zerop (logand type-filter (ash 1 i))))
			   (not (zerop (logand
					(foreign-slot-value (mem-aptr vk::memoryTypes '(:struct VkMemoryType) i)
							    '(:struct VkMemoryType) 'vk::propertyFlags)
					properties))))
		  (return i))
	     finally (error "Could not find suitable memory type!"))))))
		    
		    


#+NIL
(defclass vkapp ()
  ((window) ;; os window
  
   (instance) ;; VkInstance
   (callback) ;; VkDebugReportCallbackEXT
   (surface)  ;; VkSurfaceKHR
  
   (physical-device :initform VK_NULL_HANDLE) ;; VkPhysicalDevice
   (device)				      ;; VkDevice

   (graphics-queue) ;; VkQueue
   (present-queue)  ;; VkQueue

   (swapchain) ;; VkSwapchainKHR
   (swapchain-images
    :initform (make-array 10 :initial-element VK_NULL_HANDLE :adjustable t :fill-pointer 3)) ;; VkImage
   (swapchain-image-format) ;; VkFormat
   (swapchain-extent)	    ;; VkExtent2D
   (swapchain-image-views
    :initform (make-array 10 :initial-element VK_NULL_HANDLE :adjustable t :fill-pointer 3)) ;; VkImageView
   (swapchain-framebuffers
    :initform (make-array 10 :initial-element VK_NULL_HANDLE :adjustable t :fill-pointer 3)) ;; VkFramebuffer

   (render-pass)	  ;; VkRenderPass
   (descriptor-set-layout) ;; VkDescriptorSetLayout
   (pipeline-layout)	   ;; VkPipelineLayout
   (graphics-pipeline)	   ;; VkPipeline

   (command-pool) ;; VkCommandPool

   (depth-image)       ;; VkImage
   (depth-image-memory) ;; VkDeviceMemory
   (depth-image-view)	;; VkImageView

   (texture-image)	 ;; VkImage
   (texture-image-memory) ;; VkDeviceMemory
   (texture-image-view)	  ;; VkImageView
   (texture-sampler)	  ;; VkSampler

   ;; it seems it would only make sense to use lisp arrays if we were running AllegroCL :-(
   ;;(vertices :initform (make-array (* 5 4096) :element-type 'single-float :initial-element 0.0f0 :adjustable t :fill-pointer 15))
   ;;(indices :initform (make-array 4096 :element-type '(unsigned-byte 32) :initial-element 0 :djustable t :fill-pointer 3))

   (vertices-host-memory) ;; cffi pointer to foreign array of floats
   (vertices-size)	  ;; cl:integer
   (indices-host-memory) ;; cffi pointer to foreign array of unsigned char 32
   (indices-size)	 ;; cl:integer
   (vertex-buffer)	 ;; VkBuffer
   (vertex-buffer-memory) ;; VkDeviceMemory
   (index-buffer)	  ;; VkBuffer
   (index-buffer-memory)  ;; VkDeviceMemory

   (uniform-buffer)	  ;; VkBuffer
   (uniform-buffer-memory) ;; VkDeviceMemory

   (descriptor-pool) ;; VkDescriptorPool
   (descriptor-set)  ;; VkDescriptorSet

   (command-buffers
    :initform (make-array 24 :initial-element VK_NULL_HANDLE :adjustable t :fill-pointer 0))

   (image-available-semaphore) ;; VkSemaphore
   (render-finished-semaphore))) ;; VkSemaphore
  
  
;; consider writing the vulkan part of the imgui demo instead of this one.
