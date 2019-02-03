(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :vkapp)

(defconstant +NULL+ 0)
(defconstant VK_TRUE 1)
(defconstant VK_FALSE 0)
(defconstant IMGUI_MAX_POSSIBLE_BACK_BUFFERS 16)
(defconstant UINT64_MAX #.(1- (expt 2 64)))

(defparameter +nullptr+ (cffi-sys:null-pointer))
(defparameter VK_NULL_HANDLE +nullptr+)
(defparameter *imgui-unlimited-frame-rate* nil)
(defconstant VK_QUEUE_FAMILY_IGNORED 0)
(defconstant VK_IMAGE_LAYOUT_UNDEFINED 0)
(defconstant VK_SUBPASS_EXTERNAL 0)

(defcstruct vec2
  (a :float)
  (b :float))

(defcstruct vec3
  (a :float)
  (b :float)
  (c :float))

(defcstruct Vertex
  (pos (:struct vec3))
  (color (:struct vec3)))

(defparameter *vertices-list*
  (list -0.5f0 -0.5f0 0.0f0 1.0f0 0.0f0 0.0f0
	 0.5f0 -0.5f0 0.0f0 0.0f0 1.0f0 0.0f0
	 0.5f0  0.5f0 0.0f0 0.0f0 0.0f0 1.0f0
	 -0.5f0  0.5f0 0.0f0 1.0f0 1.0f0 1.0f0

	 -0.5f0 -0.5f0 -0.5f0 1.0f0 0.0f0 0.0f0
	 0.5f0 -0.5f0 -0.5f0 0.0f0 1.0f0 0.0f0
	 0.5f0  0.5f0 -0.5f0 0.0f0 0.0f0 1.0f0
	 -0.5f0  0.5f0 -0.5f0 1.0f0 1.0f0 1.0f0))

(defparameter *indices*
  (list 0 1 2 2 3 0
	4 5 6 6 7 4))

(defparameter *axes-coord-list*
  (list 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0
	1.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0
	0.0f0 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0
	0.0f0 1.0f0 0.0f0 0.0f0 1.0f0 0.0f0
	0.0f0 0.0f0 0.0f0 0.0f0 0.0f0 1.0f0
	0.0f0 0.0f0 1.0f0 0.0f0 0.0f0 1.0f0))

(defparameter *axes-indices*
  (list 0 1 2 3 4 5))

(defcstruct mat4
  (x0 :float)
  (y0 :float)
  (z0 :float)
  (w0 :float)
  (x1 :float)
  (y1 :float)
  (z1 :float)
  (w1 :float)
  (x2 :float)
  (y2 :float)
  (z2 :float)
  (w2 :float)
  (x3 :float)
  (y3 :float)
  (z3 :float)
  (w3 :float))

(defcstruct vec4
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(defun make-mat4 (initial-contents)
  (assert (eq (length initial-contents) 16))
  (foreign-alloc :float :initial-contents initial-contents))

#+ORIG
(defcstruct UniformBufferObjectVertexShader
  (model (:struct mat4))
  (view (:struct mat4))
  (proj (:struct mat4)))

(defcstruct UniformBufferObjectVertexShader
  (mxProj (:struct mat4)))

(defcstruct UniformBufferObjectGeometryShader
  (mxProj (:struct mat4))
  (RayOrigin (:struct vec4))
  (RayDir (:struct vec4)))

(defvar *last-app-id* 0)

(defclass vkapp ()
  ((app-id :accessor app-id)
   (window :initform +nullptr+) ;; GLFWwindow
   (mouse-pressed :initform (make-array 3 :initial-element nil))
   (mouse-wheel :initform 0.0f0)
   
   (allocator :initform VK_NULL_HANDLE)	 ;; VkAllocationCallbacks
   (instance :initform VK_NULL_HANDLE)	 ;; VkInstance
   (surface :initform VK_NULL_HANDLE)	 ;; VkSurfaceKHR
   (gpu :initform VK_NULL_HANDLE)	 ;; VkPhysicalDevice
   (device :initform VK_NULL_HANDLE)	 ;; VkDevice
   (swapchain :initform VK_NULL_HANDLE)	 ;; VkSwapchainKHR
   (render-pass :initform VK_NULL_HANDLE) ;; VkRenderPass
   (queue-family :initform 0)
   (queue :initform VK_NULL_HANDLE)	  ;; VkQueue
   (debug-report :initform VK_NULL_HANDLE) ;; VkDebugReportCallbackEXT

   (surface-format) ;; VKSurfaceFormatKHR
   (image-range) ;; VkImageSubresourceRange
   (present-mode) ;; VkPresentMode

   (pipeline-cache :initform VK_NULL_HANDLE) ;; VkPipelineCache
   (descriptor-pool :initform VK_NULL_HANDLE) ;; VkDescriptorPool

   (descriptor-set)
   (annotation-descriptor-set)
   (descriptor-set-layout)
   (selection-descriptor-set-layout)
  
   (fb-width)
   (fb-height)

   (depth-image)
   (depth-image-memory)
   (depth-image-view)
   
   (back-buffer-indices
    :initform (make-array IMGUI_VK_QUEUED_FRAMES
			  :initial-element nil :adjustable t :fill-pointer IMGUI_VK_QUEUED_FRAMES))

   (back-buffer-count :initform 0)
   (back-buffer
    :initform (make-array IMGUI_MAX_POSSIBLE_BACK_BUFFERS :adjustable t
			  :initial-element nil :fill-pointer IMGUI_MAX_POSSIBLE_BACK_BUFFERS)) ;; VkImage
   (back-buffer-view
    :initform (make-array IMGUI_MAX_POSSIBLE_BACK_BUFFERS :adjustable t
			  :initial-element nil :fill-pointer IMGUI_MAX_POSSIBLE_BACK_BUFFERS)) ;; VkImageView
   (framebuffer
    :initform (make-array IMGUI_MAX_POSSIBLE_BACK_BUFFERS :adjustable t
			  :initial-element nil :fill-pointer IMGUI_MAX_POSSIBLE_BACK_BUFFERS)) ;; VkFramebuffer
   
   (frame-index :initform 0)
   (command-pool
    :initform (make-array IMGUI_VK_QUEUED_FRAMES :adjustable t
			  :initial-element nil :fill-pointer IMGUI_VK_QUEUED_FRAMES)) ;; VkCommandPool
   (command-buffer
    :initform
    (make-array IMGUI_VK_QUEUED_FRAMES :adjustable t
		:initial-element nil :fill-pointer IMGUI_VK_QUEUED_FRAMES)) ;; VkCommandBuffer
   (fence
    :initform (make-array IMGUI_VK_QUEUED_FRAMES :adjustable t
			  :initial-element nil :fill-pointer IMGUI_VK_QUEUED_FRAMES)) ;; VkFence
   (present-complete-semaphore
    :initform (make-array IMGUI_VK_QUEUED_FRAMES :adjustable t
			  :initial-element nil :fill-pointer IMGUI_VK_QUEUED_FRAMES)) ;; VkSemaphore
   (render-complete-semaphore
    :initform (make-array IMGUI_VK_QUEUED_FRAMES :adjustable t
			  :initial-element nil :fill-pointer IMGUI_VK_QUEUED_FRAMES)) ;; VkSemaphore

   (clear-value :initform (make-array 4 :element-type 'single-float :initial-element 0.0f0)) ;; VkClearValue

   (vertex-shader-code)
   (vertex-shader-code-size)
   (vert-shader-module)
   (fragment-shader-code)
   (fragment-shader-code-size)
   (frag-shader-module)

   (vertex-buffer
    #+NIL
    :initform 
    #+NIL(make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; VkBuffer
   (index-buffer
    #+NIL
    :initform
    #+NIL(make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; VkBuffer
   (vertex-buffer-memory
    #+NIL
    :initform
    #+NIL(make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; VkDeviceMemory
   (index-buffer-memory
    #+NIL
    :initform
    #+NIL(make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; VkDeviceMemory

   (axes-vertex-buffer) ;; VkBuffer
   (axes-index-buffer) ;; VkBuffer
   (axes-vertex-buffer-memory) ;; VkDeviceMemory
   (axes-index-buffer-memory) ;; VkDeviceMemory
   
   (vertex-data :initarg :vertex-data
    :initform (foreign-alloc :float :initial-contents *vertices-list*)) ;; (:pointer :float)
   (index-data :initarg :index-data
    :initform (foreign-alloc :unsigned-short :initial-contents *indices*)) ;; (:pointer :uint32)
   (vertex-data-size :initarg :vertex-data-size
    :initform (* (foreign-type-size :float) (length *vertices-list*))
    #+NIL :initform #+NIL (make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; size-t
   (index-data-size :initarg :index-data-size
    :initform (* (foreign-type-size :unsigned-short) (length *indices*))
    #+NIL(make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; size-t

   (axes-vertex-data
    :initform (foreign-alloc :float :initial-contents *axes-coord-list*))
   (axes-index-data
    :initform (foreign-alloc :unsigned-short :initial-contents *axes-indices*))
   (axes-vertex-data-size
    :initform (* (foreign-type-size :float) (length *axes-coord-list*)))
   (axes-index-data-size
    :initform (* (foreign-type-size :unsigned-short) (length *axes-indices*)))

   (uniform-buffer-vs)
   (uniform-buffer-vs-memory)

   (uniform-buffer-gs)
   (uniform-buffer-gs-memory)

   (ssbo-buffer-gs)
   (ssbo-buffer-gs-memory)
   

   (annotation-uniform-buffer-vs)
   (annotation-uniform-buffer-vs-memory)

   (upload-buffer-memory) ;; VkDeviceMemory
   (upload-buffer) ;; VkBuffer

   (font-sampler) ;; VkSampler
   (font-memory) ;; VkDeviceMemory
   (font-image) ;; VkImage
   (font-view) ;; VkImageView

   (annotation-pipeline-layout)
   (graphics-pipeline)
   (annotation-pipeline)
   (selection-pipeline)
   (selection-pipeline-layout)

   (start-time)
   (current-item :initform 0)))

(defclass shader ()
  ((code-binary :accessor shader-binary)
   (binary-size :accessor shader-binary-size)
   (handle :accessor shader-module)))

(defclass vertex-shader (shader) ())

(defmethod shader-stage-bit ((shader vertex-shader))
  VK_SHADER_STAGE_VERTEX_BIT)

(defclass fragment-shader (shader) ())

(defmethod shader-stage-bin ((shader fragment-shader))
  VK_SHADER_STAGE_FRAGMENT_BIT)

(defclass pipeline ()
  ((cache :initform nil)
   (info :initform nil)
   (handle :initform nil)))

(defclass graphics-pipeline (pipeline) ())



(defvar *shader-entry-name* (cffi:foreign-string-alloc "main"))

(defun fill-pipeline-shader-stage-create-info (p-info &key
							stage
							module
							(p-name *shader-entry-name*)
							&allow-other-keys)
  (vk::zero-struct p-info '(:struct VkPipelineShaderStageCreateInfo))
  (with-foreign-slots ((vk::sType vk::pNext vk::flags vk::stage vk::module vk::pName)
		       p-info
		       (:struct VkPipelineShaderStageCreateInfo))
    (setf vk::sType VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
	  vk::pNext +nullptr+
	  vk::stage stage
	  vk::module module
	  vk::pName p-name))
  (values))

(defun fill-vertex-input-binding-description (p
					      &key
						(input-binding-number 0)
						stride
						(input-rate VK_VERTEX_INPUT_RATE_VERTEX)
						&allow-other-keys)
  (with-foreign-slots ((vk::binding
			vk::stride
			vk::inputRate)
		       p
		       (:struct VkVertexInputBindingDescription))
    (setf vk::binding input-binding-number
	  vk::stride stride
	  vk::inputRate input-rate)))

(defmacro with-vertex-input-binding-description ((var)
						 &body body)
  
  `(with-vk-struct (,var VkVertexInputBindingDescription)
     
     ,@body))

(defun fill-vertex-input-attribute-description (p-attribute-description &key
									  (attribute-binding-number 0)
									  (location 0)
									  (format VK_FORMAT_R32G32B32_SFLOAT)
									  offset
									  &allow-other-keys)
  (zero-struct p-attribute-description '(:struct VkVertexInputAttributeDescription))
  (with-foreign-slots ((vk::binding
			vk::location
			vk::format
			vk::offset)
		       p-attribute-description
		       (:struct VkVertexInputAttributeDescription))
    (setf vk::binding attribute-binding-number
	  vk::location location
	  vk::format format
	  vk::offset offset))
  (values))									     

(defun fill-pipeline-vertex-input-state-create-info (p-ci
						     &key
						       (vertex-binding-description-count 1)
						       p-vertex-binding-descriptions
						       (vertex-attribute-description-count 2)
						       p-vertex-attribute-descriptions
						       &allow-other-keys)
  (with-foreign-slots ((vk::vertexBindingDescriptionCount
			vk::pVertexBindingDescriptions
			vk::vertexAttributeDescriptionCount
			vk::pVertexAttributeDescriptions)
		       p-ci
		       (:struct VkPipelineVertexInputStateCreateInfo))
    (setf vk::vertexBindingDescriptionCount vertex-binding-description-count
	  vk::pVertexBindingDescriptions p-vertex-binding-descriptions
	  vk::vertexAttributeDescriptionCount vertex-attribute-description-count
	  vk::pVertexAttributeDescriptions p-vertex-attribute-descriptions))
  (values))

  
(defmacro with-pipeline-vertex-input-state-create-info ((var)
							&body body)
  `(with-vk-struct (,var VkPipelineVertexInputStateCreateInfo)
     
     ,@body))

(defun fill-pipeline-input-assembly-state-create-info (p-ci
						       &key
							 (topology VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)
							 (primitive-restart-enable VK_FALSE)
							 &allow-other-keys)
  (with-foreign-slots ((vk::topology
			vk::primitiveRestartEnable)
		       p-ci
		       (:struct VkPipelineInputAssemblyStateCreateInfo))
    (setf vk::topology topology
	  vk::primitiveRestartEnable primitive-restart-enable))
  (values))

  
(defmacro with-pipeline-input-assembly-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineInputAssemblyStateCreateInfo)
     ,@body))

(defun fill-viewport-structure (p
				&key
				  (viewport-x 0.0f0)
				  (viewport-y 0.0f0)
				  viewport-width
				  viewport-height
				  (min-depth 0.0f0)
				  (max-depth 1.0f0)
				  &allow-other-keys)
  (with-foreign-slots ((vk::x
			vk::y
			vk::width
			vk::height
			vk::minDepth
			vk::maxDepth)
		       p (:struct VkViewport))
    (setf vk::x viewport-x
	  vk::y viewport-y
	  vk::width viewport-width
	  vk::height viewport-height
	  vk::minDepth min-depth
	  vk::maxDepth max-depth))
  (values))

(defmacro with-viewport-structure ((var) &body body)
  `(with-vk-struct (,var VkViewport)
     ,@body))

(defun fill-scissor-structure (p
			       &key
				 (scissor-x 0)
				 (scissor-y 0)
				 scissor-width
				 scissor-height
				 &allow-other-keys)
  (setf (foreign-slot-value
	    (foreign-slot-pointer p '(:struct VkRect2D) 'vk::offset)
	    '(:struct VkOffset2D)
	    'vk::x) scissor-x
				 
	    (foreign-slot-value
	     (foreign-slot-pointer p '(:struct VkRect2D) 'vk::offset)
	     '(:struct VkOffset2D)
	     'vk::y) scissor-y
				  
	     (foreign-slot-value
	      (foreign-slot-pointer p '(:struct VkRect2D) 'vk::extent)
	      '(:struct VkExtent2D)
	      'vk::width) scissor-width
				   
	      (foreign-slot-value
	       (foreign-slot-pointer p '(:struct VkRect2D) 'vk::extent)
	       '(:struct VkExtent2D)
	       'vk::height) scissor-height)
  (values))
  
(defmacro with-scissor-structure ((var) &body body)
  `(with-vk-struct (,var VkRect2D)
     ,@body))

(defun fill-pipeline-viewport-state-create-info (p-ci
						 &key
						   (viewport-count 1)
						   p-viewports
						   (scissor-count 1)
						   p-scissors
						   &allow-other-keys)
  (with-foreign-slots ((vk::viewportCount
			vk::pViewports
			vk::scissorCount
			vk::pScissors)
		       p-ci
		       (:struct VkPipelineViewportStateCreateInfo))
    (setf vk::viewportCount viewport-count
	  vk::pViewports p-viewports
	  vk::scissorCount scissor-count
	  vk::pScissors p-scissors))
  (values))
  

(defmacro with-pipeline-viewport-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineViewportStateCreateInfo)
     ,@body))

(defun fill-pipeline-rasterization-state-create-info (p-ci
						      &key
							(depth-clamp-enable VK_TRUE)
							(rasterizer-discard-enable VK_FALSE)
							(polygon-mode VK_POLYGON_MODE_FILL)
							(line-width 1.0f0)
							(cull-mode VK_CULL_MODE_NONE)
							(front-face VK_FRONT_FACE_COUNTER_CLOCKWISE)
							(depth-bias-enable VK_FALSE)
							(depth-bias-constant-factor 0.0f0)
							(depth-bias-clamp 0.0f0)
							(depth-bias-slope-factor 0.0f0)
							&allow-other-keys)
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
		       p-ci
		       (:struct VkPipelineRasterizationStateCreateInfo))
    (setf vk::depthClampEnable depth-clamp-enable
	  vk::rasterizerDiscardEnable rasterizer-discard-enable
	  vk::polygonMode polygon-mode
	  vk::lineWidth line-width
	  vk::cullMode cull-mode
	  vk::frontFace front-face
	  vk::depthBiasEnable depth-bias-enable
	  vk::depthBiasConstantFactor depth-bias-constant-factor
	  vk::depthBiasClamp depth-bias-clamp
	  vk::depthBiasSlopeFactor depth-bias-slope-factor))
  (values))
  
(defmacro with-pipeline-rasterization-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineRasterizationStateCreateInfo)
     
     ,@body))

(defun fill-pipeline-multisample-state-create-info (p-ci
						    &key
						      (sample-shading-enable VK_FALSE)
						      (rasterization-samples VK_SAMPLE_COUNT_1_BIT)
						      (min-sample-shading 1.0f0)
						      (p-sample-mask +nullptr+)
						      (alpha-to-coverage-enable VK_FALSE)
						      (alpha-to-one-enable VK_FALSE)
						      &allow-other-keys)
  (with-foreign-slots ((vk::sampleShadingEnable
			vk::rasterizationSamples
			vk::minSampleShading
			vk::pSampleMask
			vk::alphaToCoverageEnable
			vk::alphaToOneEnable)
		       p-ci
		       (:struct VKPipelineMultisampleStateCreateInfo))
    (setf vk::sampleShadingEnable sample-shading-enable
	  vk::rasterizationSamples rasterization-samples
	  vk::minSampleShading min-sample-shading
	  vk::pSampleMask p-sample-mask
	  vk::alphaToCoverageEnable alpha-to-coverage-enable
	  vk::alphaToOneEnable alpha-to-one-enable))
  (values))
  
(defmacro with-pipeline-multisample-state-create-info ((var) &body body)

  `(with-vk-struct (,var VKPipelineMultisampleStateCreateInfo)
     ,@body))

(defun fill-graphics-pipeline-create-info (p-ci &key
						  (flags 0)
						  (stage-count 2)
						  p-stages
						  p-vertex-input-state
						  p-input-assembly-state
						  p-viewport-state
						  p-rasterization-state
						  p-multisample-state
						  p-depth-stencil-state
						  p-color-blend-state
						  p-dynamic-state
						  layout
						  render-pass
						  (subpass 0)
						  (base-pipeline-handle +nullptr+)
						  (base-pipeline-index -1)
						  &allow-other-keys)


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
		       p-ci
		       (:struct VkGraphicsPipelineCreateInfo))
    (setf vk::flags flags
	  vk::stageCount stage-count
	  vk::pStages p-stages
	  vk::pVertexInputState p-vertex-input-state
	  vk::pInputAssemblyState p-input-assembly-state
	  vk::pViewportState p-viewport-state
	  vk::pRasterizationState p-rasterization-state
	  vk::pMultisampleState p-multisample-state
	  vk::pDepthStencilState p-depth-stencil-state
	  vk::pColorBlendState p-color-blend-state
	  vk::pDynamicState p-dynamic-state
	  vk::layout layout
	  vk::renderPass render-pass
	  vk::subpass subpass
	  vk::basePipelineHandle base-pipeline-handle
	  vk::basePipelineIndex base-pipeline-index))
  (values))
  

(defmacro with-graphics-pipeline-create-info ((var) &body body)
  `(with-vk-struct (,var VkGraphicsPipelineCreateInfo)
     ,@body))

(defun fill-pipeline-depth-stencil-state-create-info (p-ci
					     &key
					       (depth-test-enable VK_TRUE)
					       (depth-write-enable VK_TRUE)
					       (depth-compare-op VK_COMPARE_OP_LESS)
					       (depth-bounds-test-enable VK_FALSE)
					       (stencil-test-enable VK_FALSE)
					       &allow-other-keys)
  (with-foreign-slots ((vk::depthTestEnable
			vk::depthWriteEnable
			vk::depthCompareOp
			vk::depthBoundsTestEnable
			vk::stencilTestEnable)
		       p-ci (:struct VkPipelineDepthStencilStateCreateInfo))
    (setf vk::depthTestEnable depth-test-enable
	  vk::depthWriteEnable depth-write-enable
	  vk::depthCompareOp depth-compare-op
	  vk::depthBoundsTestEnable depth-bounds-test-enable
	  vk::stencilTestEnable stencil-test-enable))
  (values))


(defmacro with-pipeline-depth-stencil-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineDepthStencilStateCreateInfo)
     ,@body))

(defmacro with-descriptor-set-layouts ((var count) &body body)
  `(with-foreign-object (,var 'VkDescriptorSetLayout ,count)
     ,@body))
     

(defun fill-pipeline-layout-create-info (p-ci
					 &key
					   (set-layout-count 1)
					   p-set-layouts
					   (push-constant-range-count 0)
					   (p-push-constant-ranges +nullptr+)
					   &allow-other-keys)
  (with-foreign-slots ((vk::setLayoutCount
			vk::pSetLayouts
			vk::pushConstantRangeCount
			vk::pPushConstantRanges)
		       p-ci
		       (:struct VkPipelineLayoutCreateInfo))
    (setf vk::setLayoutCount set-layout-count
	  vk::pSetLayouts p-set-layouts
	  vk::pushConstantRangeCount push-constant-range-count
	  vk::pPushConstantRanges p-push-constant-ranges))
  (values))

(defmacro with-pipeline-layout-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineLayoutCreateInfo)
     ,@body))

(defun fill-pipeline-dynamic-state-create-info (p-ci
						&key (dynamic-state-count 1)
						  p-dynamic-states
						  &allow-other-keys)
  (with-foreign-slots ((vk::dynamicStateCount
			vk::pDynamicStates)
		       p-ci
		       (:struct VkPipelineDynamicStateCreateInfo))
    (setf vk::dynamicStateCount dynamic-state-count
	  vk::pDynamicStates p-dynamic-states))
  (values))						       

(defmacro with-pipeline-dynamic-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineDynamicStateCreateInfo)
     ,@body))

(defmacro with-dynamic-states ((var count) &body body)
  `(with-foreign-object (,var 'VkDynamicState ,count)
     ,@body))

(defun fill-pipeline-color-blend-state-create-info (p-ci
						    &key
						      (logic-op-enable VK_FALSE)
						      (logic-op VK_LOGIC_OP_COPY)
						      (attachment-count 1)
						      p-attachments
						      (blend-constant-r 0.0f0)
						      (blend-constant-g 0.0f0)
						      (blend-constant-b 0.0f0)
						      (blend-constant-a 0.0f0)
						      &allow-other-keys)
  (with-foreign-slots ((vk::logicOpEnable
			vk::logicOp
			vk::attachmentCount
			vk::pAttachments)
		       p-ci
		       (:struct VkPipelineColorBlendStateCreateInfo))
    (let ((p-blend-constants
	   (foreign-slot-pointer p-ci '(:struct VkPipelineColorBlendStateCreateInfo) 'vk::blendConstants)))
      (setf vk::logicOpEnable logic-op-enable
	    vk::logicOp logic-op
	    vk::attachmentCount attachment-count ;;back-buffer-count
	    vk::pAttachments p-attachments
	    (mem-aref p-blend-constants :float 0) blend-constant-r
	    (mem-aref p-blend-constants :float 1) blend-constant-g
	    (mem-aref p-blend-constants :float 2) blend-constant-b
	    (mem-aref p-blend-constants :float 3) blend-constant-a)))
  (values))
  
(defmacro with-pipeline-color-blend-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineColorBlendStateCreateInfo)
     ,@body))


(defun fill-pipeline-color-blend-attachment-state (p-attachment
						   &key
						     (color-write-mask (logior VK_COLOR_COMPONENT_R_BIT VK_COLOR_COMPONENT_G_BIT
									       VK_COLOR_COMPONENT_B_BIT VK_COLOR_COMPONENT_A_BIT))
						     (blend-enable VK_FALSE)
						     (src-color-blend-factor VK_BLEND_FACTOR_SRC_ALPHA)
						     (dst-color-blend-factor VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA)
						     (color-blend-op VK_BLEND_OP_ADD)
						     (src-alpha-blend-factor VK_BLEND_FACTOR_ONE)
						     (dst-alpha-blend-factor VK_BLEND_FACTOR_ZERO)
						     (alpha-blend-op VK_BLEND_OP_ADD)
						     &allow-other-keys)
  
  (vk::zero-struct p-attachment '(:struct VkPipelineColorBlendAttachmentState))
  
  (with-foreign-slots ((vk::colorWriteMask
			vk::blendEnable
			vk::srcColorBlendFactor
			vk::dstColorBlendFactor
			vk::colorBlendOp
			vk::srcAlphaBlendFactor
			vk::dstAlphaBlendFactor
			vk::alphaBlendOp)
		       p-attachment
		       (:struct VkPipelineColorBlendAttachmentState))
    (setf vk::colorWriteMask color-write-mask
	  vk::blendEnable blend-enable
	  vk::srcColorBlendFactor src-color-blend-factor
	  vk::dstColorBlendFactor dst-color-blend-factor
	  vk::colorBlendOp color-blend-op
	  vk::srcAlphaBlendFactor src-alpha-blend-factor
	  vk::dstAlphaBlendFactor dst-alpha-blend-factor
	  vk::alphaBlendOp alpha-blend-op))
  (values))

(defclass object-with-foreign-handle ()
  ((handle :initarg :handle :accessor h)))

(defclass vk-instance (object-with-foreign-handle)
  ())

(defclass gpu (object-with-foreign-handle)
  ((graphics-queue-family :accessor graphics-queue-family)))

(defun find-queue-families (gpu)
  ;; get queue:
  (with-foreign-object (p-count :uint32)
    (vkGetPhysicalDeviceQueueFamilyProperties (h gpu) p-count +nullptr+)
    (let ((count (mem-aref p-count :uint32)))
      (with-foreign-object (p-queues '(:struct VkQueueFamilyProperties) count)
	(vkGetPhysicalDeviceQueueFamilyProperties (h gpu) p-count p-queues)
	(loop for i from 0 below count
	   do (when (not (zerop
			  (logand (foreign-slot-value (mem-aptr p-queues '(:struct VkQueueFamilyProperties) i)
						      '(:struct VkQueueFamilyProperties) 'vk::queueFlags)
				  VK_QUEUE_GRAPHICS_BIT)))
		(return i)))))))

(defun check-for-wsi-support (gpu surface)
  ;; Check for WSI support
  (with-foreign-object (p-res 'VkBool32)
    (vkGetPhysicalDeviceSurfaceSupportKHR (h gpu) (graphics-queue-family gpu) (h surface) p-res)
    (when (not (eq (mem-aref p-res 'VkBool32) VK_TRUE))
      (error "No WSI support on physical device 0"))))

(defmethod initialize-instance ((gpu gpu) &rest initargs &key instance (index 0) &allow-other-keys)
  (declare (ignore initargs))
  (assert instance)
  ;; Get GPU
  (with-foreign-object (p-gpu-count :uint32)
    (check-vk-result (vkEnumeratePhysicalDevices (h instance) p-gpu-count +nullptr+))
    (let ((gpu-count (mem-aref p-gpu-count :uint32)))
      (with-foreign-object (p-gpus 'VkPhysicalDevice gpu-count)
	(check-vk-result (vkEnumeratePhysicalDevices (h instance) p-gpu-count p-gpus))
	;; todo:
	;; If a number >1 of GPUs got reported, you should find the best fit GPU for your purpose
	;; e.g. VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU if available, or with the greatest memory available, etc.
	;; for sake of simplicity we'll just take the first one, assuming it has a graphics queue family.
	(setf (h gpu) (mem-aref p-gpus 'VkPhysicalDevice index)))))
  (setf (graphics-queue-family gpu) (find-queue-families gpu))
  gpu)

(defclass pipeline (object-with-foreign-handle)
  ((app :initarg :app :reader app)
   (dsl :initarg :dsl :accessor dsl)  ;; descriptor set layout
   (pipeline-cache :initarg :pipeline-cache :reader pipeline-cache)
   (render-pass :initarg :render-pass :reader render-pass)
   (back-buffer-count :initarg :back-buffer-count :reader back-buffer-count)
   (framebuffer :initarg :framebuffer :reader framebuffer)))

(defun get-surface-format (gpu surface)
  ;; Get Surface Format:
  ;; Per Spec Format and View Format are expected to be the same unless VK_IMAGE_CREATE_MUTABLE_BIT was set at image creation
  ;; Assuming that the default behavior is without setting this bit, there is no need for separate Spawchain image and image view format
  ;; additionally several new color spaces were introduced with Vulkan Spec v1.0.40
  ;; hence we must make sure that a format with the mostly available color space, VK_COLOR_SPACE_SRGB_NONLINEAR_KHR, is found and used
  (with-slots (format) surface
    (setf format (make-instance 'surface-format :handle (foreign-alloc '(:struct VkSurfaceFormatKHR))))
    (with-foreign-object (p-count :uint32)
      (vkGetPhysicalDeviceSurfaceFormatsKHR (h gpu) (h surface) p-count +nullptr+)
      (let ((count (mem-aref p-count :uint32)))
	(with-foreign-object (p-formats '(:struct VkSurfaceFormatKHR) count)
	  (vkGetPhysicalDeviceSurfaceFormatsKHR (h gpu) (h surface) p-count p-formats)
	  ;; first check if only one format, VK_FORMAT_UNDEFINED, is available,
	  ;; which would imply that any format is available
	  (if (eq count 1)
	      (if (eq (foreign-slot-value (mem-aptr p-formats '(:struct VkSurfaceFormatKHR) 0)
					  '(:struct VkSurfaceFormatKHR) 'vk::format)
		      VK_FORMAT_UNDEFINED)
		  
		  (setf (foreign-slot-value (h format) '(:struct VkSurfaceFormatKHR) 'vk::format)
			VK_FORMAT_B8G8R8A8_UNORM
			(foreign-slot-value (h format) '(:struct VkSurfaceFormatKHR) 'vk::colorSpace)
			VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)
		  ;; no point in search for another format
		  (copy-vk-surface-format-khr (mem-aptr p-formats '(:struct VkSurfaceFormatKHR) 0)
					      (h format)))
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
				  
				  (copy-vk-surface-format-khr p-format-j (h format))
				  (return-from find-requested)))))
		  ;; if none of the requested image formats could be found, use the first available
		  (copy-vk-surface-format-khr (mem-aptr p-formats '(:struct VkSurfaceFormatsKHR) 0)
					      (h format))))))))
    format))

(defun get-present-mode (gpu surface)
  ;; Get present mode
  ;; request a certain mode and confirm that it is available.  If not use VK_PRESENT_MODE_FIFO_KHR which is mandatory
  (let ((present-mode 
	 (if *imgui-unlimited-frame-rate*
	     VK_PRESENT_MODE_IMMEDIATE_KHR
	     VK_PRESENT_MODE_FIFO_KHR)))
    ;; this looks screwed up
    (with-foreign-object (p-count :uint32)
      (vkGetPhysicalDeviceSurfacePresentModesKHR (h gpu) (h surface) p-count +nullptr+)
      (let ((count (mem-aref p-count :uint32)))
	(with-foreign-object (p-present-modes 'VkPresentModeKHR count)
	  (vkGetPhysicalDeviceSurfacePresentModesKHR (h gpu) (h surface) p-count p-present-modes)
	  (loop for i from 0 below count
	     when (eq (mem-aref p-present-modes 'VkPresentModeKHR i)
		      present-mode)
	     do (return-from get-present-mode present-mode)))))
    present-mode)) ;; always available




(defclass app ()
  ((id :accessor id)
   (window :accessor window)
   (instance :accessor instance)
   (gpu :accessor gpu)
   (debug-report :accessor debug-report)
   (device :accessor device)
   (allocator :accessor allocator)))	 ;; VkAllocationCallbacks

(defmethod initialize-instance ((app app) &rest initargs
				&key (width 1280) (height 720) (window-title "ImGui Vulkan example") &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (id window allocator instance gpu debug-report device) app
    (setf id (make-pointer (incf *last-app-id*)))
    (setf allocator VK_NULL_HANDLE)
    (setf window (make-instance 'window :app app :title window-title :width width :height height))
    (setf instance (make-instance 'vk-instance :allocator allocator))
    (setf gpu (make-instance 'gpu :instance instance))
    (setf debug-report
	  (when *debug*
	    (make-instance 'debug-report-callback :instance instance :allocator allocator)))
    (setf (surface window) (make-instance 'surface :gpu gpu :instance instance :window window
					  :allocator allocator))
    (setf device (make-instance 'device :gpu gpu :window window :allocator allocator)))
  (push app *apps*))

(defclass vkinstance (object-with-foreign-handle)
  ())

(defmethod initialize-instance ((instance vk-instance) &rest initargs
				&key allocator &allow-other-keys)
  (declare (ignore initargs))
  (assert allocator)
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
		    (setf (h instance) (mem-aref p-instance 'VkInstance)))))))))))
  instance)

(defclass window (object-with-foreign-handle)
  ((app :initarg :app :reader app)
   (surface :accessor surface)))

(defun get-window-size (window)
  (with-foreign-objects ((p-w :int)
			 (p-h :int))
    (glfwGetWindowSize (h window) p-w p-h)
    (values (mem-aref p-w :int) (mem-aref p-h :int))))

(defclass debug-report-callback (object-with-foreign-handle)
  ((callback-name :initarg :callback-name
		  :initform 'debug-report-callback
		  :accessor callback-name)))

(defmethod initialize-instance ((callback debug-report-callback) &rest initargs
				&key instance allocator
				  (flags (logior VK_DEBUG_REPORT_ERROR_BIT_EXT
						 VK_DEBUG_REPORT_WARNING_BIT_EXT
						 VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT))
				  &allow-other-keys)
  (declare (ignore initargs))
  (assert instance)
  (assert allocator)
  
  ;; create the debug report callback
  (with-vk-struct (p-debug-report-create-info VkDebugReportCallbackCreateInfoEXT)
    (with-foreign-slots ((vk::flags vk::pfnCallback vk::pUserData)
			 p-debug-report-create-info (:struct VkDebugReportCallbackCreateInfoEXT))
      (setf vk::flags flags
	    vk::pfnCallback (get-callback 'debug-report-callback)
	    vk::pUserData +nullptr+)
      (with-foreign-object (p-debug-report 'VkDebugReportCallbackEXT)
	(check-vk-result (vkCreateDebugReportCallbackEXT (h instance) (h instance)
							 p-debug-report-create-info
							 allocator p-debug-report))
	(setf (h callback) (mem-aref p-debug-report 'VkDebugReportCallbackEXT)))))
  callback)
   

(defmethod initialize-instance ((window window) &rest initargs &key app title width height &allow-other-keys)
  (declare (ignore initargs))
  (assert app)
  (assert (typep width 'integer))
  (assert (typep height 'integer))
  
  (glfwSetErrorCallback (get-callback 'error-callback))

  (when (zerop (glfwInit))
    (error "GLFW failed to initialize."))
    
  (glfwWindowHint GLFW_CLIENT_API GLFW_NO_API)
  (setf (h window) (glfwCreateWindow width height title +nullptr+ +nullptr+))

  (glfwSetWindowUserPointer (h window) (id app))

  (when (zerop (glfwVulkanSupported))
    (error "GLFW: Vulkan Not Supported."))

  window)
  
(defclass surface (object-with-foreign-handle)
  ((format :accessor surface-format)
   (present-mode :accessor present-mode)))

(defclass surface-format (object-with-foreign-handle)
  ())
  

(defmethod initialize-instance ((surface surface) &rest initargs &key gpu instance window allocator &allow-other-keys)
  (declare (ignore initargs))
  (assert gpu)
  (assert instance)
  (assert window)
  (assert allocator)
  (with-foreign-object (p-surface 'VkSurfaceKHR)
    (check-vk-result (vkappui::glfwCreateWindowSurface (h instance) (h window) allocator p-surface))
    (setf (h surface) (mem-aref p-surface 'VkSurfaceKHR)))
  (check-for-wsi-support gpu surface)
  (setf (surface-format surface) (get-surface-format gpu surface))
  (setf (present-mode surface) (get-present-mode gpu surface))
  surface)

(defclass device (object-with-foreign-handle)
  ((queue :accessor queue)
   (swapchain :accessor swapchain)))

(defclass queue (object-with-foreign-handle)
  ())

(defmethod initialize-instance ((device device) &rest initargs
				&key gpu window allocator (device-extensions (list "VK_KHR_swapchain"))
				  (enable-geometry-shaders t))
  (declare (ignore initargs))
  (assert gpu)
  (assert allocator)
  ;; Create Logical Device:
  (let ((device-extension-count (length device-extensions)))
    (with-foreign-object (p-device-extensions :pointer device-extension-count)
      (unwind-protect

	   (loop for i from 0 for extension in device-extensions
	      do (setf (mem-aref p-device-extensions :pointer i) (foreign-string-alloc extension)))
	   (let ((queue-index 0)
		 (queue-count 1))
	     (with-foreign-object (p-queue-priority :float)
	       (setf (mem-aref p-queue-priority :float) 1.0f0)
	       (with-vk-struct (p-queue-info VkDeviceQueueCreateInfo)
		 (with-foreign-slots ((vk::queueFamilyIndex vk::queueCount vk::pQueuePriorities)
				      p-queue-info (:struct VkDeviceQueueCreateInfo))
		   (setf vk::queueFamilyIndex (graphics-queue-family gpu)
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
		       (check-vk-result (vkCreateDevice (h gpu) p-create-info allocator p-device))
		       (setf (h device) (mem-aref p-device 'VkDevice))
		       (setf (queue device) (make-instance 'queue :device device :gpu gpu :queue-index queue-index))
		       (setf (swapchain device) (make-instance 'swapchain :device device :gpu gpu :window window :allocator allocator))
		       device))))))
	   (loop for i from 0 below device-extension-count
	      do (foreign-string-free (mem-aref p-device-extensions :pointer i)))))))

(defmethod initialize-instance ((queue queue) &rest initargs &key device gpu queue-index &allow-other-keys)
  (declare (ignore initargs))
  (assert device)
  (assert gpu)
  (assert queue-index)
  (with-foreign-object (p-queue 'VkQueue)
    (vkGetDeviceQueue (h device) (graphics-queue-family gpu) queue-index p-queue)
    (setf (h queue) (mem-aref p-queue 'VkQueue))
    queue))

(defclass owner-device-mixin ()
  ((device :accessor device)))

(defclass swapchain (object-with-foreign-handle owner-device-mixin)
  ((images :accessor images)
   (image-views :accessor image-views)
   (image-range :accessor image-range)))

(defclass owner-swapchain-mixin ()
  ((swapchain :accessor swapchain)))

(defclass foreign-array-mixin ()
  ((data :accessor array-data)
   (count :accessor array-count)))

(defmethod get-aptr ((array foreign-array-mixin) (index integer))
  (mem-aref (array-data array) (array-foreign-type array) index))

(defclass image-array (foreign-array-mixin owner-device-mixin owner-swapchain-mixin)
  ())

(defmethod array-foreign-type ((images image-array))
  'VkImage)

(defclass image-view-array (foreign-array-mixin owner-device-mixin)
  ())

(defmethod array-foreign-type ((views image-view-array))
  'VkImageViews)



(defmethod initialize-instance ((swapchain swapchain) &rest initargs &key device gpu window allocator old-swapchain &allow-other-keys)
  (declare (ignore initargs))
  (assert device)
  (assert gpu)
  (assert window)
  (assert allocator)

  (let ((fb-width)
	(fb-height))
    (multiple-value-bind (width height) (get-window-size window)
      (with-slots (surface) window
	(with-slots (format present-mode) surface
	  (with-vk-struct (p-info VkSwapchainCreateInfoKHR)
	    (with-foreign-slots ((vk::surface vk::imageFormat vk::imageColorSpace vk::imageArrayLayers
					      vk::imageUsage vk::imageSharingMode vk::preTransform
					      vk::compositeAlpha vk::presentMode vk::clipped vk::oldSwapchain
					      vk::minImageCount vk::imageExtent)
				 p-info (:struct VkSwapchainCreateInfoKHR))
	      (setf vk::surface (h surface)
		    vk::imageFormat (foreign-slot-value (h format) '(:struct VkSurfaceFormatKHR) 'vk::format)
		    vk::imageColorSpace (foreign-slot-value (h format) '(:struct VkSurfaceFormatKHR) 'vk::colorSpace)
		    vk::imageArrayLayers 1
		    vk::imageUsage (logior vk::imageUsage VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
		    vk::imageSharingMode VK_SHARING_MODE_EXCLUSIVE
		    vk::preTransform VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
		    vk::compositeAlpha VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
		    vk::presentMode present-mode
		    vk::clipped VK_TRUE
		    vk::oldSwapchain (unless old-swapchain +nullptr+))
	      (with-vk-struct (p-cap VkSurfaceCapabilitiesKHR)
		(check-vk-result (vkGetPhysicalDeviceSurfaceCapabilitiesKHR (h gpu) (h surface) p-cap))
		  
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
			  
			(setf fb-width width
			      fb-height height)
		  
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
		      (check-vk-result (vkCreateSwapchainKHR (h device) p-info allocator p-swapchain))
		      (setf (h swapchain) (mem-aref p-swapchain 'VkSwapchainKHR))))))))
	  (setf (images swapchain) (make-instance 'image-array :device device :swapchain swapchain))
	  (setf (image-range swapchain) (make-instance 'image-range))
	  (setf (image-views swapchain) (make-instance 'image-view-array
						       :device device :swapchain swapchain
						       :allocator allocator :format format))
	  (destroy-old-swapchain old-swapchain :device device :allocator allocator)
	  swapchain)))))

(defun destroy-old-swapchain (old-swapchain &key device allocator)
  (assert device)
  (assert allocator)

  (when old-swapchain
    (unless (null-pointer-p (h old-swapchain))
      (vkDestroySwapchainKHR (h device) (h old-swapchain) allocator)))
  (values))

(defmethod initialize-instance ((images image-array) &rest initargs &key device swapchain &allow-other-keys)
  (declare (ignore initargs))
  (assert device)
  (assert swapchain)

  (with-foreign-objects ((p-back-buffer-count :uint32)
			 (p-back-buffer 'VkImage 4))
    (check-vk-result (vkGetSwapchainImagesKHR (h device) (h swapchain) p-back-buffer-count +nullptr+))
    (let ((count (mem-aref p-back-buffer-count :uint32)))
      (check-vk-result (vkGetSwapchainImagesKHR (h device) (h swapchain) p-back-buffer-count p-back-buffer))
      (setf (array-data images) p-back-buffer
	    (array-count images) count
	    (device images) device
	    (swapchain images) swapchain)))
  images)

(defmethod vk-destroy ((images image-array))
  (with-slots (device swapchain) images
    (with-slots (allocator) device
      (loop for i from 0 below (array-count images)
	 do (vkDestroyImage (h device) (get-aptr images i) allocator))))
  (values))

(defclass image-range (object-with-foreign-handle)
  ())

(defmethod initialize-instance ((image-range image-range) &rest initargs
				&key (aspect-mask VK_IMAGE_ASPECT_COLOR_BIT)
				  (base-mip-level 0)
				  (level-count 1)
				  (base-array-layer 0)
				  (layer-count 1)
				  &allow-other-keys)
  (declare (ignore initargs))
  
  (setf (h image-range) (foreign-alloc '(:struct VkImageSubresourceRange)))
  
  (with-foreign-slots ((vk::aspectMask
			vk::baseMipLevel
			vk::levelCount
			vk::baseArrayLayer
			vk::layerCount)
		       (h image-range)
		       (:struct VkImageSubresourceRange))

    (setf vk::aspectMask aspect-mask
	  vk::baseMipLevel base-mip-level
	  vk::levelCount level-count
	  vk::baseArrayLayer base-array-layer
	  vk::layerCount layer-count))
  image-range)

(defmethod vk-destroy ((image-range image-range))
  (foreign-free image-range)
  (values))

(defmethod initialize-instance ((image-views image-view-array) &rest initargs &key device swapchain allocator format &allow-other-keys)
  (declare (ignore initargs))
  (assert device)
  (assert swapchain)
  (assert allocator)
  (assert format)
  ;; Create the image views:
  (with-slots (images image-range) swapchain
    (setf image-range (make-instance 'image-range))
    
    (with-foreign-object (p-info '(:struct VkImageViewCreateInfo) (array-count images))
      (with-foreign-object (p-image-view 'VkImageView (array-count images))
	(loop for i from 0 below (array-count images)
	   do (zero-struct (mem-aptr p-info '(:struct VkImageViewCreateInfo) i) '(:struct VkImageViewCreateInfo))
	     
	     (with-foreign-slots ((vk::sType vk::viewType vk::format vk::subresourceRange)
				  (mem-aptr p-info '(:struct VkImageViewCreateInfo) i)
				  (:struct VkImageViewCreateInfo))
	       
	       (with-foreign-slots ((vk::r vk::g vk::b vk::a)
				    (foreign-slot-pointer (mem-aptr p-info '(:struct VkImageViewCreateInfo) i) '(:struct VkImageViewCreateInfo)
							  'vk::components)
				    (:struct VkComponentMapping))
		 
		 (setf vk::sType VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
		       vk::viewType VK_IMAGE_VIEW_TYPE_2D
		       vk::format (foreign-slot-value (h format) '(:struct VkSurfaceFormatKHR) 'vk::format)
		       vk::r VK_COMPONENT_SWIZZLE_R
		       vk::g VK_COMPONENT_SWIZZLE_G
		       vk::b VK_COMPONENT_SWIZZLE_B
		       vk::a VK_COMPONENT_SWIZZLE_A
		       vk::subresourceRange (h image-range))))
		      
	     (setf (foreign-slot-value (mem-aptr p-info '(:struct VkImageViewCreateInfo) i) '(:struct VkImageViewCreateInfo) 'vk::image)
		   (get-aptr images i))
	     (check-vk-result (vkCreateImageView (h device) (mem-aptr p-info '(:struct VkImageViewCreateInfo) i) allocator
						 (mem-aptr p-image-view 'VkImageView i))))
	(setf (array-data image-views) p-image-view
	      (array-count image-views) (array-count images)))))
  image-views)

(defmethod vk-destroy ((image-views image-view-array))
  (with-slots (device) image-views
    (with-slots (allocator) device
      (loop for i from 0 below (array-count image-views)
	 do (vkDestroyImageView (h device) (get-aptr image-views i) allocator))))
  (values))

(defclass graphics-pipeline (pipeline)
  ())

(defclass compute-pipeline (pipeline)
  ())

;; 
(defclass selectable-graphics-pipeline (graphics-pipeline)
  ())

;; Does not use model matrix
(defclass annotation-graphics-pipeline (graphics-pipeline)
  ())


(defun create-selection-pipeline-1 (device allocator pipeline-cache render-pass back-buffer-count width height
				    &key
				      (descriptor-set-layouts +nullptr+)
				      (descriptor-set-layout-count 0)
				      (geom-shader-file "C:/Users/awolven/vkapp/shaders/selection.geom.spv")
				      &allow-other-keys)
  (create-pipeline-1 device allocator pipeline-cache render-pass back-buffer-count width height
		     :descriptor-set-layouts descriptor-set-layouts
		     :descriptor-set-layout-count descriptor-set-layout-count
		     :geom-shader-file geom-shader-file))




(defun create-pipeline-1 (device allocator pipeline-cache render-pass back-buffer-count width height
			  &rest args &key
				       (descriptor-set-layouts +nullptr+)
				       (descriptor-set-layout-count 0)
				       (vert-shader-file "C:/Users/awolven/vkapp/shaders/vert.spv")
				       (geom-shader-file nil)
				       (frag-shader-file "C:/Users/awolven/vkapp/shaders/frag.spv")
				       &allow-other-keys)
  (let ((vert-shader-code)
	(vert-shader-code-size)
	(geom-shader-code)
	(geom-shader-code-size)
	(frag-shader-code)
	(frag-shader-code-size))
			 
    (multiple-value-setq (vert-shader-code vert-shader-code-size)
      (read-shader-file vert-shader-file))
    
    (when geom-shader-file
      (multiple-value-setq (geom-shader-code geom-shader-code-size)
	(read-shader-file geom-shader-file)))
    
    (multiple-value-setq (frag-shader-code frag-shader-code-size)
      (read-shader-file frag-shader-file))
    
    (let ((vert-shader-module (create-shader-module device allocator vert-shader-code vert-shader-code-size))
	  (geom-shader-module (when geom-shader-file
				(create-shader-module device allocator geom-shader-code geom-shader-code-size)))
	  (frag-shader-module (create-shader-module device allocator frag-shader-code frag-shader-code-size)))
      
      (with-foreign-object (p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) (if geom-shader-file 3 2))
	(fill-pipeline-shader-stage-create-info (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 0)
						:stage VK_SHADER_STAGE_VERTEX_BIT
						:module vert-shader-module)
	(fill-pipeline-shader-stage-create-info (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 1)
						:stage VK_SHADER_STAGE_FRAGMENT_BIT
						:module frag-shader-module)
	(when geom-shader-file
	  (fill-pipeline-shader-stage-create-info (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 2)
						  :stage VK_SHADER_STAGE_GEOMETRY_BIT
						  :module geom-shader-module))
	
	(with-vertex-input-binding-description (p-vibd)
	  (apply #'fill-vertex-input-binding-description p-vibd (append args (list :stride (foreign-type-size '(:struct Vertex)))))
	  
	  (with-foreign-object (p-attribute-descriptions '(:struct VkVertexInputAttributeDescription) 2)
	    (fill-vertex-input-attribute-description (mem-aptr p-attribute-descriptions '(:struct VkVertexInputAttributeDescription) 0)
						     :location 0
						     :offset (foreign-slot-offset '(:struct Vertex) 'pos))
	    (fill-vertex-input-attribute-description (mem-aptr p-attribute-descriptions '(:struct VkVertexInputAttributeDescription) 1)
						     :location 1
						     :offset (foreign-slot-offset '(:struct Vertex) 'color))
	    
	    (with-pipeline-vertex-input-state-create-info (p-pvisci)
	      (apply #'fill-pipeline-vertex-input-state-create-info p-pvisci
		     :vertex-binding-description-count 1
		     :p-vertex-binding-descriptions p-vibd
		     :vertex-attribute-description-count 2
		     :p-vertex-attribute-descriptions p-attribute-descriptions
		     args)
	      
	      (with-pipeline-input-assembly-state-create-info (p-piasci)
		(apply #'fill-pipeline-input-assembly-state-create-info p-piasci args)
		
		(with-viewport-structure (p-viewport)
		  (apply #'fill-viewport-structure p-viewport (append args (list :viewport-width (coerce width 'single-float)
										 :viewport-height (coerce height 'single-float))))
		  
		  (with-scissor-structure (p-scissor)
		    (apply #'fill-scissor-structure p-scissor (append args (list :scissor-width width :scissor-height height)))
		    
		    (with-pipeline-viewport-state-create-info (p-viewport-state)
		      (apply #'fill-pipeline-viewport-state-create-info p-viewport-state
			     :viewport-count 1
			     :p-viewports p-viewport
			     :scissor-count 1
			     :p-scissors p-scissor
			     args)
		      
		      (with-pipeline-rasterization-state-create-info (p-rasterizer)
			(apply #'fill-pipeline-rasterization-state-create-info
			       p-rasterizer args)
			
			(with-pipeline-multisample-state-create-info (p-multisampling)
			  (apply #'fill-pipeline-multisample-state-create-info p-multisampling args)
			  
			  (with-foreign-object (p-color-blend-attachments '(:struct VkPipelineColorBlendAttachmentState) back-buffer-count)
			    (loop for i below back-buffer-count
			       do (fill-pipeline-color-blend-attachment-state
				   (mem-aptr p-color-blend-attachments '(:struct VkPipelineColorBlendAttachmentState) i)))
			    
			    (with-pipeline-color-blend-state-create-info  (p-color-blending)
			      (apply #'fill-pipeline-color-blend-state-create-info p-color-blending
				     :p-attachments p-color-blend-attachments
				     :attachment-count 1 ;;back-buffer-count
				     args)
			      
			      (with-dynamic-states (p-dynamic-states 3)
				(setf (mem-aref p-dynamic-states 'VkDynamicState 0) VK_DYNAMIC_STATE_VIEWPORT
				      (mem-aref p-dynamic-states 'VkDynamicState 1) VK_DYNAMIC_STATE_SCISSOR
				      (mem-aref p-dynamic-states 'VKDynamicState 2) VK_DYNAMIC_STATE_LINE_WIDTH)
				
				(with-pipeline-dynamic-state-create-info (p-pipeline-dynamic-state-ci)
				  (apply #'fill-pipeline-dynamic-state-create-info p-pipeline-dynamic-state-ci
					 :dynamic-state-count 3 :p-dynamic-states p-dynamic-states args)
				  
				  ;; Each VkDescriptorSetLayout corresponds to a "set" in the GLSL code.
				  ;; descriptor-set-layouts is an array of VkDescriptorSetLayout
				  (with-pipeline-layout-create-info (p-pipeline-layout-ci)
				    (apply #'fill-pipeline-layout-create-info p-pipeline-layout-ci
					   :set-layout-count descriptor-set-layout-count :p-set-layouts descriptor-set-layouts
					   args)
				      
				    (with-foreign-object (p-pipeline-layout 'VKPipelineLayout)
				      (check-vk-result
				       (vkCreatePipelineLayout device p-pipeline-layout-ci allocator p-pipeline-layout))
					
				      (let ((pipeline-layout (mem-aref p-pipeline-layout 'VkPipelineLayout)))
					(with-pipeline-depth-stencil-state-create-info (p-depth-stencil)
					  (apply #'fill-pipeline-depth-stencil-state-create-info p-depth-stencil args)
					  (with-graphics-pipeline-create-info (p-pipeline-ci)
					    (apply #'fill-graphics-pipeline-create-info p-pipeline-ci
						   :stage-count (if geom-shader-module 3 2)
						   :p-stages p-shader-stages
						   :p-vertex-input-state p-pvisci
						   :p-input-assembly-state p-piasci
						   :p-viewport-state p-viewport-state
						   :p-rasterization-state p-rasterizer
						   :p-multisample-state p-multisampling
						   :p-depth-stencil-state p-depth-stencil
						   :p-color-blend-state p-color-blending
						   :p-dynamic-state +nullptr+ ;;p-pipeline-dynamic-state-ci
						   :layout pipeline-layout
						   :render-pass render-pass
						   args)
					      
					    (with-foreign-object (p-graphics-pipeline 'VkPipeline)
					      (check-vk-result
					       (vkCreateGraphicsPipelines device pipeline-cache 1 p-pipeline-ci allocator p-graphics-pipeline))
					      (vkDestroyShaderModule device vert-shader-module allocator)
					      (when geom-shader-module
						(vkDestroyShaderModule device geom-shader-module allocator))
					      (vkDestroyShaderModule device frag-shader-module allocator)
					      (values (mem-aref p-graphics-pipeline 'VkPipeline) pipeline-layout))))))))))))))))))))))))

   
  
  

(defvar *apps* '())

(defun find-app (user-pointer)
  (find user-pointer *apps* :key #'app-id :test #'pointer-eq))





