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

#+ORIG
(defun create-pipeline-1 (device allocator pipeline-cache render-pass back-buffer-count descriptor-set-layouts width height
			  &rest args &key &allow-other-keys)
  (multiple-value-bind (vert-shader-code vert-shader-code-size)
      (read-shader-file "C:/Users/awolven/vkapp/shaders/vert.spv")
    (multiple-value-bind (frag-shader-code frag-shader-code-size)
	(read-shader-file "C:/Users/awolven/vkapp/shaders/frag.spv")
      (let ((vert-shader-module (create-shader-module device allocator vert-shader-code vert-shader-code-size))
	    (frag-shader-module (create-shader-module device allocator frag-shader-code frag-shader-code-size)))
	(with-foreign-object (p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 2)
	  (fill-pipeline-shader-stage-create-info (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 0)
						  :stage VK_SHADER_STAGE_VERTEX_BIT
						  :module vert-shader-module)
	  (fill-pipeline-shader-stage-create-info (mem-aptr p-shader-stages '(:struct VkPipelineShaderStageCreateInfo) 1)
						  :stage VK_SHADER_STAGE_FRAGMENT_BIT
						  :module frag-shader-module)
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
				    (with-descriptor-set-layouts (p-dsl (length descriptor-set-layouts))
				      (loop for dsl in descriptor-set-layouts for i from 0
					 do (setf (mem-aref p-dsl 'VkDescriptorSetLayout i) dsl))
				      (with-pipeline-layout-create-info (p-pipeline-layout-ci)
					(apply #'fill-pipeline-layout-create-info p-pipeline-layout-ci
					       :set-layout-count (length descriptor-set-layouts) :p-set-layouts p-dsl
					       args)
					(with-foreign-object (p-pipeline-layout 'VKPipelineLayout)
					  (check-vk-result
					   (vkCreatePipelineLayout device p-pipeline-layout-ci allocator p-pipeline-layout))
					  (let ((pipeline-layout (mem-aref p-pipeline-layout 'VkPipelineLayout)))
					    (with-pipeline-depth-stencil-state-create-info (p-depth-stencil)
					      (apply #'fill-pipeline-depth-stencil-state-create-info p-depth-stencil args)
					      (with-graphics-pipeline-create-info (p-pipeline-ci)
						(apply #'fill-graphics-pipeline-create-info p-pipeline-ci
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
						  (vkDestroyShaderModule device frag-shader-module allocator)
						  (mem-aref p-graphics-pipeline 'VkPipeline))))))))))))))))))))))))))
					    
  
  

(defvar *apps* '())

(defun find-app (user-pointer)
  (find user-pointer *apps* :key #'app-id :test #'pointer-eq))





