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

(defun make-mat4 (initial-contents)
  (assert (eq (length initial-contents) 16))
  (foreign-alloc :float :initial-contents initial-contents))

(defcstruct UniformBufferObject
  (model (:struct mat4))
  (view (:struct mat4))
  (proj (:struct mat4)))

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
   (descriptor-set-layout)
  
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

   (uniform-buffer)
   (uniform-buffer-memory)

   (upload-buffer-memory) ;; VkDeviceMemory
   (upload-buffer) ;; VkBuffer

   (font-sampler) ;; VkSampler
   (font-memory) ;; VkDeviceMemory
   (font-image) ;; VkImage
   (font-view) ;; VkImageView

   (pipeline-layout)
   (graphics-pipeline)

   (start-time)
   (current-item :initform 0)))

(defvar *apps* '())

(defun find-app (user-pointer)
  (find user-pointer *apps* :key #'app-id :test #'pointer-eq))





