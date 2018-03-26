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
  
   (fb-width)
   (fb-height)
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
    :initform (make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; VkBuffer
   (index-buffer
    :initform (make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; VkBuffer
   (vertex-buffer-memory
    :initform (make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; VkDeviceMemory
   (index-buffer-memory
    :initform (make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; VkDeviceMemory
   (vertex-data) ;; (:pointer :float)
   (index-data) ;; (:pointer :uint32)
   (vertex-buffer-size
    :initform (make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; size-t
   (index-buffer-size
    :initform (make-array IMGUI_VK_QUEUED_FRAMES :initial-element nil)) ;; size-t

   (upload-buffer-memory) ;; VkDeviceMemory
   (upload-buffer) ;; VkBuffer

   (font-sampler) ;; VkSampler
   (font-memory) ;; VkDeviceMemory
   (font-image) ;; VkImage
   (font-view) ;; VkImageView

   (pipeline-layout)
   (graphics-pipeline)))

(defvar *apps* '())

(defun find-app (user-pointer)
  (find user-pointer *apps* :key #'app-id :test #'pointer-eq))
