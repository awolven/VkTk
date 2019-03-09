(in-package :vktk)

(defvar *debug* 1)

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
