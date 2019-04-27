(in-package :vktk)

(defun run-demo (&optional (debug t))
  (let* ((*debug* debug)
	 (app (make-instance 'application)))
    #+windows
    (sb-thread:make-thread
     (lambda ()
       (main app)))
    #+darwin
    (sb-thread:interrupt-thread
     (sb-thread:main-thread)
     (lambda ()
       (sp-int:with-float-traps-masked
	   (:invalid :inexact :overflow)
	 (main app))))
    app))

(defclass vktk-module ()
  ((allocator :accessor allocator)
   (device :accessor device)
   (render-pass :accessor render-pass)
   (pipeline-cache :accessor pipeline-cache)
   (descriptor-pool :accessor descriptor-pool)))

(defclass 3d-demo-module (vktk-module)
  ((window :accessor window)
   (descriptor-set-layout :accessor descriptor-set-layout)
   (descriptor-set :accessor descriptor-set)
   (pipeline-layout :accessor pipeline-layout)
   (pipeline :accessor pipeline)		   
   
   (camera :initform (make-instance 'camera) :reader camera)
   (start-time :reader start-time :initform (get-internal-real-time))
   (vertex-shader :accessor vertex-shader)
   (geometry-shader :accessor geometry-shader)
   (fragment-shader :accessor fragment-shader)

   (uniform-buffer-vs :accessor uniform-buffer-vs)
   (uniform-buffer-gs :accessor uniform-buffer-gs)

   (vertex-buffer :accessor vertex-buffer :initform nil)
   (index-buffer :accessor index-buffer :initform nil)))

(defclass camera ()
  ((ortho-p :initform t :accessor ortho-p)
   (zoom :initform 1.0f0 :accessor zoom)
   (x-loc :initform 1.0f0 :accessor camera-x)
   (y-loc :initform 1.0f0 :accessor camera-y)
   (z-loc :initform 1.0f0 :accessor camera-z)
   (eye-x :initform 0.0f0 :accessor eye-x)
   (eye-y :initform 0.0f0 :accessor eye-y)
   (eye-z :initform 0.0f0 :accessor eye-z)))

(defun 3d-demo-init (module &key
			   device
			   (allocator +null-allocator+)
			   window
			   render-pass
			   pipeline-cache
			   descriptor-pool)
  
  (setf (allocator module) allocator
	(device module) device
	(render-pass module) render-pass
	(pipeline-cache module) pipeline-cache
	(descriptor-pool module) descriptor-pool
	(window module) window)

  (3d-demo-create-device-objects module)
  
  (values))
			   
(defcstruct vec2
  (a :float)
  (b :float))

(defcstruct vec3
  (a :float)
  (b :float)
  (c :float))

(defcstruct 3DVertex
  (position (:struct vec3))
  (color (:struct vec3)))

(defun 3d-demo-create-device-objects (module)
  (with-slots (device) module
    (multiple-value-bind (width height) (get-framebuffer-size (window module))
      (let ((vtx-shader (create-shader-module-from-file device (concatenate 'string *assets-dir* "shaders/vert.spv")))
	    (frg-shader (create-shader-module-from-file device (concatenate 'string *assets-dir* "shaders/frag.spv")))
	    (geo-shader (when (has-geometry-shader-p (physical-device device))
			  (create-shader-module-from-file device (concatenate 'string *assets-dir* "shaders/selection.geom.spv")))))
	(setf (descriptor-set-layout module) (create-descriptor-set-layout device
									   :bindings (list* (make-instance 'uniform-buffer-for-vertex-shader-dsl-binding)
											    (when (has-geometry-shader-p (physical-device device))
											      (list (make-instance 'uniform-buffer-for-geometry-shader-dsl-binding)))))
	      (pipeline-layout module) (create-pipeline-layout device (list (descriptor-set-layout module)))
	      (pipeline module) (create-graphics-pipeline (device module) (pipeline-cache module) (pipeline-layout module)
							  (render-pass module) 1 width height vtx-shader frg-shader
							  :geometry-shader-module geo-shader
							  :vertex-type '(:struct 3DVertex)
							  :vertex-input-attribute-descriptions
							  (list (make-instance 'vertex-input-attribute-description
									       :location 0
									       :format VK_FORMAT_R32G32B32_SFLOAT
									       :offset (foreign-slot-offset '(:struct 3DVertex) 'position))
								(make-instance 'vertex-input-attribute-description
									       :location 1
									       :format VK_FORMAT_R32G32B32_SFLOAT
									       :offset (foreign-slot-offset '(:struct 3DVertex) 'color)))
							  :min-sample-shading 1.0f0
							  :depth-test-enable VK_TRUE
							  :depth-write-enable VK_TRUE
							  :depth-compare-op VK_COMPARE_OP_LESS
							  :logic-op VK_LOGIC_OP_COPY
							  :blend-enable VK_FALSE
							  :depth-clamp-enable VK_FALSE
							  :src-alpha-blend-factor VK_BLEND_FACTOR_ONE))
	(setf (vertex-shader module) vtx-shader
	      (geometry-shader module) geo-shader
	      (fragment-shader module) frg-shader)

	(setf (uniform-buffer-vs module) (create-uniform-buffer device (foreign-type-size '(:struct 3DDemoVSUBO))))
	(setf (uniform-buffer-gs module) (create-uniform-buffer device (foreign-type-size '(:struct 3DDemoGSUBO))))
	(setf (descriptor-set module) (create-descriptor-set device (list (descriptor-set-layout module))
							     
							     (descriptor-pool module)
							     :descriptor-buffer-info
							     (list (make-instance 'uniform-descriptor-buffer-info
										  :uniform-buffer (uniform-buffer-vs module)
										  :range (foreign-type-size '(:struct 3DDemoVSUBO)))
								   (make-instance 'uniform-descriptor-buffer-info
										  :uniform-buffer (uniform-buffer-gs module)
										  :range (foreign-type-size '(:struct 3DDemoGSUBO))))))
	(values)))))

(defun shutdown-3d-demo (module)
  (let* ((window (window module))
	 (swapchain (swapchain window)))

    (when (vertex-shader module)
      (destroy-shader-module (vertex-shader module))
      (setf (vertex-shader module) nil))

    (when (fragment-shader module)
      (destroy-shader-module (fragment-shader module))
      (setf (fragment-shader module) nil))

    ;; fixme: geometry shader unbound
    (when (geometry-shader module)
      (destroy-shader-module (geometry-shader module))
      (setf (geometry-shader module) nil))

    (when (depth-image-view swapchain)
      (destroy-image-view (depth-image-view swapchain))
      (setf (depth-image-view swapchain) nil))
    
    (when (depth-image swapchain)
      (destroy-image (depth-image swapchain))
      (setf (depth-image swapchain) nil))
    
    (loop for image-view across (color-image-views swapchain)
       do (destroy-image-view image-view)
       finally (setf (color-image-views swapchain) nil))
    
    (loop for framebuffer across (framebuffers swapchain)
       do (destroy-framebuffer framebuffer)
       finally (setf (framebuffers swapchain) nil))
    
    (when (pipeline module)
      (destroy-pipeline (pipeline module))
      (setf (pipeline module) nil))
    
    (when (pipeline-layout module)
      (destroy-pipeline-layout (pipeline-layout module))
      (setf (pipeline-layout module) nil))

    #+NIL
    (when (descriptor-set module)
      (free-descriptor-sets (list (descriptor-set module)) (descriptor-pool module))
      (setf (descriptor-set module) nil))
    
    (when (descriptor-set-layout module)
      (destroy-descriptor-set-layout (descriptor-set-layout module))
      (setf (descriptor-set-layout module) nil))
    
    (when (vertex-buffer module)
      (destroy-buffer (vertex-buffer module))
      (setf (vertex-buffer module) nil))
    
    (when (index-buffer module)
      (destroy-buffer (index-buffer module))
      (setf (index-buffer module) nil))

    (when (uniform-buffer-gs module)
      (destroy-buffer (uniform-buffer-gs module))
      (setf (uniform-buffer-gs module) nil))

    (when (uniform-buffer-vs module)
      (destroy-buffer (uniform-buffer-vs module))
      (setf (uniform-buffer-gs module) nil))

    (values)))

(defparameter *vertices-list*
  (list -0.5f0 -0.5f0 0.0f0 1.0f0 0.0f0 0.0f0
	 0.5f0 -0.5f0 0.0f0 0.0f0 1.0f0 0.0f0
	 0.5f0  0.5f0 0.0f0 0.0f0 0.0f0 1.0f0
	 -0.5f0  0.5f0 0.0f0 1.0f0 1.0f0 1.0f0

	 -0.5f0 -0.5f0 -0.5f0 1.0f0 0.0f0 0.0f0
	 0.5f0 -0.5f0 -0.5f0 0.0f0 1.0f0 0.0f0
	 0.5f0  0.5f0 -0.5f0 0.0f0 0.0f0 1.0f0
	 -0.5f0  0.5f0 -0.5f0 1.0f0 1.0f0 1.0f0))

(defparameter *vertex-data* (foreign-alloc :float :initial-contents *vertices-list*))
(defparameter *vertex-data-size* (* (foreign-type-size :float) (length *vertices-list*)))

(defparameter *indices*
  (list 0 1 2 2 3 0
	4 5 6 6 7 4))

(defparameter *index-data* (foreign-alloc :unsigned-short :initial-contents *indices*))
(defparameter *index-data-size* (* (foreign-type-size :unsigned-short) (length *indices*)))

(defcstruct vec4
  (x :float)
  (y :float)
  (z :float)
  (w :float))

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

(defcstruct 3DDemoVSUBO
  (model (:struct mat4))
  (view (:struct mat4)))

(defcstruct 3DDemoGSUBO
  (proj (:struct mat4))
  (pickOrigin (:struct vec4))
  (pickingDir (:struct vec4)))	

(defun look-at2 (eye target up-dir)
	   
  (let* ((forward (vunit (v- eye target)))
	 (left (vunit (vc up-dir forward)))
	 (up (vunit (vc forward left)))
	 (view-matrix (mat4)))

    (setf (mcref view-matrix 0 0) (vx3 left)
	  (mcref view-matrix 0 1) (vy3 left)
	  (mcref view-matrix 0 2) (vz3 left)
	  (mcref view-matrix 0 3) (- (v. left eye))
	  (mcref view-matrix 1 0) (vx3 up)
	  (mcref view-matrix 1 1) (vy3 up)
	  (mcref view-matrix 1 2) (vz3 up)
	  (mcref view-matrix 1 3) (- (v. up eye))
	  (mcref view-matrix 2 0) (vx3 forward)
	  (mcref view-matrix 2 1) (vy3 forward)
	  (mcref view-matrix 2 2) (vz3 forward)
	  (mcref view-matrix 2 3) (- (v. forward eye))
	  (mcref view-matrix 3 0) 0.0d0
	  (mcref view-matrix 3 1) 0.0d0
	  (mcref view-matrix 3 2) 0.0d0
	  (mcref view-matrix 3 3) 1.0d0)
    view-matrix))

(defun compute-picking-ray (window camera projection-matrix view-matrix)
  (multiple-value-bind (viewport-width viewport-height) (get-framebuffer-size window)
    (let ((unprojmat (minv projection-matrix)))
      (multiple-value-bind (mouse-x mouse-y) (get-cursor-pos window)
	(let* ((ray-nds (vec3 (- (/ (* 2.0f0 mouse-x) viewport-width) 1.0f0) (- (/ (* 2.0f0 mouse-y) viewport-height) 1.0f0) 1.0f0))
	       (ray-clip1 (vec4 (vx ray-nds) (vy ray-nds) 0.0f0 1.0f0))
	       (ray-clip2 (vec4 (vx ray-nds) (vy ray-nds) 1.0f0 1.0f0)))
	  (let ((ray-eye1 (m* unprojmat ray-clip1))
		(ray-eye2 (m* unprojmat ray-clip2))
		(ray-world)
		(inv-view-matrix (minv view-matrix)))
	    (setq ray-eye1 (m* inv-view-matrix ray-eye1))
	    (setq ray-eye2 (m* inv-view-matrix ray-eye2))

	    (setq ray-eye1 (vec3 (/ (vx ray-eye1) (vw ray-eye1)) (/ (vy ray-eye1) (vw ray-eye1)) (/ (vz ray-eye1) (vw ray-eye1))))
	    (setq ray-eye2 (vec3 (/ (vx ray-eye2) (vw ray-eye2)) (/ (vy ray-eye2) (vw ray-eye2)) (/ (vz ray-eye2) (vw ray-eye2))))
	    #+NIL(igText "ray-eye1") #+NIL (igText (format nil "~S" ray-eye1))
	    #+NIL(igText "ray-eye2") #+NIL (igText (format nil "~S" ray-eye2))
	    (setq ray-world (vunit (v- ray-eye1 ray-eye2)))

	    (if (not (ortho-p camera))
		(values (vec3 (camera-x camera) (camera-y camera) (camera-z camera)) #+NIL ray-world ray-eye2)
		(values ray-eye1 (v- (vec3 (camera-x camera) (camera-y camera) (camera-z camera)))))))))))

(defun copy-uniform-buffer-memory (device data uniform-buffer-memory size)
  (with-foreign-object (pp-data :pointer)
    (vkMapMemory (h device) (h uniform-buffer-memory) 0 size 0 pp-data)
    (memcpy (mem-aref pp-data :pointer) data size)
    (vkUnmapMemory (h device) (h uniform-buffer-memory))))

(defun update-3d-demo-gs-uniform-buffer-object (module view-matrix proj-matrix)
  (with-foreign-object (p-ubo '(:struct 3DDemoGSUBO))
    (let ((p-p (foreign-slot-pointer p-ubo '(:struct 3DDemoGSUBO) 'proj))
	  (p-po (foreign-slot-pointer p-ubo '(:struct 3DDemoGSUBO) 'pickOrigin))
	  (p-pd (foreign-slot-pointer p-ubo '(:struct 3DDemoGSUBO) 'pickingDir)))
      
      (multiple-value-bind (pick-origin picking-direction)
	  (compute-picking-ray (window module) (camera module) proj-matrix view-matrix)
	
	(setf (mem-aref p-po :float 0) (coerce (vx pick-origin) 'single-float)
	      (mem-aref p-po :float 1) (coerce (vy pick-origin) 'single-float)
	      (mem-aref p-po :float 2) (coerce (vz pick-origin) 'single-float)
	      (mem-aref p-po :float 3) 1.0f0)
	  
	(setf (mem-aref p-pd :float 0) (coerce (vx picking-direction) 'single-float)
	      (mem-aref p-pd :float 1) (coerce (vy picking-direction) 'single-float)
	      (mem-aref p-pd :float 2) (coerce (vz picking-direction) 'single-float)
	      (mem-aref p-pd :float 3) 0.0f0))
      
	(let ((pv-mat (m* proj-matrix view-matrix)))
      (loop for i from 0 below 4
	 do (loop for j from 0 below 4
	       do (setf (mem-aref p-p :float (+ i (* j 4))) (coerce (mcref pv-mat i j) 'single-float))))))
    
    (copy-uniform-buffer-memory (device module)
				p-ubo (allocated-memory (uniform-buffer-gs module))
				(foreign-type-size '(:struct 3DDemoGSUBO))))
  
  nil)

(defun update-3d-demo-vs-uniform-buffer-object (module)
  (with-slots (device camera start-time window) module
    (let* ((current-time (get-internal-real-time))
	   (elapsed-time (- current-time start-time))
	   (model-matrix
	    (m* (mrotation (vec3 0.0 0.0 1.0)
			   (rem (* (/ elapsed-time 1000.0d0) #.(/ pi 4.0d0)) #.(* pi 2.0d0)))
		(mtranslation (vec3 1.0 0.0 0.0))))
	   (view-matrix
	    (look-at2 (vec3 (camera-x camera) (camera-y camera) (camera-z camera))
		      (vec3 0.0 0.0 0.0)
		      (vec3 0.0 0.0 1.0))))
      (with-foreign-object (p-ubo '(:struct 3DDemoVSUBO))
	(let ((p-m (foreign-slot-pointer p-ubo '(:struct 3DDemoVSUBO) 'model))
	      (p-v (foreign-slot-pointer p-ubo '(:struct 3DDemoVSUBO) 'view)))

	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-m :float (+ i (* j 4))) (coerce (mcref model-matrix i j) 'single-float))))

	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-v :float (+ i (* j 4))) (coerce (mcref view-matrix i j) 'single-float))))
	  
	  (copy-uniform-buffer-memory (device module)
				      p-ubo (allocated-memory (uniform-buffer-vs module))
				      (foreign-type-size '(:struct 3DDemoVSUBO)))))
      view-matrix)))

(defun convert-projection-matrix-to-vulkan (src)
  (let ((dest (mcopy4 src)))
    (setf (mcref dest 1 1) (- (mcref dest 1 1))
	  (mcref dest 2 0) (/ (+ (mcref dest 2 0) (mcref dest 3 0)) 2)
	  (mcref dest 2 1) (/ (+ (mcref dest 2 1) (mcref dest 3 1)) 2)
	  (mcref dest 2 2) (/ (+ (mcref dest 2 2) (mcref dest 3 2)) 2)
	  (mcref dest 2 3) (/ (+ (mcref dest 2 3) (mcref dest 3 3)) 2))
    dest))

(defun perspective-2 (width height)
  (let* ((m (mat4))
	 (f (coerce (/ 1.0d0 (tan (/ pi 4))) 'single-float)))
    (setf (mcref m 0 0) (/ f (/ width height))
	  (mcref m 0 1) 0.0f0
	  (mcref m 0 2) 0.0f0
	  (mcref m 0 3) 0.0f0

	  (mcref m 1 0) 0.0f0
	  (mcref m 1 1) (- f)
	  (mcref m 1 2) 0.0f0
	  (mcref m 1 3) 0.0f0

	  (mcref m 2 0) 0.0f0
	  (mcref m 2 1) 0.0f0
	  (mcref m 2 2) (/ -5 (- 5 -5.0f0))
	  (mcref m 2 3) -1.0f0

	  (mcref m 3 0) 0.0f0
	  (mcref m 3 1) 0.0f0
	  (mcref m 3 2) (/ (* 5 -5.0f0) (- 5 -5.0f0))
	  (mcref m 3 3) 0.0f0)
    m))

(defun 3d-demo-render (module command-buffer fb-width fb-height)
  (with-slots (device) module
    (unless (vertex-buffer module)
      ;;(setf (vertex-buffer module) nil)
      (setf (vertex-buffer module) (create-vertex-buffer device *vertex-data* *vertex-data-size*)))

    (unless (index-buffer module)
      #+NIL
      (vkDestroyBuffer (h device) (h (index-buffer module)) (h (allocator module)))
      #+NIL
      (vkFreeMemory (h device) (h (allocated-memory (index-buffer module))) (h (allocator module)))
      ;;(setf (index-buffer module) nil)
      (setf (index-buffer module) (create-index-buffer device *index-data* *index-data-size*)))

    (let ((projection-matrix
	   (if (ortho-p (camera module))
	       (mortho (* (- 2) (zoom (camera module)) (/ fb-width fb-height))
		       (*    2  (zoom (camera module)) (/ fb-width fb-height))
		       (* (- 2) (zoom (camera module)))
		       (*    2  (zoom (camera module)))
		       -10 10)
	       (mperspective (* 75 (zoom (camera module))) (/ fb-width fb-height) 0.001 10000))))

      ;;(when (ortho-p (camera module))
      (setq projection-matrix (convert-projection-matrix-to-vulkan projection-matrix));;)

      (vkCmdBindPipeline (h command-buffer) VK_PIPELINE_BIND_POINT_GRAPHICS (h (pipeline module)))

      (let ((view-matrix (update-3d-demo-vs-uniform-buffer-object module)))
	(update-3d-demo-gs-uniform-buffer-object module
						 view-matrix
						 projection-matrix))

      (with-foreign-objects ((p-vertex-buffers 'VkBuffer)
			     (p-offsets 'VkDeviceSize)
			     (p-descriptor-sets 'VkDescriptorSet))
	(setf (mem-aref p-vertex-buffers 'VkBuffer) (h (vertex-buffer module))
	      (mem-aref p-offsets 'VkDeviceSize) 0
	      (mem-aref p-descriptor-sets 'VkDescriptorSet) (h (descriptor-set module)))

	(vkCmdBindVertexBuffers (h command-buffer) 0 1 p-vertex-buffers p-offsets)

	(vkCmdBindIndexBuffer (h command-buffer) (h (index-buffer module)) 0 VK_INDEX_TYPE_UINT16)

	(vkCmdBindDescriptorSets (h command-buffer) VK_PIPELINE_BIND_POINT_GRAPHICS (h (pipeline-layout module))
				 0 1 p-descriptor-sets 0 +nullptr+))

      (with-vk-struct (p-viewport VkViewport)
	(with-foreign-slots ((vk::x
			      vk::y
			      vk::width
			      vk::height
			      vk::minDepth
			      vk::maxDepth)
			     p-viewport
			     (:struct VkViewport))
	  (setf vk::x 0.0f0
		vk::y 0.0f0
		vk::width (coerce fb-width 'single-float)
		vk::height (coerce fb-height 'single-float)
		vk::minDepth 0.0f0
		vk::maxDepth 1.0f0)
	  (vkCmdSetViewport (h command-buffer) 0 1 p-viewport)))

      (with-vk-struct (p-scissor VkRect2D)
	(let ((p-offset
	       (foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::offset))
	      (p-extent
	       (foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::extent)))
			       
	  (with-foreign-slots ((vk::x vk::y) p-offset (:struct VkOffset2D))
	    (with-foreign-slots ((vk::width vk::height) p-extent (:struct VkExtent2D))
				   
	      (setf vk::x 0
		    vk::y 0
		    vk::width fb-width
		    vk::height fb-height)
				   
	      (vkCmdSetScissor (h command-buffer) 0 1 p-scissor)))))

      (vkCmdDrawIndexed (h command-buffer) (length *indices*) 1 0 0 0)

      (values))))

      
		    
		    
