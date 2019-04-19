(in-package :vktk)

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
   (model-matrix :reader model-matrix)
   (view-matrix :reader view-matrix)
   (projection-matrix :reader projection-matrix)
   (vertex-shader :accessor vertex-shader)
   (fragment-shader :accessor fragment-shader)

   (uniform-buffer-vs :accessor uniform-buffer-vs)

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
	    (frg-shader (create-shader-module-from-file device (concatenate 'string *assets-dir* "shaders/frag.spv"))))
	(setf (descriptor-set-layout module) (create-descriptor-set-layout device)
	      (pipeline-layout module) (create-pipeline-layout device (list (descriptor-set-layout module)))
	      (pipeline module) (create-graphics-pipeline (device module) (pipeline-cache module) (pipeline-layout module)
							  (render-pass module) 1 width height vtx-shader frg-shader
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
	      (fragment-shader module) frg-shader)

	(setf (uniform-buffer-vs module) (create-uniform-buffer device (foreign-type-size '(:struct 3DDemoVSUBO))))
	(setf (descriptor-set module) (create-descriptor-set device (uniform-buffer-vs module) (list (descriptor-set-layout module))
							     (descriptor-pool module)
							     :range (foreign-type-size '(:struct 3DDemoVSUBO))))
	(values)))))

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
  (view (:struct mat4))
  (proj (:struct mat4)))

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

(defun update-3d-demo-vs-uniform-buffer-object (module proj-matrix)
  (with-slots (device camera start-time) module
    (let* ((current-time (get-internal-real-time))
	   (elapsed-time (- current-time start-time))
	   (model-matrix
	    (meye 4)
	    #+NIL
	    (m* (mrotation (vec3 0.0 0.0 0.0) (rem (* (/ elapsed-time 1000.0d0) #.(/ pi 4.0d0)) #.(/ pi 2.0d0)))
		(mtranslation (vec3 1.0 0.0 0.0))))
	   (view-matrix
	    (meye 4)
	    #+NIL
	    (look-at2 (vec3 (camera-x camera) (camera-y camera) (camera-z camera))
		      (vec3 0.0 0.0 0.0)
		      (vec3 0.0 0.0 1.0))))
      (setq proj-matrix (meye 4))
      (with-foreign-object (p-ubo '(:struct 3DDemoVSUBO))
	(let ((p-m (foreign-slot-pointer p-ubo '(:struct 3DDemoVSUBO) 'model))
	      (p-v (foreign-slot-pointer p-ubo '(:struct 3DDemoVSUBO) 'view))
	      (p-p (foreign-slot-pointer p-ubo '(:struct 3DDemoVSUBO) 'proj)))

	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-m :float (+ i (* j 4))) (coerce (mcref model-matrix i j) 'single-float))))

	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-v :float (+ i (* j 4))) (coerce (mcref view-matrix i j) 'single-float))))

	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-p :float (+ i (* j 4))) (coerce (mcref proj-matrix i j) 'single-float))))

	  (with-foreign-object (pp-data :pointer)
	    (vkMapMemory (h device) (h (allocated-memory (uniform-buffer-vs module))) 0 (foreign-type-size '(:struct 3DDemoVSUBO)) 0 pp-data)
	    (memcpy (mem-aref pp-data :pointer) p-ubo (foreign-type-size '(:struct 3DDemoVSUBO)))
	    (vkUnmapMemory (h device) (h (allocated-memory (uniform-buffer-vs module)))))))))

  (values))

(defun convert-projection-matrix-to-vulkan (src)
  (let ((dest (3d-matrices:mcopy4 src)))
    (setf (3d-matrices:mcref dest 1 1) (- (3d-matrices:mcref dest 1 1))
	  (3d-matrices:mcref dest 2 0) (/ (+ (3d-matrices:mcref dest 2 0) (3d-matrices:mcref dest 3 0)) 2)
	  (3d-matrices:mcref dest 2 1) (/ (+ (3d-matrices:mcref dest 2 1) (3d-matrices:mcref dest 3 1)) 2)
	  (3d-matrices:mcref dest 2 2) (/ (+ (3d-matrices:mcref dest 2 2) (3d-matrices:mcref dest 3 2)) 2)
	  (3d-matrices:mcref dest 2 3) (/ (+ (3d-matrices:mcref dest 2 3) (3d-matrices:mcref dest 3 3)) 2))
    dest))

(defun perspective-2 (width height)
  (let* ((m (mat4))
	 (a (marr m))
	 (f (coerce (/ 1.0d0 (tan (/ pi 8))) 'single-float)))
    (setf (aref a 0) (/ f (/ width height))
	  (aref a 1) 0.0f0
	  (aref a 2) 0.0f0
	  (aref a 3) 0.0f0

	  (aref a 4) 0.0f0
	  (aref a 5) (- f)
	  (aref a 6) 0.0f0
	  (aref a 7) 0.0f0

	  (aref a 8) 0.0f0
	  (aref a 9) 0.0f0
	  (aref a 10) (/ -5 (- 5 -5.0f0))
	  (aref a 11) -1.0f0

	  (aref a 12) 0.0f0
	  (aref a 13) 0.0f0
	  (aref a 14) (/ (* 5 -5.0f0) (- 5 -5.0f0))
	  (aref a 15) 0.0f0)
    m))

(defun 3d-demo-render (module command-buffer fb-width fb-height)
  (with-slots (device) module
    ;;(unless (vertex-buffer module)
      #+NIL
      (vkDestroyBuffer (h device) (h (vertex-buffer module)) (h (allocator module)))
      #+NIL
      (vkFreeMemory (h device) (h (allocated-memory (vertex-buffer module))) (h (allocator module)))
      ;;(setf (vertex-buffer module) nil)
      (setf (vertex-buffer module) (create-vertex-buffer device *vertex-data* *vertex-data-size*));;)

      ;;(unless (index-buffer module)
      #+NIL
      (vkDestroyBuffer (h device) (h (index-buffer module)) (h (allocator module)))
      #+NIL
      (vkFreeMemory (h device) (h (allocated-memory (index-buffer module))) (h (allocator module)))
      ;;(setf (index-buffer module) nil)
      (setf (index-buffer module) (create-index-buffer device *index-data* *index-data-size*));;)

    (let ((geometry-projection-matrix
	   (if (ortho-p (camera module))
	       (mortho (* (- 2) (zoom (camera module)) (/ fb-width fb-height))
		       (*    2  (zoom (camera module)) (/ fb-width fb-height))
		       (* (- 2) (zoom (camera module)))
		       (*    2  (zoom (camera module)))
		       -10 10)
	       (perspective-2 fb-width fb-height))))

      (when (ortho-p (camera module))
	(setq geometry-projection-matrix (convert-projection-matrix-to-vulkan geometry-projection-matrix)))

      (vkCmdBindPipeline (h command-buffer) VK_PIPELINE_BIND_POINT_GRAPHICS (h (pipeline module)))

      (update-3d-demo-vs-uniform-buffer-object module geometry-projection-matrix)

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

      
		    
		    
