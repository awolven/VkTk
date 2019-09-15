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
       (sb-int:with-float-traps-masked
	   (:invalid :inexact :overflow)
	 (main app))))
    app))

#+NOTYET
(defvar *libshaderc*
  (cffi:load-foreign-library "c:/Users/awolven/Documents/Visual Studio 2015/projects/shaderc_wrap/x64/Debug/shaderc_wrap.dll"))

(cffi:defcfun ("compile_to_spv" compile_to_spv) :int
  (kind :int)
  (program :string)
  (bytes :pointer)
  (length :pointer))

(defstruct spirv
  (code)
  (length))

(defun compile-to-spirv (program &key (kind :vertex-shader))
  (let ((enum-kind (ecase kind
		     (:vertex-shader 0)
		     (:fragment-shader 1))))
    (with-foreign-objects ((pp-bytes :pointer)
			   (p-length :int64))
      (when (zerop (compile_to_spv enum-kind program pp-bytes p-length))
	(make-spirv :code (cffi:mem-aref pp-bytes :pointer)
		    :length (cffi:mem-aref p-length :int64))))))

(defvar *selection-mode* :face)

(defclass viewport ()
  ((left :initform 100)
   (top :initform 100)
   (width :initform 1024)
   (height :initform 768)))

(defclass renderer-mixin ()
  ((application :reader application :initarg :app)
   (window :accessor window :initarg :window)
   (scene :reader scene :initarg :scene)
   (allocator :accessor allocator :initarg :allocator)
   (device :accessor device :initarg :device)
   (render-pass :accessor render-pass :initarg :render-pass)
   (pipeline-cache :accessor pipeline-cache :initarg :pipeline-cache)
   (descriptor-pool :accessor descriptor-pool :initarg :descriptor-pool)
   (descriptor-set-layout :accessor descriptor-set-layout)
   (descriptor-set :accessor descriptor-set)
   (pipeline-layout :accessor pipeline-layout)
   (pipeline :accessor pipeline)
   (uniform-buffer-vs :accessor uniform-buffer-vs)
   (draw-index-args :accessor draw-index-args :initform nil)))

(defmethod main-window ((renderer renderer-mixin))
  (main-window (slot-value renderer 'scene)))

(defmethod vertex-shader-module ((device sgpu-device) (renderer renderer-mixin))
  (if (slot-value renderer 'vertex-shader)
      (slot-value renderer 'vertex-shader)
      (prog1 (setf (slot-value renderer 'vertex-shader)
		   (create-shader-module-from-file device (vertex-shader-file-name renderer)))
	(setf (slot-value renderer 'descriptor-set-layout) (create-descriptor-set-layout (dsl-bindings renderer))
	      (slot-value renderer 'pipeline-layout) (create-pipeline-layout device (list (slot-value renderer 'descriptor-set-layout)))))))

(defmethod fragment-shader-module ((device sgpu-device) (renderer renderer-mixin))
  (if (slot-value renderer 'fragment-shader)
      (slot-value renderer 'fragment-shader)
      (setf (slot-value renderer 'fragment-shader)
	    (create-shader-module-from-file device (fragment-shader-file-name renderer)))))

(defclass face-renderer (renderer-mixin)
  ())

(defmethod pipeline-topology ((renderer face-renderer))
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)

(defmethod dsl-bindings ((renderer face-renderer))
  (list (make-instance 'uniform-buffer-for-vertex-shader-dsl-binding)))  

(defmethod vertex-shader-file-name ((renderer face-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/vert.spv"))

(defmethod fragment-shader-file-name ((renderer face-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/frag.spv"))

(defclass edge-renderer (renderer-mixin)
  ())

(defmethod pipeline-topology ((renderer edge-renderer))
  VK_PRIMITIVE_TOPOLOGY_LINE_STRIP)

(defmethod dsl-bindings ((renderer edge-renderer))
  (list (make-instance 'uniform-buffer-for-vertex-shader-dsl-binding)))

(defmethod vertex-shader-file-name ((renderer edge-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/lines.vert.spv"))

(defmethod fragment-shader-file-name ((renderer edge-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/lines.frag.spv"))


(defclass annotation-renderer (renderer-mixin)
  ())

(defmethod pipeline-topology ((renderer annotation-renderer))
  VK_PRIMITIVE_TOPOLOGY_LINE_LIST)

(defmethod vertex-shader-file-name ((renderer annotation-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/lines.vert.spv"))

(defmethod fragment-shader-file-name ((renderer annotation-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/lines.frag.spv"))

(defmethod initialize-instance :after ((renderer renderer-mixin) &rest initargs
				       &key &allow-other-keys)
  (declare (ignore initargs))
  (create-device-objects renderer)
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

(defmethod create-device-objects ((renderer annotation-renderer))
  (with-slots (device) renderer
    (multiple-value-bind (width height) (get-framebuffer-size (window renderer))
      (let ((vtx-shader (create-shader-module-from-file device (vertex-shader-file-name renderer)))
	    (frg-shader (create-shader-module-from-file device (fragment-shader-file-name renderer))))
	(setf (descriptor-set-layout renderer)
	      (create-descriptor-set-layout
	       device
	       :bindings (list (make-instance 'uniform-buffer-for-vertex-shader-dsl-binding)))
	      (pipeline-layout renderer)
	      (create-pipeline-layout device (list (descriptor-set-layout renderer)))
	      (pipeline renderer)
	      (create-graphics-pipeline
	       device (pipeline-cache renderer) (pipeline-layout renderer)
	       (render-pass renderer) 1 width height vtx-shader frg-shader
	       #+windows :line-width #+windows 2.00f0
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
	       :topology (pipeline-topology renderer)
	       :min-sample-shading 1.0f0
	       :depth-test-enable VK_TRUE
	       :depth-write-enable VK_TRUE
	       :depth-compare-op VK_COMPARE_OP_LESS
	       :logic-op VK_LOGIC_OP_COPY
	       :blend-enable VK_FALSE
	       :depth-clamp-enable VK_FALSE
	       :src-alpha-blend-factor VK_BLEND_FACTOR_ONE))

	(destroy-shader-module vtx-shader)
	(destroy-shader-module frg-shader)
	  
	(setf (uniform-buffer-vs renderer)
	      (create-uniform-buffer device (foreign-type-size '(:struct 3DDemoVSUBO))))
	  
	(setf (descriptor-set renderer)
	      (create-descriptor-set
	       device
	       (list (descriptor-set-layout renderer))
	       (descriptor-pool renderer)
	       :descriptor-buffer-info
	       (list (make-instance 'descriptor-uniform-buffer-info
				    :buffer (uniform-buffer-vs renderer)
				    :range (foreign-type-size '(:struct 3DDemoVSUBO)))))))
      (values))))

(defmethod create-device-objects ((renderer face-renderer))
  (with-slots (device) renderer
    (multiple-value-bind (width height) (get-framebuffer-size (window renderer))
	(let ((vtx-shader (create-shader-module-from-file device (vertex-shader-file-name renderer)))
	      (frg-shader (create-shader-module-from-file device (fragment-shader-file-name renderer))))
	  (setf (descriptor-set-layout renderer)
		(create-descriptor-set-layout
		 device
		 :bindings (list (make-instance 'uniform-buffer-for-vertex-shader-dsl-binding)))
		(pipeline-layout renderer)
		(create-pipeline-layout device (list (descriptor-set-layout renderer)))
		(pipeline renderer)
		(create-graphics-pipeline device (pipeline-cache renderer) (pipeline-layout renderer)
		       (render-pass renderer) 1 width height vtx-shader frg-shader
		       #+windows :line-width #+windows 2.00f0
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
		       :topology (pipeline-topology renderer)
		       :min-sample-shading 1.0f0
		       :depth-test-enable VK_TRUE
		       :depth-write-enable VK_TRUE
		       :depth-compare-op VK_COMPARE_OP_LESS
		       :logic-op VK_LOGIC_OP_COPY
		       :blend-enable VK_FALSE
		       :depth-clamp-enable VK_FALSE
		       :src-alpha-blend-factor VK_BLEND_FACTOR_ONE))

	  (destroy-shader-module vtx-shader)
	  (destroy-shader-module frg-shader)
	  
	  (setf (uniform-buffer-vs renderer)
		(create-uniform-buffer device (foreign-type-size '(:struct 3DDemoVSUBO))))
	  
	  (setf (descriptor-set renderer)
		(create-descriptor-set
		 device
		 (list (descriptor-set-layout renderer))
		 (descriptor-pool renderer)
		 :descriptor-buffer-info
		 (list (make-instance 'descriptor-uniform-buffer-info
				      :buffer (uniform-buffer-vs renderer)
				      :range (foreign-type-size '(:struct 3DDemoVSUBO)))))))
	(values))))

(defmethod create-device-objects ((renderer edge-renderer))
  (with-slots (device) renderer
    (multiple-value-bind (width height) (get-framebuffer-size (window renderer))
      (let ((vtx-shader (create-shader-module-from-file device (vertex-shader-file-name renderer)))
	    (frg-shader (create-shader-module-from-file device (fragment-shader-file-name renderer))))
	(setf (descriptor-set-layout renderer)
	      (create-descriptor-set-layout
	       device
	       :bindings (list (make-instance 'uniform-buffer-for-vertex-shader-dsl-binding)))
	      (pipeline-layout renderer)
	      (create-pipeline-layout device (list (descriptor-set-layout renderer)))
	      (pipeline renderer)
	      (create-graphics-pipeline
	       device (pipeline-cache renderer) (pipeline-layout renderer)
	       (render-pass renderer) 1 width height vtx-shader frg-shader
	       #+windows :line-width #+windows 2.00f0
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
	       :topology (pipeline-topology renderer)
	       :min-sample-shading 1.0f0
	       :depth-test-enable VK_TRUE
	       :depth-write-enable VK_TRUE
	       :depth-compare-op VK_COMPARE_OP_LESS
	       :logic-op VK_LOGIC_OP_COPY
	       :blend-enable VK_FALSE
	       :depth-clamp-enable VK_FALSE
	       :src-alpha-blend-factor VK_BLEND_FACTOR_ONE))

	(destroy-shader-module vtx-shader)
	(destroy-shader-module frg-shader)

	(setf (uniform-buffer-vs renderer)
	      (create-uniform-buffer device (foreign-type-size '(:struct 3DDemoVSUBO))))
	  
	(setf (descriptor-set renderer)
	      (create-descriptor-set
	       device
	       (list (descriptor-set-layout renderer))
	       (descriptor-pool renderer)
	       :descriptor-buffer-info
	       (list (make-instance 'descriptor-uniform-buffer-info
				    :buffer (uniform-buffer-vs renderer)
				    :range (foreign-type-size '(:struct 3DDemoVSUBO)))))))
      (values))))
  
(defmethod destroy-device-objects ((renderer face-renderer))
  (let* ((window (window renderer))
	 (swapchain (swapchain window)))

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
    
    (when (pipeline renderer)
      (destroy-pipeline (pipeline renderer))
      (setf (pipeline renderer) nil))
    
    (when (pipeline-layout renderer)
      (destroy-pipeline-layout (pipeline-layout renderer))
      (setf (pipeline-layout renderer) nil))

    #+NIL
    (when (descriptor-set renderer)
      (free-descriptor-sets (list (descriptor-set renderer)) (descriptor-pool renderer))
      (setf (descriptor-set renderer) nil))
    
    (when (descriptor-set-layout renderer)
      (destroy-descriptor-set-layout (descriptor-set-layout renderer))
      (setf (descriptor-set-layout renderer) nil))

    (when (uniform-buffer-vs renderer)
      (destroy-buffer (uniform-buffer-vs renderer))
      (setf (uniform-buffer-vs renderer) nil))

    #+NOTYET
    (progn
    (when (vertex-data renderer)
      (foreign-free (vertex-data renderer))
      (setf (vertex-data renderer) nil))

    (setf (vertex-data-size renderer) nil)

    (when (index-data renderer)
      (foreign-free (index-data renderer))
      (setf (index-data renderer) nil))

    (setf (index-data-size renderer) nil)    
    )
    (values)))

(defmethod destroy-device-objects ((renderer annotation-renderer))
  (let* (#+NIL(window (window renderer))
	      #+NIL(swapchain (swapchain window)))

    (when (pipeline renderer)
      (destroy-pipeline (pipeline renderer))
      (setf (pipeline renderer) nil))
    
    (when (pipeline-layout renderer)
      (destroy-pipeline-layout (pipeline-layout renderer))
      (setf (pipeline-layout renderer) nil))

    #+NIL
    (when (descriptor-set renderer)
      (free-descriptor-sets (list (descriptor-set renderer)) (descriptor-pool renderer))
      (setf (descriptor-set renderer) nil))
    
    (when (descriptor-set-layout renderer)
      (destroy-descriptor-set-layout (descriptor-set-layout renderer))
      (setf (descriptor-set-layout renderer) nil))
    
    (when (uniform-buffer-vs renderer)
      (destroy-buffer (uniform-buffer-vs renderer))
      (setf (uniform-buffer-vs renderer) nil))

    #+NOTYET
    (progn
      (when (vertex-data renderer)
	(foreign-free (vertex-data renderer))
	(setf (vertex-data renderer) nil))
      
      (setf (vertex-data-size renderer) nil)
      
      (when (index-data renderer)
	(foreign-free (index-data renderer))
	(setf (index-data renderer) nil))
      
      (setf (index-data-size renderer) nil)
      )

    (values)))

(defmethod destroy-device-objects ((renderer edge-renderer))

  (when (pipeline renderer)
    (destroy-pipeline (pipeline renderer))
    (setf (pipeline renderer) nil))
    
  (when (pipeline-layout renderer)
    (destroy-pipeline-layout (pipeline-layout renderer))
    (setf (pipeline-layout renderer) nil))

  (when (descriptor-set-layout renderer)
    (destroy-descriptor-set-layout (descriptor-set-layout renderer))
    (setf (descriptor-set-layout renderer) nil))
    
  (when (uniform-buffer-vs renderer)
    (destroy-buffer (uniform-buffer-vs renderer))
    (setf (uniform-buffer-vs renderer) nil))
    
  (values))

(defparameter *vertices-list*
  (list -0.5f0 -0.5f0 0.5f0 1.0f0 0.0f0 0.0f0
	 0.5f0 -0.5f0 0.5f0 0.0f0 1.0f0 0.0f0
	 0.5f0  0.5f0 0.5f0 0.0f0 0.0f0 1.0f0
	 -0.5f0  0.5f0 0.5f0 1.0f0 1.0f0 1.0f0

	 -0.5f0 -0.5f0 -0.5f0 1.0f0 0.0f0 0.0f0
	 0.5f0 -0.5f0 -0.5f0 0.0f0 1.0f0 0.0f0
	 0.5f0  0.5f0 -0.5f0 0.0f0 0.0f0 1.0f0
	 -0.5f0  0.5f0 -0.5f0 1.0f0 1.0f0 1.0f0
	))

(defparameter *vertex-data* (foreign-alloc :float :initial-contents *vertices-list*))
(defparameter *vertex-data-size* (* (foreign-type-size :float) (length *vertices-list*)))

(defparameter *indices*
  (list 0 1 2 2 3 0
	4 5 6 6 7 4
	0 1 4 4 5 1
	1 2 6 1 5 6
	0 4 7 0 3 7
	7 6 3 3 2 6))

(defparameter *index-data* (foreign-alloc :unsigned-short :initial-contents *indices*))
(defparameter *index-data-size* (* (foreign-type-size :unsigned-short) (length *indices*)))

(defvar *draw-index-args* nil)

(defparameter *axes-coord-list*
  (list 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0
	1.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0
	0.0f0 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0
	0.0f0 1.0f0 0.0f0 0.0f0 1.0f0 0.0f0
	0.0f0 0.0f0 0.0f0 0.0f0 0.0f0 1.0f0
	0.0f0 0.0f0 1.0f0 0.0f0 0.0f0 1.0f0))

(defparameter *axes-coord-data*
  (foreign-alloc :float :initial-contents *axes-coord-list*))

(defparameter *axes-coord-data-size*
  (* (foreign-type-size :float) (length *axes-coord-list*)))

(defparameter *axes-indices*
  (list 0 1 2 3 4 5))

(defparameter *axes-index-data* (foreign-alloc :unsigned-short :initial-contents *axes-indices*))

(defparameter *axes-index-data-size* (* (foreign-type-size :unsigned-short) (length *axes-indices*)))

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
  (view (:struct mat4))
  (proj (:struct mat4))
  (clip (:struct mat4)))

(defcstruct 3DDemoGSUBO
  (proj (:struct mat4))
  (pickOrigin (:struct vec4))
  (pickingDir (:struct vec4)))	

(defun look-at2 (eye target up-dir)
	   
  (let* ((forward (vunit (v- eye target)))
	 (left (vunit (vc up-dir forward)))
	 (up (vc forward left))
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

(defun euclid (vec4)
  (vec3 (/ (vx vec4) (vw vec4))
	(/ (vy vec4) (vw vec4))
	(/ (vz vec4) (vw vec4))))

(defun compute-picking-ray (window camera view-matrix projection-matrix clip-matrix)
  (multiple-value-bind (viewport-width viewport-height) (get-framebuffer-size window)
    (let ((unclip (minv clip-matrix))
	  (unproj (minv projection-matrix))
	  (unview (minv view-matrix)))
      (let ((p (igp::eye-point camera)))
	(multiple-value-bind (mouse-x mouse-y) (get-cursor-pos window)
	  (let* ((mouse-clip (vec4 (- (/ (* 2.0f0 mouse-x) viewport-width) 1.0f0)
				   (- (/ (* 2.0f0 mouse-y) viewport-height) 1.0f0)
				   (if (igp::perspective-p camera) 0.3254f0 0.0f0)
				   1.0f0))
		 (mouse-world (euclid (m* unview unproj unclip mouse-clip))))
	    (igText (format nil "mouse-world: ~S" mouse-world))
	    (let ((ray
		   (if (igp::perspective-p camera)
		       (igp::make-ray :origin p :direction (vunit (v- mouse-world p)))
		       (let ((dir (vunit (v- (slot-value camera 'igp::aim-point) p))))
			 (igp::make-ray :origin mouse-world :direction dir)))))
	      (igText (format nil "~S" ray))
	      ray)))))))

(defun copy-uniform-buffer-memory (device data uniform-buffer-memory size)
  (with-foreign-object (pp-data :pointer)
    (vkMapMemory (h device) (h uniform-buffer-memory) 0 size 0 pp-data)
    (memcpy (mem-aref pp-data :pointer) data size)
    (vkUnmapMemory (h device) (h uniform-buffer-memory))))

(defun update-vertex-shader-uniform-buffer-object (module model-matrix view-matrix proj-matrix clip-matrix)

  (with-foreign-object (p-ubo '(:struct 3DDemoVSUBO))
	(let ((p-m (foreign-slot-pointer p-ubo '(:struct 3DDemoVSUBO) 'model))
	      (p-v (foreign-slot-pointer p-ubo '(:struct 3DDemoVSUBO) 'view))
	      (p-p (foreign-slot-pointer p-ubo '(:struct 3DDemoVSUBO) 'proj))
	      (p-c (foreign-slot-pointer p-ubo '(:struct 3DDemoVSUBO) 'clip)))

	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-m :float (+ i (* j 4))) (coerce (mcref model-matrix i j) 'single-float))))

	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-v :float (+ i (* j 4))) (coerce (mcref view-matrix i j) 'single-float))))

	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-p :float (+ i (* j 4))) (coerce (mcref proj-matrix i j) 'single-float))))


	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-c :float (+ i (* j 4))) (coerce (mcref clip-matrix i j) 'single-float))))
	  
	  (copy-uniform-buffer-memory (device module)
				      p-ubo (allocated-memory (uniform-buffer-vs module))
				      (foreign-type-size '(:struct 3DDemoVSUBO)))))
      (values))


(defparameter +clip-matrix+
  (mat 1 0 0 0
       0 -1 0 0
       0 0 1/2 0
       0 0 1/2 1))

(defun 3d-demo-select (queue renderer model-matrix view-matrix projection-matrix)
  (let ((scene (slot-value renderer 'scene)))
    
    (let* ((ray (compute-picking-ray (slot-value renderer 'window)
				     (slot-value (slot-value renderer 'scene) 'igp::camera)
				     view-matrix projection-matrix +clip-matrix+))
	   (box-intersecting (igp::ray-intersects-bvh ray (first (slot-value scene 'igp::standins)) model-matrix)))
      
      (when box-intersecting
	(cond ((eq *selection-mode* :face)
	       (let* ((toplevel-standin (first (slot-value scene 'igp::standins))))
		 (igp::unhighlight-toplevel-standin queue toplevel-standin)
		 (let ((selected (caar
				  (sort (remove-if #'null
						   (mapcar #'(lambda (face)
							       (let ((tmin (igp::ray-intersects-face-p ray face model-matrix)))
								 (when tmin
								   (list face
									 tmin))))
							   box-intersecting))
					#'< :key #'cadr))))
		   (when selected
		     (igp::highlight-standin queue selected toplevel-standin)
		     (igText (format nil "~S" (slot-value selected 'igp::random-id))))
		   
		   ))))
	
	(igText "SELECTED!")))))

(let ((last-time (get-internal-real-time))
      (frame-counter 0)
      (show-demo-window t)
      (show-another-window nil)
      (temp)
      (dt))

  (defmethod process-ui (app)

    (imgui-new-frame (imgui-module app))

    (let* ((scene (slot-value app 'scene))
	   (camera (slot-value scene 'igp::camera)))

      (ig:igBeginMainMenuBar)
      (when (ig:begin-menu "File")
	(ig:menu-item "New")
	(ig:menu-item "Open")
	(when (ig:menu-item "Exit")
	  (shutdown-application app))
	(ig:end-menu))
      (when (ig:begin-menu "Edit")
	(ig:menu-item "Create")
	(ig:menu-item "Move")
	(ig:menu-item "Delete")
	(ig:end-menu))
      (when (ig:begin-menu "View")
	(when (ig:menu-item "Reset Camera")
	  (igp::reset-camera camera))
	(ig:end-menu))	  
      (ig:igEndMainMenuBar)

      (incf frame-counter)

      (multiple-value-bind (width height) (get-framebuffer-size (main-window app))
	(ig:set-next-window-pos 0 0 :condition :always)
	(ig:set-next-window-size width height)
	(ig:begin "background"
		  :no-background t
		  :no-title-bar t
		  :no-resize t
		  :no-move t
		  :no-scrollbar t
		  :no-scroll-with-mouse t
		  :no-collapse t
		  :no-saved-settings t
		  :no-mouse-inputs t
		  :no-focus-on-appearing t
		  :no-bring-to-front-on-focus t
		  :no-nav-inputs t
		  :no-nav-focus t
		  :no-decoration t
		  :no-inputs t)

	(vktk::set-cursor-screen-pos 100 50)
	(ig:text "TEST")
	(ig:end)

	(ig:set-next-window-pos (- width 120) 19 :condition :always)
	(ig:set-next-window-size 120 (- height 38))
	(ig:begin "right-docker"
		  :no-title-bar t
		  :no-resize t
		  :no-move t
		  :no-scrollbar t
		  :no-scroll-with-mouse t
		  :no-saved-settings t
		  ;;:no-mouse-inputs t
		  :no-focus-on-appearing t
		  :no-bring-to-front-on-focus t
		  :no-nav-inputs t
		  :no-nav-focus t
		  :no-decoration t
		  ;;:no-inputs t
		  )
	(ig:text "selection mode")
	(when (ig:radio-button "compsolid" (eq *selection-mode* :compsolid))
	  (setq *selection-mode* :compsolid))
	(when (ig:radio-button "compound" (eq *selection-mode* :compound))
	  (setq *selection-mode* :compound))
	(when (ig:radio-button "solid" (eq *selection-mode* :solid))
	  (setq *selection-mode* :solid))
	(when (ig:radio-button "shell" (eq *selection-mode* :shell))
	  (setq *selection-mode* :shell))	
	(when (ig:radio-button "face" (eq *selection-mode* :face))
	  (setq *selection-mode* :face))
	(when (ig:radio-button "wire" (eq *selection-mode* :wire))
	  (setq *selection-mode* :wire))
	(when (ig:radio-button "edge" (eq *selection-mode* :edge))
	  (setq *selection-mode* :edge))
	(when (ig:radio-button "vertex" (eq *selection-mode* :vertex))
	  (setq *selection-mode* :vertex))

	(ig:new-line)
	(ig:text "view projection")
	(when (ig:radio-button "perspective" (igp::perspective-p camera))
	  (setf (slot-value camera 'igp::type) :perspective))
	(when (ig:radio-button "orthographic" (igp::ortho-p camera))
	  (setf (slot-value camera 'igp::type) :ortho))
	(igp::update-projection-matrix camera)

	(ig:new-line)
	(with-foreign-object (p-f :float 3)
	  (let ((ap (slot-value camera 'igp::aim-point)))
	    (setf (mem-aref p-f :float 0) (vx ap)
		  (mem-aref p-f :float 1) (vy ap)
		  (mem-aref p-f :float 2) (vz ap))
	    
	    (ig:igInputFloat3 "aim point" p-f "%f %f %f" 0)

	    (setf (slot-value camera 'igp::aim-point)
		  (vec3 (mem-aref p-f :float 0) (mem-aref p-f :float 1) (mem-aref p-f :float 2)))
	    (igp::update-view-matrix camera)))
	(ig:end)

	(ig:set-next-window-pos 0 (- height 20) :condition :always)
	(ig:set-next-window-size width 20)

	(ig:begin "bottom-docker"
		  :no-title-bar t
		  :no-resize t)
	(if (elt (igp::camera-mode scene) 2)
	    (progn
	      (vktk::set-cursor-screen-pos 5 (- height 15))
	      (ig:text "move to rotate camera")
	      (vktk::set-cursor-screen-pos (- (/ width 2) 40) (- height 15))
	      (ig:text "M: accept")
	      (vktk::set-cursor-screen-pos (- width 150) (- height 15))
	      (ig:text "[Ctrl] + move to pan"))
	    (progn
	      (vktk::set-cursor-screen-pos 5 (- height 15))
	      (ig:text "L: select")
	      (vktk::set-cursor-screen-pos (- (/ width 2) 40) (- height 15))
	      (ig:text "M: press: camera mode, scroll: zoom")))
	(ig:end))      
		 
      (text "Hello, wworld!")

      (text "camera up: ~S" (igp::up-vector camera))
      
      (color-edit "clear color" (clear-value app))
		 
      (text "~S" (clear-value app))
		 
      (text "camera position: ~S" (igp::eye-point camera))

      (text "camera right: ~S" (vc (v- (slot-value camera 'igp::aim-point) (igp::eye-point camera)) (igp::up-vector camera)))
		 
      (let* ((ortho-p (igp::ortho-p camera)))
	       
	(when (button (if ortho-p "Set Perspective" "Set Orthographic") :dx 120.0f0 :dy 20.0f0)
	  (setf (slot-value camera 'igp::type) (if ortho-p :perspective :ortho))
	  (igp::update-projection-matrix camera)))

      (when (button "Reset Camera" :dx 120.0f0 :dy 20.0f0)
	(igp::reset-camera camera))

      (when (button "Demo Window" :dx 100.0f0 :dy 20.0f0)
	(if show-demo-window
	    (setq show-demo-window nil)
	    (setq show-demo-window t)))
      
      (when (button "Another Window" :dx 120.0f0 :dy 20.0f0)
	(if show-another-window
	    (setq show-another-window nil)
	    (setq show-another-window t)))
      
      (setq temp (get-internal-real-time)
	    dt (- temp last-time))
      
      (text "~s ms/frame (~s FPS)" dt
	    (if (zerop dt)
		0
		(round (/ frame-counter (/ dt 1000.0f0)))))
      
      (setq frame-counter 0
	    last-time temp)
      
      (when show-another-window
	
	(multiple-value-bind (ignore open)
	    (begin "Another Window" :open show-another-window)
	  (declare (ignore ignore))
	  
	  (setq show-another-window open)
	
	  (text "Hello from another window!")
	
	  (end)))
      
      (when show-demo-window
	(set-next-window-pos 650.0f0 20.0f0 :condition :first-use-ever)
	
	(setq show-demo-window (show-demo-window show-demo-window)))

      (igp::maybe-move-camera camera))))


(defmethod 3d-demo-render (queue (renderer face-renderer) command-buffer fb-width fb-height
			   &optional (model-matrix (meye 4)))

  (let ((scene (slot-value renderer 'scene)))
    (with-slots (device) renderer
      (let* ((camera (slot-value scene 'igp::camera))
	     (view-matrix (slot-value camera 'igp::view-matrix))
	     (projection-matrix (slot-value camera 'igp::projection-matrix)))
	(with-slots (window) renderer
	  (let* ((current-time (get-internal-real-time))
		   (elapsed-time (- current-time (igp::start-time (slot-value renderer 'scene)))))


	    #+NIL
	      (setq model-matrix 
		    (m* (mrotation (vec3 0.0 0.0 1.0)
				   (rem (* (/ elapsed-time 1000.0d0) #.(/ pi 4.0d0)) #.(* pi 2.0d0)))
			(mtranslation (vec3 100.0 0.0 0.0)))))

	  (3d-demo-select queue renderer model-matrix view-matrix projection-matrix)

	  (vkCmdBindPipeline (h command-buffer) VK_PIPELINE_BIND_POINT_GRAPHICS (h (pipeline renderer)))

	  (update-vertex-shader-uniform-buffer-object renderer model-matrix view-matrix projection-matrix +clip-matrix+))
	
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
				   
		  (vkCmdSetScissor (h command-buffer) 0 1 p-scissor))))))

	(loop for object in (slot-value (slot-value renderer 'scene) 'igp::standins)
	   do (let ((standin object))
		(unless (igp::face-vertex-buffer standin)
		  (let ((vertex-array (igp::face-vertex-array standin)))
		    (setf (igp::face-vertex-buffer standin)
			  (sb-sys:with-pinned-objects (vertex-array)
			    (vktk:create-vertex-buffer device
						       (sb-sys:vector-sap vertex-array)
						       (* (length (igp::face-vertex-array standin))
							  (foreign-type-size :float)))))))
		(unless (igp::face-index-buffer standin)
		  (let ((index-array (igp::face-index-array standin)))
		    (setf (igp::face-index-buffer standin)
			  (sb-sys:with-pinned-objects (index-array)
			    (vktk:create-index-buffer device
						      (sb-sys:vector-sap index-array)
						      (* (length (igp::face-index-array standin))
							 (foreign-type-size :unsigned-short)))))))

		(cmd-bind-vertex-buffers command-buffer (list (igp::face-vertex-buffer standin)))

		(cmd-bind-descriptor-sets command-buffer (pipeline-layout renderer) (list (descriptor-set renderer)))
		(cmd-bind-index-buffer command-buffer (igp::face-index-buffer standin))
		
		(labels ((render-indexed (standin)
			   (loop for cmd across (igp::face-commands standin)
			      do (cmd-draw-indexed command-buffer cmd))))
		  
		  (render-indexed standin)))))
    
    model-matrix))

(defmethod 3d-demo-render (queue (renderer edge-renderer) command-buffer fb-width fb-height
			   &optional (model-matrix (meye 4)))
  (let ((scene (slot-value renderer 'scene)))
    (with-slots (device) renderer
      (let* ((camera (slot-value scene 'igp::camera))
	     (view-matrix (slot-value camera 'igp::view-matrix))
	     (projection-matrix (slot-value camera 'igp::projection-matrix)))
	(with-slots (window) renderer

	  (vkCmdBindPipeline (h command-buffer) VK_PIPELINE_BIND_POINT_GRAPHICS (h (pipeline renderer)))

	  (update-vertex-shader-uniform-buffer-object renderer model-matrix view-matrix projection-matrix +clip-matrix+))
	
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
				   
		  (vkCmdSetScissor (h command-buffer) 0 1 p-scissor))))))

	(loop for object in (slot-value (slot-value renderer 'scene) 'igp::standins)
	   do (let ((standin object))
		(unless (igp::edge-vertex-buffer standin)
		  (let ((vertex-array (igp::edge-vertex-array standin)))
		    (setf (igp::edge-vertex-buffer standin)
			  (sb-sys:with-pinned-objects (vertex-array)
			    (vktk:create-vertex-buffer device (sb-sys:vector-sap vertex-array) (* (length (igp::edge-vertex-array standin)) (foreign-type-size :float)))))))
		(unless (igp::edge-index-buffer standin)
		  (let ((index-array (igp::edge-index-array standin)))
		    (setf (igp::edge-index-buffer standin)
			  (sb-sys:with-pinned-objects (index-array)
			    (vktk:create-index-buffer device (sb-sys:vector-sap index-array) (* (length (igp::edge-index-array standin)) (foreign-type-size :unsigned-short)))))))

		(cmd-bind-vertex-buffers command-buffer (list (igp::edge-vertex-buffer standin)))

		(cmd-bind-descriptor-sets command-buffer (pipeline-layout renderer) (list (descriptor-set renderer)))

		(cmd-bind-index-buffer command-buffer (igp::edge-index-buffer standin))
		
		(labels ((render-indexed (standin)
			   (loop for cmd across (igp::edge-commands standin)
			      do (cmd-draw-indexed command-buffer cmd))))
		  (render-indexed standin)
		))))
    model-matrix))


(defmethod 3d-demo-render (queue (renderer annotation-renderer) command-buffer fb-width fb-height
			   &optional (model-matrix (meye 4)))

  (let ((scene (slot-value renderer 'scene)))
    (with-slots (device) renderer
      (let* ((camera (slot-value scene 'igp::camera))
	     (view-matrix (slot-value camera 'igp::view-matrix))
	     (projection-matrix (slot-value camera 'igp::annotation-projection-matrix)))
	
	(with-slots (window) renderer
	  (let ((camera-distance (slot-value camera 'igp::distance)))
	    
	    (nmscale model-matrix (vec3 camera-distance camera-distance camera-distance)))
	  
	  
	  (vkCmdBindPipeline (h command-buffer) VK_PIPELINE_BIND_POINT_GRAPHICS (h (pipeline renderer)))	  
	  
	  (update-vertex-shader-uniform-buffer-object renderer model-matrix view-matrix projection-matrix +clip-matrix+)
	  )
	
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
		
		(vkCmdSetScissor (h command-buffer) 0 1 p-scissor))))))

      (let ((annotation (slot-value (slot-value renderer 'scene) 'igp::annotation)))

	    (let ((index-array (igp::line-index-array annotation)))
	      (let ((vertex-array (igp::line-vertex-array annotation)))

		(unless (igp::vertex-buffer annotation)
		  
		  (setf (igp::vertex-buffer annotation)
			(sb-sys:with-pinned-objects
			 (vertex-array)
			 (create-vertex-buffer device (sb-sys:vector-sap vertex-array)
					       (* (length (igp::line-vertex-array annotation))
						  (foreign-type-size :float))))))
		(unless (igp::index-buffer annotation)
		  (setf (igp::index-buffer annotation)
			(sb-sys:with-pinned-objects
			 (index-array)
			 (create-index-buffer device (sb-sys:vector-sap index-array)
					      (* (length (igp::line-index-array annotation))
						 (foreign-type-size :unsigned-short))))))

		(with-foreign-objects ((p-vertex-buffers 'VkBuffer)
				       (p-offsets 'VkDeviceSize)
				       (p-descriptor-sets 'VkDescriptorSet))
		  
		  (setf (mem-aref p-vertex-buffers 'VkBuffer) (h (igp::vertex-buffer annotation))
			(mem-aref p-offsets 'VkDeviceSize) 0
			(mem-aref p-descriptor-sets 'VkDescriptorSet) (h (descriptor-set renderer)))
		  
		  (vkCmdBindVertexBuffers (h command-buffer) 0 1 p-vertex-buffers p-offsets)
		  
		  (vkCmdBindDescriptorSets (h command-buffer) VK_PIPELINE_BIND_POINT_GRAPHICS
					   (h (pipeline-layout renderer))
					   0 1 p-descriptor-sets 0 +nullptr+)
		  
		  (vkCmdBindIndexBuffer (h command-buffer) (h (igp::index-buffer annotation))
					0 VK_INDEX_TYPE_UINT16)
		  
		  (vkCmdDrawIndexed (h command-buffer) (length index-array) 1 0 0 0))))))
    (values)))

(defmethod render-graphics (app queue command-pool)
  ;; draw stuff here
  (let ((main-window (main-window app)))
    (multiple-value-bind (w h) (get-framebuffer-size main-window)
      (3d-demo-render queue (annotation-renderer app)
		      (elt (command-buffers command-pool)
			   (current-frame app))
		      w h)
      (let ((model-matrix (3d-demo-render queue (face-renderer app)
					  (elt (command-buffers command-pool)
					       (current-frame app))
					  w h)))
	(declare (ignorable model-matrix))
	(3d-demo-render queue
			(edge-renderer app)
			(elt (command-buffers command-pool)
			     (current-frame app))
			w h model-matrix))
      (imgui-render (imgui-module app)
		    (elt (command-buffers command-pool) (current-frame app))
		    (current-frame app)))))

		    
(defcstruct xform-uniform
  (matrix (:struct mat4))
  (nverts :uint32))

	
(defun compute-transformation (device matrix vertex-array)
  (assert (zerop (mod (length vertex-array) 3)))
  (let ((output-array)
	(nverts (/ (length vertex-array) 3))
	(dsl)
	(pipeline-layout)
	(shader-module)
	(pipeline)
	(command-pool)
	(command-buffer)
	(ds)
	(uniform-buffer)
	(storage-buffer)
	(storage-buffer-memory))

    (unwind-protect
	 (multiple-value-bind (queue family-index)
	     (compute-queue device)
	   
	   (setq dsl (create-descriptor-set-layout
		      device
		      :bindings
		      (list (make-instance 'sample-uniform-buffer-for-compute-shader-dsl-binding)
			    (make-instance 'sample-input-storage-buffer-for-compute-shader-dsl-binding)))
		 pipeline-layout (create-pipeline-layout device (list dsl))
		 shader-module (create-shader-module-from-file device (concatenate 'string *assets-dir* "shaders/comp.spv"))
		 pipeline (create-compute-pipeline device pipeline-layout shader-module)
		 command-pool (second (assoc family-index (command-pools device)))
		 command-buffer (elt (command-buffers command-pool) 0))

	   (time
	    (unwind-protect
		 (let* ((size (* (load-time-value (foreign-type-size :float)) (length vertex-array))))
	   
		   (setq storage-buffer (create-buffer-1 device size VK_BUFFER_USAGE_STORAGE_BUFFER_BIT)
			 storage-buffer-memory (allocate-buffer-memory
						device storage-buffer
						(logior VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
							VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)))
	    
		   (bind-buffer-memory device storage-buffer storage-buffer-memory)
		   (setf (allocated-memory storage-buffer) storage-buffer-memory)

		   (with-foreign-object (pp-data :pointer)
		     (vkMapMemory (h device) (h storage-buffer-memory) 0 size 0 pp-data)
		     (sb-sys:with-pinned-objects
		      (vertex-array)
		      (memcpy (mem-aref pp-data :pointer) (sb-sys:vector-sap vertex-array) size))
		     (vkUnmapMemory (h device) (h storage-buffer-memory)))
				
		   (let* ((uniform-range (* (load-time-value (foreign-type-size :float)) 16)))
	     
		     (setq uniform-buffer (create-uniform-buffer device uniform-range))

		     (with-foreign-object (p-uniform '(:struct xform-uniform))
		       (let ((p-matrix (foreign-slot-pointer p-uniform '(:struct xform-uniform) 'matrix)))
			 
			 (loop for i from 0 below 4
			    do (loop for j from 0 below 4
				  do (setf (mem-aref p-matrix :float (+ (* j 4) i)) (mcref4 matrix i j))
				  finally (setf (foreign-slot-value p-uniform '(:struct xform-uniform) 'nverts) nverts))))

		       (copy-uniform-buffer-memory device p-uniform
						   (allocated-memory uniform-buffer) uniform-range))
	    
		     (setq ds (create-descriptor-set device (list dsl)
						     (descriptor-pool device)
						     :descriptor-buffer-info
						     (list (make-instance 'descriptor-uniform-buffer-info
									  :buffer uniform-buffer
									  :range uniform-range)
							   (make-instance 'descriptor-storage-buffer-info
									  :buffer storage-buffer
									  :range size))))
		

		     (begin-command-buffer command-buffer)
	   
		     (vkCmdBindPipeline (h command-buffer) VK_PIPELINE_BIND_POINT_COMPUTE (h pipeline))
	   
		     (cmd-bind-descriptor-sets command-buffer pipeline-layout (list ds) :compute)
	     
		     (vkCmdDispatch (h command-buffer) nverts 1 1)
	     
		     (vkEndCommandBuffer (h command-buffer))
	     
		     (with-vk-struct (p-submit-info VkSubmitInfo)
		       (with-foreign-slots ((vk::commandBufferCount
					     vk::pCommandBuffers)
					    p-submit-info
					    (:struct VkSubmitInfo))
		 
			 (with-foreign-object (p-command-buffer 'VkCommandBuffer)
			   (setf (mem-aref p-command-buffer 'VkCommandBuffer) (h command-buffer))
		   
			   (setf vk::commandBufferCount 1
				 vk::pCommandBuffers p-command-buffer)
		   
			   (vkQueueSubmit (h queue) 1 p-submit-info VK_NULL_HANDLE)
		       
			   (setq output-array
				 (make-array (length vertex-array) :element-type 'single-float))
		       
			   (vkQueueWaitIdle (h queue))

			   (with-foreign-object (pp-data :pointer)
			     (vkMapMemory (h device) (h (allocated-memory storage-buffer)) 0 size 0 pp-data)
			     (sb-sys:with-pinned-objects
			      (output-array)
			      (memcpy (sb-sys:vector-sap output-array) (mem-aref pp-data :pointer) size))
			     (vkUnmapMemory (h device) (h (allocated-memory storage-buffer)))))))))
	      
	      #+NIL(when ds (free-descriptor-sets (list ds) (descriptor-pool device))) ;; just return to pool
	      (when storage-buffer
		(destroy-buffer storage-buffer))
	      (when uniform-buffer
		(destroy-buffer uniform-buffer)))))
      
      (when pipeline (destroy-pipeline pipeline))
      (when shader-module (destroy-shader-module shader-module))
      (when pipeline-layout (destroy-pipeline-layout pipeline-layout))
      (when dsl (destroy-descriptor-set-layout dsl)))
    output-array))

#+NIL
(vktk::compute-transformation *d* (3d-matrices:nmrotate (3d-matrices::meye 4) (3d-vectors::vec3 0 0 1) 42) vktk::*test-verts*)

(defcstruct ray-intersects-triangles-uniform
  (matrix (:struct mat4))
  (origin (:struct vec3))
  (dir (:struct vec3))
  (ntri :uint32))

#+NIL(defun compute-ray-triangle-intersection (ray matrix vertices))
