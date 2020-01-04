(in-package :igp)



(defclass database-frame-resource ()
  ((point-buffer :initform nil :accessor point-buffer)
   (point-buffer-memory :initform nil :accessor point-buffer-memory)
   (point-buffer-memory-size :initform 0 :accessor point-buffer-memory-size)
   (line-list-vertex-buffer :initform nil)
   (line-list-vertex-buffer-memory :initform nil)
   (line-list-vertex-buffer-memory-size :initform 0)
   (line-list-index-buffer :initform nil)
   (line-list-index-buffer-memory :initform nil)
   (line-list-index-buffer-memory-size :initform 0)
   (line-strip-vertex-buffer :initform nil :accessor line-strip-vertex-buffer)
   (line-strip-vertex-buffer-memory :initform nil :accessor line-strip-vertex-buffer-memory)
   (line-strip-vertex-buffer-memory-size :initform 0 :accessor line-strip-vertex-buffer-memory)
   (line-strip-index-buffer :initform nil :accessor line-strip-index-buffer)
   (line-strip-index-buffer-memory :initform nil :accessor line-strip-index-buffer-memory)
   (line-strip-index-buffer-memory-size :initform 0 :accessor line-strip-index-buffer-memory-size)
   (triangle-list-vertex-buffer :initform nil)
   (triangle-list-vertex-buffer-memory :initform nil)
   (triangle-list-vertex-buffer-memory-size :initform 0)
   (triangle-list-index-buffer :initform nil)
   (triangle-list-index-buffer-memory :initform nil)
   (triangle-list-index-buffer-memory-size :initform 0)
   (triangle-strip-vertex-buffer :initform nil)
   (triangle-strip-vertex-buffer-memory :initform nil)
   (triangle-strip-vertex-buffer-memory-size :initform 0)
   (triangle-strip-index-buffer :initform nil)
   (triangle-strip-index-buffer-memory :initform nil)
   (triangle-strip-index-buffer-memory-size :initform 0)
   (triangle-fan-buffer :initform nil)))

(defclass database-mixin ()
  ((frame-resources :initform nil :accessor frame-resources)
   (db-lock  :initform (sb-thread:make-mutex :name "database lock"))))

(defconstant +point-vertices-block-size+ #.(* 6 100))
(defconstant +line-list-vertices-block-size+ #.(* 3 1000))
(defconstant +line-list-indices-block-size+ #.(* 3 1000))
(defconstant +line-strip-vertices-block-size+ #.(* 3 1000))
(defconstant +line-strip-indices-block-size+ #.(* 3 1000))
(defconstant +triangle-list-vertices-block-size+ #.(* 3 3 3000))
(defconstant +triangle-list-indices-block-size+ #.(* 3 3 3000))
(defconstant +triangle-strip-vertices-block-size+ #.(* 3 3 3000))
(defconstant +triangle-strip-indices-block-size+ #.(* 3 3 3000))

(defclass dynamic-database-mixin (database-mixin)
  ((point-vertices :accessor point-vertices :initform (foreign-alloc :float :count +point-vertices-block-size+))
   (point-vertices-fill-pointer :accessor point-vertices-fill-pointer :initform 0)
   (point-vertices-allocated-size :accessor point-vertices-allocated-size :initform +point-vertices-block-size+)

   (line-list-vertices :accessor line-list-vertices :initform (foreign-alloc :float :count +line-list-vertices-block-size+))
   (line-list-vertices-fill-pointer :accessor line-list-vertices-fill-pointer :initform 0)
   (line-list-vertices-allocated-size :accessor line-list-vertices-allocated-size :initform +line-list-vertices-block-size+)
   (line-list-indices :accessor line-list-indices :initform (foreign-alloc :unsigned-short :count +line-list-indices-block-size+))
   (line-list-indices-fill-pointer :accessor line-list-indices-fill-pointer :initform 0)
   (line-list-indices-allocated-size :accessor line-list-indices-allocated-size :initform +line-list-indices-block-size+)
   (line-list-commands :accessor line-list-commands :initform (make-array 100 :adjustable t :fill-pointer 0))

   (line-strip-vertices :accessor line-strip-vertices :initform (foreign-alloc :float :count +line-strip-vertices-block-size+))
   (line-strip-vertices-fill-pointer :accessor line-strip-vertices-fill-pointer :initform 0)
   (line-strip-vertices-allocated-size :accessor line-strip-vertices-allocated-size :initform +line-strip-vertices-block-size+)
   (line-strip-indices :accessor line-strip-indices :initform (foreign-alloc :unsigned-short :count +line-strip-indices-block-size+))
   (line-strip-indices-fill-pointer :accessor line-strip-indices-fill-pointer :initform 0)
   (line-strip-indices-allocated-size :accessor line-strip-indices-allocated-size :initform +line-strip-indices-block-size+)
   (line-strip-commands :accessor line-strip-commands :initform (make-array 100 :adjustable t :fill-pointer 0))

   (triangle-list-vertices :accessor triangle-list-vertices :initform (foreign-alloc :float :count +triangle-list-vertices-block-size+))
   (triangle-list-vertices-fill-pointer :accessor triangle-list-vertices-fill-pointer :initform 0)
   (triangle-list-vertices-allocated-size :accessor triangle-list-vertices-allocated-size :initform +triangle-list-vertices-block-size+)
   (triangle-list-indices :accessor triangle-list-indices :initform (foreign-alloc :unsigned-short :count +triangle-list-indices-block-size+))
   (triangle-list-indices-fill-pointer :accessor triangle-list-indices-fill-pointer :initform 0)
   (triangle-list-indices-allocated-size :accessor triangle-list-indices-allocated-size :initform +triangle-list-indices-block-size+)
   (triangle-list-commands :accessor triangle-list-commands :initform (make-array 1000 :adjustable t :fill-pointer 0))

   (triangle-strip-vertices :accessor triangle-strip-vertices :initform (foreign-alloc :float :count +triangle-strip-vertices-block-size+))
   (triangle-strip-vertices-fill-pointer :accessor triangle-strip-vertices-fill-pointer :initform 0)
   (triangle-strip-vertices-allocated-size :accessor triangle-strip-vertices-allocated-size :initform +triangle-strip-vertices-block-size+)
   (triangle-strip-indices :accessor triangle-strip-indices :initform (foreign-alloc :unsigned-short :count +triangle-strip-indices-block-size+))
   (triangle-strip-indices-fill-pointer :accessor triangle-strip-indices-fill-pointer :initform 0)
   (triangle-strip-indices-allocated-size :accessor triangle-strip-indices-allocated-size :initform +triangle-strip-indices-block-size+)
   (triangle-strip-commands :accessor triangle-strip-commands :initform (make-array 1000 :adjustable t :fill-pointer 0))))

(defclass dynamic-database (dynamic-database-mixin)
  ())

(defclass static-database (dynamic-database-mixin)
  ((entities :initform (make-array 1000 :fill-pointer 0 :adjustable t) :reader database-entities)
   (needs-regen? :initform t :accessor needs-regen?)))

(defclass editor ()
  ((mode :accessor editor-mode :initform nil)
   (selection-stack :accessor selection-stack :initform nil)
   (static-database :initform (make-instance 'static-database) :reader static-database)
   (dynamic-database :initform (make-instance 'dynamic-database) :reader dynamic-database)))

;; need foreign memory for point vertices
;; need fill pointer for point vertices

;; need foreign memory for line list vertices
;; need foreign memory for line list indices
;; need fill pointer for line list vertices
;; need fill pointer for line list indices

;; need foreign memory for line strip vertices
;; need foreign memory for line strip indices
;; need fill pointer for line strip vertices
;; need fill pointer for line strip indices

;; need foreign memory for triangle list vertices
;; need foreign memory for triangle list indices
;; need fill pointer for triangle list vertices
;; need fill pointer for triangle list indices

;; need foreign memory for triangle strip vertices
;; need foreign memory for triangle strip indices
;; need fill pointer for triangle strip vertices
;; need fill pointer for tirangle list vertices

;; why? cannot vector push extend simple array.



(defcstruct 2DVertex
  (position (:struct vec2)))

(defcstruct 3DVertex2
  (position (:struct vec3)))

(defcstruct 2DPoint
  (vertex (:struct 2DVertex))
  (color (:array :float 4)))

(defcstruct 3DPoint
  (vertex (:struct 3DVertex2))
  (color (:array :float 4)))

(defun append-point-vertex (database point color)
  (with-slots (point-vertices
	       point-vertices-fill-pointer
	       point-vertices-allocated-size) database
    (flet ((add ()
	     (setf (mem-aref point-vertices :float point-vertices-fill-pointer) (vx point)
		   (mem-aref point-vertices :float (1+ point-vertices-fill-pointer)) (vy point)
		   (mem-aref point-vertices :float (+ 2 point-vertices-fill-pointer)) (aref color 0)
		   (mem-aref point-vertices :float (+ 3 point-vertices-fill-pointer)) (aref color 1)
		   (mem-aref point-vertices :float (+ 4 point-vertices-fill-pointer)) (aref color 2)
		   (mem-aref point-vertices :float (+ 4 point-vertices-fill-pointer)) (aref color 3))
	     (incf point-vertices-fill-pointer 6)))
    (if (< point-vertices-fill-pointer point-vertices-allocated-size)
	(add)
	(if (eq point-vertices-fill-pointer point-vertices-allocated-size)
	    (let ((new-array (foreign-alloc :float :count (incf point-vertices-allocated-size +point-vertices-block-size+)))
		  (old-array point-vertices))
	      (memcpy new-array point-vertices (* point-vertices-fill-pointer (foreign-type-size :float)))
	      (setf point-vertices new-array)
	      (foreign-free old-array)
	      (add))
	    (error "invalid indices (3)")))))
    (values))

(defun append-line-list-command (database command)
  (vector-push-extend command (line-list-commands database))
  (values))

;; need to have functions to append entities to dynamic-entity-database
;; need to have functionality to mmap memory into buffers
;; need to have rendererers to draw buffers, including shaders
;; need to have functionality to reset database
	
(defun append-line-strip-vertex (database vertex)
  (with-slots (line-strip-vertices
	       line-strip-vertices-fill-pointer
	       line-strip-vertices-allocated-size) database
    (flet ((add ()
	     (setf (mem-aref line-strip-vertices :float line-strip-vertices-fill-pointer) (vx vertex)
		   (mem-aref line-strip-vertices :float (1+ line-strip-vertices-fill-pointer)) (vy vertex)
		   (mem-aref line-strip-vertices :float (+ 2 line-strip-vertices-fill-pointer)) (vz vertex))
	     (incf line-strip-vertices-fill-pointer 3)))
      (if (< line-strip-vertices-fill-pointer line-strip-vertices-allocated-size)
	  (add)
	  (if (eq line-strip-vertices-fill-pointer line-strip-vertices-allocated-size)
	      (let ((new-array (foreign-alloc :float :count (incf line-strip-vertices-allocated-size +line-strip-vertices-block-size+)))
		    (old-array line-strip-vertices))
		(memcpy new-array line-strip-vertices (* line-strip-vertices-fill-pointer (foreign-type-size :float)))
		(setf line-strip-vertices new-array)
		(foreign-free old-array)
		(add))
	      (error "invalid indices")))))
  (values))

(defun append-line-strip-index (database index)
  (with-slots (line-strip-indices
	       line-strip-indices-fill-pointer
	       line-strip-indices-allocated-size) database
    (flet ((add ()
	     (setf (mem-aref line-strip-indices :unsigned-short line-strip-indices-fill-pointer) index)
	     (incf line-strip-indices-fill-pointer)))
      
      (if (< line-strip-indices-fill-pointer line-strip-indices-allocated-size)
	  (add)
	  (if (eq line-strip-indices-fill-pointer line-strip-indices-allocated-size)
	      (let ((new-array (foreign-alloc :unsigned-short :count (incf line-strip-indices-allocated-size +line-strip-indices-block-size+)))
		    (old-array line-strip-indices))
		(memcpy new-array line-strip-indices (* line-strip-indices-fill-pointer (foreign-type-size :unsigned-short)))
		(setf line-strip-indices new-array)
		(foreign-free old-array)
		(add))
	      (error "invalid indices (2)")))
      (values))))

(defun append-line-strip-command (database &key index-count first-index vertex-offset color)
  (vector-push-extend (make-draw-indexed-cmd
		       :index-count index-count
		       :first-index first-index
		       :vertex-offset vertex-offset
		       :color color)
		      (line-strip-commands database))
  (values))

(defun append-circle (database center radius &optional (color (make-array 4 :element-type 'single-float)))
  (sb-thread:with-mutex ((slot-value database 'db-lock))
    (let* ((first-index (line-strip-indices-fill-pointer database))
	   (vertex-offset (/ (line-strip-vertices-fill-pointer database) 2)))

      (append-point-vertex database center color)
      
      (loop for i from 0 for theta from 0 below (* 2 pi) by 0.01
	 
	 do (append-line-strip-vertex database (vec3 (convert-to-single-float (+ (vx center) (* radius (cos theta))))
						     (convert-to-single-float (+ (vy center) (* radius (sin theta))))
						     0.0d0))
	   (append-line-strip-index database i)
	 finally
	   
	   (append-line-strip-index database 0)	   
	   (append-line-strip-command database :index-count (1+ i)
				      :first-index first-index
				      :vertex-offset vertex-offset
				      :color color))))
  
  (values))

(defun append-line (database point1 point2 &optional (color (make-array 4 :element-type 'single-float)))
  (sb-thread:with-mutex ((slot-value database 'db-lock))
    (let* ((first-index (line-strip-indices-fill-pointer database))
	   (vertex-offset (/ (line-strip-vertices-fill-pointer database) 3)))

      (append-line-strip-vertex database point1)
      (append-line-strip-index database 0)
      (append-line-strip-vertex database point2)
      (append-line-strip-index database 1)
      (append-line-strip-command database :index-count 2
				 :first-index first-index
				 :vertex-offset vertex-offset
				 :color color))))

(defun append-geom2d-curve (database curve &optional (color (make-array 4 :element-type 'single-float)))
  (sb-thread:with-mutex ((slot-value database 'db-lock))
    (let* ((first-index (line-strip-indices-fill-pointer database))
	   (vertex-offset (/ (line-strip-vertices-fill-pointer database) 3))
	   (adaptor (make-instance 'oc::Geom2d-Adaptor-Curve :C curve))
	   (cpnts (make-instance 'oc::CPnts-Uniform-Deflection
				 :C adaptor :deflection 0.0005d0 :resolution 0.01d0 :WithControl nil)))
      (loop for i from 0 while (oc::more-p cpnts)
	 do (let ((pnt (oc::get-point cpnts)))
	      (append-line-strip-vertex database (vec3 (convert-to-single-float (oc::x pnt)) (convert-to-single-float (oc::y pnt))
						       0.0d0))
	      (append-line-strip-index database i)
	      (oc::next cpnts))
	 finally (append-line-strip-command database :index-count i
					    :first-index first-index
					    :vertex-offset vertex-offset
					    :color color)))))

(defun convert-to-single-float (number)
  (when (< (abs number) least-positive-single-float)
    (setq number 0.0f0))
  (when (> (abs number) most-positive-single-float)
    (setq number (if (minusp number) most-negative-single-float most-positive-single-float)))
  (coerce number 'single-float))

(defun add-entity (static-database entity)
  (vector-push-extend entity (database-entities static-database))
  (typecase entity
    (oc::geom2d-curve (append-geom2d-curve static-database entity))
    (oc::geom-curve (append-geom-curve static-database entity))))

(defun remove-entity (static-database entity)
  (let ((entities (database-entities static-database)))
    (loop for i from 0 below (fill-pointer entities)
       do (when (oc:equal? (aref entities i) entity)
	    (setf (aref entities i) nil)
	    (setf (needs-regen? static-database) t)
	    (return t)))))

(defun regen (static-database)
  (setf (point-vertices-fill-pointer static-database) 0
	(line-strip-vertices-fill-pointer static-database) 0)
  (compact-entities static-database)
  (loop for entity across (database-entities static-database)
     do (typecase entity
	  (oc::geom2d-curve (append-geom2d-curve static-database entity))))
  t)

(defun compact-entities (static-database)
  (compact-entities-1 (database-entities static-database)))

;; get rid of compact entities with something that copies the array
;; this is most certainly a point of failure

(defun compact-entities-1 (array &optional (low 0) (high (1- (fill-pointer array))))
  (labels ((next-empty-index (array low high)
	     (loop for i from low to high
		when (null (aref array i))
		do (return i)
		finally (return nil)))
	   
	   (next-full-index (array low high)
	     (loop for i from high downto low
		when (aref array i)
		do (return i)
		finally (return nil))))

    (cond ((eq (fill-pointer array) 0) (return-from compact-entities-1 t))
	  ((eq low high) (setf (fill-pointer array) (1+ high)))
	  ((< low high) (let ((empty-index (next-empty-index array low high)))
			  (if empty-index ;; array is not compacted
			      (let ((full-index (next-full-index array (1+ empty-index) high)))
				(if full-index ;; there is an entity to move
				    (progn
				      (setf (aref array empty-index) (aref array full-index) ;; move it
					    (aref array full-index) nil)
				      (if (>= (1+ empty-index) (1- full-index))
					  (setf (fill-pointer array) (1+ empty-index))
					  ;; only scan range between last hole and last full, exclusive
					  (compact-entities-1 array (1+ empty-index) (1- full-index))))
				    (progn
				      (setf (fill-pointer array) (1- empty-index))
				      array)))
			      (progn (setf (fill-pointer array) (1+ high))
				     array))))
	  ((> low high) (error "unexpected condition ~S ~S" low high)))
    array))

(defclass colored-line-strip-renderer (renderer-mixin)
  ((frame-count :initarg :frame-count :reader frame-count)))

(defclass colored-point-vertex-renderer (renderer-mixin)
  ((frame-count :initarg :frame-count :reader frame-count)))



(defun maybe-init-frame-resources (editor device frame-count frame-index)
  (maybe-init-frame-resource (slot-value editor 'dynamic-database) device frame-count frame-index)
  (maybe-init-frame-resource (slot-value editor 'static-database) device frame-count frame-index))

(defun maybe-init-frame-resource (database device frame-count frame-index)
  (let* ((frame-resources (or (frame-resources database)
			      (setf (frame-resources database) (make-array frame-count :initial-element nil))))
	 (frame-resource (or (aref frame-resources frame-index)
			     (setf (aref frame-resources frame-index) (make-instance 'database-frame-resource)))))
    (maybe-init-buffers database frame-resource device)
    frame-resource))

(defmethod maybe-init-buffers ((database static-database) frame-resource device)
  (when (needs-regen? database)
    (regen database)
    (setf (needs-regen? database) nil))
  (call-next-method))

(defmethod maybe-init-buffers ((database dynamic-database-mixin) frame-resource device)
  (let ((vertex-size (* (slot-value database 'line-strip-vertices-fill-pointer) (foreign-type-size :float)))
	(vertex-buffer (slot-value frame-resource 'line-strip-vertex-buffer))
	(vertex-buffer-memory (slot-value frame-resource 'line-strip-vertex-buffer-memory))
	(vertex-buffer-size (slot-value frame-resource 'line-strip-vertex-buffer-memory-size))
	(alignment 256))
    
    (when (or (null vertex-buffer) (< vertex-buffer-size vertex-size))
      (when vertex-buffer
	(vk:vkDestroyBuffer (h device) (h vertex-buffer) (h (allocator vertex-buffer))))
      (when vertex-buffer-memory
	(vk:vkFreeMemory (h device) (h vertex-buffer-memory) (h (allocator vertex-buffer-memory))))
      (let ((new-vertex-buffer-size (* (1+ (ceiling (/ (1- vertex-size) alignment))) alignment)))
	(setf (slot-value frame-resource 'line-strip-vertex-buffer)
	      (create-buffer-1 device new-vertex-buffer-size vk:VK_BUFFER_USAGE_VERTEX_BUFFER_BIT :buffer-class 'vertex-buffer :allocator (allocator device))
	      (slot-value frame-resource 'line-strip-vertex-buffer-memory)
	      (allocate-buffer-memory device (slot-value frame-resource 'line-strip-vertex-buffer) vk:VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
				      :allocator (allocator device)))
	(bind-buffer-memory device (slot-value frame-resource 'line-strip-vertex-buffer) (slot-value frame-resource 'line-strip-vertex-buffer-memory))
	(setf (slot-value frame-resource 'line-strip-vertex-buffer-memory-size) new-vertex-buffer-size))
      (let ((new-index-buffer-size (* (1+ (ceiling (/ (1- (* (slot-value database 'line-strip-vertices-fill-pointer)
							     (foreign-type-size :float)))
						      alignment)))
				      alignment)))
	(setf (slot-value frame-resource 'line-strip-index-buffer)
	      (create-buffer-1 device new-index-buffer-size vk:VK_BUFFER_USAGE_INDEX_BUFFER_BIT :buffer-class 'index-buffer :allocator (allocator device))
	      (slot-value frame-resource 'line-strip-index-buffer-memory)
	      (allocate-buffer-memory device (slot-value frame-resource 'line-strip-index-buffer) vk:VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
				      :allocator (allocator device)))
	(bind-buffer-memory device (slot-value frame-resource 'line-strip-index-buffer) (slot-value frame-resource 'line-strip-index-buffer-memory))
	(setf (slot-value frame-resource 'line-strip-index-buffer-memory-size) new-index-buffer-size)))
    (let ((p-vtx-dst)
	  (p-idx-dst)
	  (vertex-buffer-memory (slot-value frame-resource 'line-strip-vertex-buffer-memory))
	  (vertex-size (slot-value frame-resource 'line-strip-vertex-buffer-memory-size))
	  (index-buffer-memory (slot-value frame-resource 'line-strip-index-buffer-memory))
	  (index-size (slot-value frame-resource 'line-strip-index-buffer-memory-size)))
      
      (with-foreign-objects ((pp-vtx-dst :pointer)
			     (pp-idx-dst :pointer))
	(vktk::check-vk-result (vk:vkMapMemory (h device) (h vertex-buffer-memory) 0 vertex-size 0 pp-vtx-dst))
	(setq p-vtx-dst (mem-aref pp-vtx-dst :pointer))
	(vktk::check-vk-result (vk:vkMapMemory (h device) (h index-buffer-memory) 0 index-size 0 pp-idx-dst))
	(setq p-idx-dst (mem-aref pp-idx-dst :pointer))

	(sb-thread:with-mutex ((slot-value database 'db-lock))
	  (memcpy p-vtx-dst (slot-value database 'line-strip-vertices) vertex-size)
	  (memcpy p-idx-dst (slot-value database 'line-strip-indices) index-size))

	(with-foreign-object (p-ranges '(:struct vk:VkMappedMemoryRange) 2)
	  (vktk::zero-struct (mem-aptr p-ranges '(:struct vk:VkMappedMemoryRange) 0)
			     '(:struct vk:VkMappedMemoryRange))
	  (vktk::zero-struct (mem-aptr p-ranges '(:struct vk:VkMappedMemoryRange) 1)
			     '(:struct vk:VkMappedMemoryRange))

	  (with-foreign-slots ((vk::sType
				vk::memory
				vk::size)
			       (mem-aptr p-ranges '(:struct vk:VkMappedMemoryRange) 0)
			       (:struct vk:VkMappedMemoryRange))
	    (setf vk::sType vk:VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
		  vk::memory (h (slot-value frame-resource 'line-strip-vertex-buffer-memory))
		  vk::size vktk::VK_WHOLE_SIZE))
	      
	  (with-foreign-slots ((vk::sType
				vk::memory
				vk::size)
			       (mem-aptr p-ranges '(:struct vk:VkMappedMemoryRange) 1)
			       (:struct vk:VkMappedMemoryRange))
	    (setf vk::sType vk:VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
		  vk::memory (h (slot-value frame-resource 'line-strip-index-buffer-memory))
		  vk::size vktk::VK_WHOLE_SIZE))

	  (vktk::check-vk-result (vk:vkFlushMappedMemoryRanges (h device) 2 p-ranges))

	  (vk:vkUnmapMemory (h device) (h vertex-buffer-memory))

	  (vk:vkUnmapMemory (h device) (h index-buffer-memory))))))

  (let* ((vertex-size (* (slot-value database 'point-vertices-fill-pointer) (foreign-type-size :float)))
	 (vertex-buffer (slot-value frame-resource 'point-buffer))
	 (vertex-buffer-memory (slot-value frame-resource 'point-buffer-memory))
	 (vertex-buffer-size (slot-value frame-resource 'point-buffer-memory-size))
	 (alignment 256))
    
    (when (or (null vertex-buffer) (< vertex-buffer-size vertex-size))
      (when vertex-buffer
	(vk:vkDestroyBuffer (h device) (h vertex-buffer) (h (allocator vertex-buffer))))
      (when vertex-buffer-memory
	(vk:vkFreeMemory (h device) (h vertex-buffer-memory) (h (allocator vertex-buffer-memory))))
	   
      (let ((new-vertex-buffer-size (* (1+ (ceiling (/ (1- (* (slot-value database 'point-vertices-fill-pointer)
							      (foreign-type-size :float)))
						       alignment)))
				       alignment)))
	(setf (slot-value frame-resource 'point-buffer)
	      (create-buffer-1 device new-vertex-buffer-size vk:VK_BUFFER_USAGE_VERTEX_BUFFER_BIT :buffer-class 'vertex-buffer :allocator (allocator device))
	      (slot-value frame-resource 'point-buffer-memory)
	      (allocate-buffer-memory device (slot-value frame-resource 'point-buffer) vk:VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
				      :allocator (allocator device)))
	(bind-buffer-memory device (slot-value frame-resource 'point-buffer) (slot-value frame-resource 'point-buffer-memory))
	(setf (slot-value frame-resource 'point-buffer-memory-size) new-vertex-buffer-size)))

    (let ((p-vtx-dst)
	  (vertex-buffer-memory (slot-value frame-resource 'point-buffer-memory))
	  (vertex-size (slot-value frame-resource 'point-buffer-memory-size)))
	      
      (with-foreign-objects ((pp-vtx-dst :pointer))
	(vktk::check-vk-result (vk:vkMapMemory (h device) (h vertex-buffer-memory) 0 vertex-size 0 pp-vtx-dst))
	(setq p-vtx-dst (mem-aref pp-vtx-dst :pointer))

	(sb-thread:with-mutex ((slot-value database 'db-lock))
	  (memcpy p-vtx-dst (slot-value database 'point-vertices) vertex-size))

	(with-foreign-object (p-range '(:struct vk:VkMappedMemoryRange))
	  (vktk::zero-struct p-range '(:struct vk:VkMappedMemoryRange))

	  (with-foreign-slots ((vk::sType
				vk::memory
				vk::size)
			       p-range (:struct vk:VkMappedMemoryRange))
		  
	    (setf vk::sType vk:VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
		  vk::memory (h (slot-value frame-resource 'point-buffer-memory))
		  vk::size vktk::VK_WHOLE_SIZE))
		
	  (vktk::check-vk-result (vk:vkFlushMappedMemoryRanges (h device) 1 p-range))

	  (vk:vkUnmapMemory (h device) (h vertex-buffer-memory)))))))
	   



(defmethod editor-render-draw-lists (editor (renderer colored-point-vertex-renderer) command-buffer frame-index fb-width fb-height model-matrix view-matrix projection-matrix)
  (with-slots (device frame-count) renderer
    (let* ((static-database (static-database editor))
	   (dynamic-database (dynamic-database editor))
	   (static-db-frame-resource (maybe-init-frame-resource static-database device frame-count frame-index))
	   (dynamic-db-frame-resource (maybe-init-frame-resource dynamic-database device frame-count frame-index)))
      (vk:vkCmdBindPipeline (h command-buffer) vk:VK_PIPELINE_BIND_POINT_GRAPHICS (h (pipeline renderer)))
      (update-vertex-shader-uniform-buffer renderer model-matrix view-matrix projection-matrix +clip-matrix+)
      (cmd-set-viewport command-buffer :width fb-width :height fb-height)
      (cmd-set-scissor command-buffer :width fb-width :height fb-height)
      (flet ((render-buffers (database frame-resource)
	       (with-foreign-object (p-vertex-buffers 'vk:VkBuffer)
		 (setf (mem-aref p-vertex-buffers 'vk:VkBuffer) (h (point-buffer frame-resource)))
		 (with-foreign-object (p-vertex-offset 'vk:VkDeviceSize)
		   (setf (mem-aref p-vertex-offset 'vk:VkDeviceSize) 0)
		   (vk:vkCmdBindVertexBuffers (h command-buffer) 0 1 p-vertex-buffers p-vertex-offset)))
	       (cmd-bind-descriptor-sets command-buffer (pipeline-layout renderer)
					 (list (descriptor-set renderer)))
      
	       (vk:vkCmdDraw (h command-buffer) (/ (slot-value database 'point-vertices-fill-pointer) 6) 1 0 0)))
	(unless (zerop (point-vertices-fill-pointer dynamic-database))
	  (render-buffers dynamic-database dynamic-db-frame-resource))
	(unless (zerop (point-vertices-fill-pointer static-database))
	  (render-buffers static-database static-db-frame-resource))))))

(defmethod editor-render-draw-lists (editor (renderer colored-line-strip-renderer) command-buffer frame-index fb-width fb-height model-matrix view-matrix projection-matrix)
  (with-slots (device frame-count) renderer
    (let* ((static-database (static-database editor))
	   (dynamic-database (dynamic-database editor))
	   (static-db-frame-resource (maybe-init-frame-resource static-database device frame-count frame-index))
	   (dynamic-db-frame-resource (maybe-init-frame-resource dynamic-database device frame-count frame-index)))
      (vk:vkCmdBindPipeline (h command-buffer) vk:VK_PIPELINE_BIND_POINT_GRAPHICS (h (pipeline renderer)))
      (update-vertex-shader-uniform-buffer renderer model-matrix view-matrix projection-matrix +clip-matrix+)
      (cmd-set-viewport command-buffer :width fb-width :height fb-height)
      (cmd-set-scissor command-buffer :width fb-width :height fb-height)
      (flet ((render-buffers (database frame-resource)
	       (with-foreign-object (p-vertex-buffers 'vk:VkBuffer)
		 (setf (mem-aref p-vertex-buffers 'vk:VkBuffer) (h (line-strip-vertex-buffer frame-resource)))
		 (with-foreign-object (p-vertex-offset 'vk:VkDeviceSize)
		   (setf (mem-aref p-vertex-offset 'vk:VkDeviceSize) 0)
		   (vk:vkCmdBindVertexBuffers (h command-buffer) 0 1 p-vertex-buffers p-vertex-offset)))
	       (cmd-bind-descriptor-sets command-buffer (pipeline-layout renderer)
					 (list (descriptor-set renderer)))
	       (vk:vkCmdBindIndexBuffer (h command-buffer) (h (line-strip-index-buffer frame-resource)) 0 vk:VK_INDEX_TYPE_UINT16)
	       (loop for cmd across (slot-value database 'line-strip-commands) for first-instance from 0
		  do (let ((color (vktk::draw-indexed-cmd-color cmd)))
		       (sb-sys:with-pinned-objects (color)
			 (vk:vkCmdPushConstants (h command-buffer) (h (pipeline-layout renderer))
						vk:VK_SHADER_STAGE_VERTEX_BIT
						0
						(* (foreign-type-size :float) 4)
						(sb-sys:vector-sap color)))
		       (cmd-draw-indexed command-buffer cmd)))))
	(unless (zerop (line-strip-vertices-fill-pointer dynamic-database))
	  (render-buffers dynamic-database dynamic-db-frame-resource))
	(unless (zerop (line-strip-vertices-fill-pointer static-database))
	  (render-buffers static-database static-db-frame-resource))))))

(defmethod pipeline-topology ((renderer colored-line-strip-renderer))
  vk:VK_PRIMITIVE_TOPOLOGY_LINE_STRIP)

(defmethod pipeline-topology ((renderer colored-point-vertex-renderer))
  vk:VK_PRIMITIVE_TOPOLOGY_POINT_LIST)

(defmethod vertex-shader-file-name ((renderer colored-line-strip-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/color-lines.vert.spv"))

(defmethod vertex-shader-file-name ((renderer colored-point-vertex-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/color-points.vert.spv"))

(defmethod fragment-shader-file-name ((renderer colored-line-strip-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/lines.frag.spv"))

(defmethod fragment-shader-file-name ((renderer colored-point-vertex-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/lines.frag.spv"))

(defmethod make-vertex-input-attribute-descriptions ((renderer colored-line-strip-renderer))
  (list (make-instance 'vertex-input-attribute-description
		       :location 0
		       :format vk:VK_FORMAT_R32G32B32_SFLOAT
		       :offset 0)))

(defmethod make-vertex-input-attribute-descriptions ((renderer colored-point-vertex-renderer))
  (list (make-instance 'vertex-input-attribute-description
		       :location 0
		       :format vk:VK_FORMAT_R32G32B32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type renderer) 'vertex))
	(make-instance 'vertex-input-attribute-description
		       :location 1
		       :format vk:VK_FORMAT_R32G32B32A32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type renderer) 'color))))
	

(defmethod pipeline-vertex-type ((renderer colored-line-strip-renderer))
  '(:struct 3DVertex2))

(defmethod pipeline-vertex-type ((renderer colored-point-vertex-renderer))
  '(:struct 3DPoint))

(defmethod make-push-constant-ranges ((renderer colored-line-strip-renderer))
  (list (make-instance 'push-constant-range
		       :stage-flags vk:VK_SHADER_STAGE_VERTEX_BIT
		       :offset 0
		       :size (* 4 (foreign-type-size :float)))))





   


    
 


(defun ray-intersects-plane? (ray plane)
  (let* ((P0 (ray-origin ray))
	 (V (ray-direction ray))
	 (equation (plane-equation plane))
	 (N (vec3 (vx equation) (vy equation) (vz equation)))
	 (d (vw equation))
	 (denom (v. V N))
	 (numer (+ (v. P0 N) d)))
    (if (< (abs denom) +epsilon+)
	(values nil (signum numer))
	(- (/ numer denom)))))

(defun evaluate-ray (ray parameter)
  (v+ (ray-origin ray) (v* parameter (ray-direction ray))))

(defun point-in-front-of-plane? (point plane)
  (let ((equation (plane-equation plane)))
    (not (minusp (+ (v. point (vec3 (vx equation) (vy equation) (vz equation))) (vw equation))))))

(defun ray-frustum-intersection (ray frustum)
  (let ((planes (list (frustum-near-plane frustum)
		      (frustum-far-plane frustum)
		      (frustum-bottom-plane frustum)
		      (frustum-top-plane frustum)
		      (frustum-left-plane frustum)
		      (frustum-right-plane frustum))))
    (let ((current-plane)
	  (parameter)
	  (inside? nil)
	  (current-point)
	  (i -1)
	  (found0)
	  (found1))

      (tagbody
       start
	 (incf i)
	 (when (eq i 6) (go exit))
	 
	 (setq current-plane (nth i planes))

	 (setq parameter (ray-intersects-plane? ray current-plane))
	 
	 (when parameter
	   (setq inside? t
		 current-point (evaluate-ray ray parameter))
	   (loop for plane in (remove current-plane planes)
	      do
		(unless (point-in-front-of-plane? current-point plane)
		  (setq inside? nil)
		  (go start))))

	 (when inside?
	   (if found0
	       (progn (setq found1 current-point)
		      (go exit))
	       (progn
		 (setq found0 current-point)
		 (setq inside? nil)
		 (go start))))

       exit)
      
      (when found1 (values found0 found1)))))

;; need a place where to keep the actual contents of the drawing
;; should be persistent and serializable
;; perhaps should cache 2d and 3d vertices to go into dynamic-entity-database
;; or perhaps vertices should be computed on the fly






;; how do we know if an entity has been edited resulting in a db-stale condition?
;; do we have to pass the database as an argument to every editing method?
;; do we put backpointers from the entities to the database?



;; need to code the machinery to convert quasi-static-database into something like the dynamic entity database
;; for each of:

;; Geom2d_AxisPlacement
;; Geom2d_BezierCurve
;; Geom2d_BoundedCurve
;; Geom2d_BSplineCurve
;; Geom2d_CartesianPoint
;; Geom2d_Circle
;; Geom2d_Direction
;; Geom2d_Ellipse
;; Geom2d_Line
;; Geom2d_OffsetCurve
;; Geom2d_Parabola
;; Geom2d_Point

;; Geom2d_TrimmedCurve


;; protocol
;; operation
;; regen: fill vertex, index buffers and cmds with data to draw everything in database
;; regen occurs when an entity is edited or deleted from database
;; when an append to database takes place, vertex, index buffers and cmds are appended to not regenerated
;; must know when an entity has changed
;; must know when an entity has been deleted
;; must know when an entity has been added

(defun intersect-ray-with-plane (ray plane)
  (let* ((plane-equation (plane-equation plane))
	 (ray-direction (ray-direction ray))
	 (ray-origin (ray-origin ray))
	 (param (- (/ (+ (* (vx ray-origin) (vx plane-equation))
			 (* (vy ray-origin) (vy plane-equation))
			 (* (vz ray-origin) (vz plane-equation))
			 (vw plane-equation))
		      (+ (* (vx ray-direction) (vx plane-equation))
			 (* (vy ray-direction) (vy plane-equation))
			 (* (vz ray-direction) (vz plane-equation)))))))
    (v+ ray-origin (v* param ray-direction))))
   

   

(defun intersect-bezier-curve-and-ray (curve ray &optional database #+NIL(tolerance 1.0d-3))
  (let* ((width1 0.5d0)
	 (width2 (* width1 width1))
	 (origin (ray-origin ray))
	 (direction (ray-direction ray))
	 (translation (gp:trsf))
	 (rotation (gp:trsf)))

    (intersect-ray-with-plane ray (make-plane :equation (vec4 0.0 0.0 1.0 0.0)))



    #+NIL(oc::_wrap_gp_Trsf_SetValues (gp:ptr translation)
		    1.0d0 0.0d0 0.0d0 (- (coerce (vx origin) 'double-float))
		    0.0d0 1.0d0 0.0d0 (- (coerce (vy origin) 'double-float))
		    0.0d0 0.0d0 1.0d0 (- (coerce (vz origin) 'double-float)))

    (setf (gp::translation translation) (gp:vec (- (coerce (vx origin) 'double-float))
						(- (coerce (vy origin) 'double-float))
						(- (coerce (vz origin) 'double-float))))

    (let ((d (coerce (sqrt (+ (* (vx direction) (vx direction))
			      (* (vz direction) (vz direction))))
		     'double-float)))

      (oc::_wrap_gp_Trsf_SetValues (gp:ptr rotation)
		      (/ (vz direction) d) 0.0d0 (- (/ (vx direction) d)) 0.0d0
		      (- (/ (* (vx direction) (vy direction)) d)) d
		      (- (/ (* (vy direction) (vz direction)) d)) 0.0d0
		      (coerce (vx direction) 'double-float)
		      (coerce (vy direction) 'double-float)
		      (coerce (vz direction) 'double-float) 0.0d0)

      (oc::multiply! rotation translation)

      (let ((curve
	     (make-instance 'oc::geom-adaptor-curve
			    :c (let ((c (oc::transformed curve rotation)))
				 ;;(oc::transform! c rotation)
				 c))))
	(converge curve 0.0d0 1.0d0 5 database)))))

(defun converge (curve u-start u-end depth database)
  (let ((bbox (make-instance 'oc::bnd-box)))
    (oc::bnd-lib-add-3d-curve-optimal curve 0.1d0 bbox :u1 u-start :u2 u-end)
    (multiple-value-bind (xmin ymin zmin xmax ymax zmax) (oc::bnd-box-bounds bbox)
      (declare (ignore zmin zmax))
      ;;(append-line database (vec3 xmin ymin 0) (vec3 xmax ymin 0))
      ;;(append-line database (vec3 xmax ymin 0) (vec3 xmax ymax 0))
      ;;(append-line database (vec3 xmax ymax 0) (vec3 xmin ymax 0))
      ;;(append-line database (vec3 xmin ymax 0) (vec3 xmin ymin 0))
      
      (cond ((or (<= 0.0d0 xmin) (>= 0.0d0 xmax)
		 (<= 0.0d0 ymin) (>= 0.0d0 ymax))
	     (return-from converge nil))
	    ((eq depth 0)
	     ;; maximum recursion depth has been reached
	     t
	     #+NIL
	     (let* ((pn (oc::evaluate-curve curve u-start))
		    (vpn (gp:make-vec :ptr (gp:ptr pn) :own pn))
		    (p0 (oc::evaluate-curve curve u-end))
		    (vp0 (gp:make-vec :ptr (gp:ptr p0) :own p0))
		    (dir (oc::subtracted vpn vp0))
		    (dP0 (oc::first-derivative curve u-start)))
	       (when (< (oc::dot dir dP0) 0.0d0)
		 (oc::multiply! dP0 -1))
	       (when (< (oc::dot dP0 vp0) 0.0d0)
		 (return-from converge nil))
	       (let ((dPn (oc::first-derivative curve u-end)))
		 (when (< (oc::dot dir dPn) 0.0d0)
		   (oc::multiply! dPn -1))
		 (when (< (oc::dot dPn vpn) 0.0d0)
		   (return-from converge nil))
		 (let ((w (+ (* (oc:x dir) (oc:x dir))
			     (* (oc:y dir) (oc:y dir)))))
		   (when (zerop w)
		     (return-from converge nil))
		   (setq w (- (/ (+ (* (oc:x vp0) (oc:x dir)) (* (oc:y vp0) (oc:y dir))) w)))
		   (setq w (max (min w 1) 0))
		   (let ((v (+ (* u-start (- 1 w)) (* u-end w))))
		     (let ((p (oc::evaluate-curve curve v)))
		       (when (or (>= (+ (* (oc:x p) (oc:x p)) (* (oc:y p) (oc:y p)))
				     0.025d0)
				 )
			 (return-from converge nil))))
		   t))))
	    (t (let ((u-m (/ (+ u-start u-end) 2.0d0)))
		 (return-from converge (or (converge curve u-start u-m (1- depth) database)
					   (converge curve u-m u-end (1- depth) database)))))))))

;; define some modes

;;(:create :line :from (accept 'point) :to (accept 'point))

;; what can be accepted?
;; point
;; line [segment]
;; arc
;; curve

;; how can they be accepted
;; point: crosshairs select coordinate, end point, mid point, center point, quad, at parameter on curve, control point, tangent, origin, dialog fill, intersection

