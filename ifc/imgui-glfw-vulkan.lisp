;; Copyright 2019 Andrew Kenneth Wolven <awolven@gmail.com>
;; 
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :vktk)

(defconstant VK_WHOLE_SIZE (1- (expt 2 64)))

(defclass imgui ()
  ((window :initform nil :accessor window)
   (imgui-time  :initform 0.0f0 :accessor imgui-time)
   (mouse-pressed :initform (make-array 3 :initial-element nil) :reader mouse-pressed)
   (mouse-wheel :initform 0.0f0 :accessor mouse-wheel)
   (allocator :initform +null-allocator+ :accessor allocator)
   (device :accessor device :initform nil)
   (render-pass :accessor render-pass :initform nil)
   (pipeline-cache :initform +null-pipeline-cache+ :accessor pipeline-cache)
   (descriptor-pool :accessor descriptor-pool :initform nil)
   (err :initform 0 :accessor imgui-err)
   (buffer-memory-alignment :initform 64 :accessor buffer-memory-alignment)
   (pipeline-create-flags :initform 0 :accessor pipeline-create-flags)
   (descriptor-set-layout :accessor descriptor-set-layout :initform nil)
   (pipeline-layout :accessor pipeline-layout :initform nil)
   (descriptor-set :accessor descriptor-set :initform nil)
   (pipeline :accessor pipeline :initform nil)
   (font-sampler :accessor font-sampler :initform nil)
   (font-memory :accessor font-memory :initform nil)
   (font-image :accessor font-image :initform nil)
   (font-view :accessor font-view :initform nil)
   (upload-buffer-memory :accessor upload-buffer-memory :initform nil)
   (upload-buffer :accessor upload-buffer :initform nil)
   (imgui-context :accessor imgui-context :initform nil)
   (frame-count :accessor frame-count)
   (frame-data :initform nil :accessor frame-data)))

(defclass imgui-frame-data ()
  ((vertex-buffer :initform nil :accessor vertex-buffer)
   (index-buffer :initform nil :accessor index-buffer)
   (vertex-buffer-size :initform nil :accessor vertex-buffer-size)
   (index-buffer-size :initform nil :accessor index-buffer-size)
   (vertex-buffer-memory :initform nil :accessor vertex-buffer-memory)
   (index-buffer-memory :initform nil :accessor index-buffer-memory)))

(defvar *imgui-vertex-shader-binary*
  (foreign-alloc
   :uint32
   :initial-contents
   (list #x07230203 #x00010000 #x00080001 #x0000002e #x00000000 #x00020011 #x00000001 #x0006000b 
	 #x00000001 #x4c534c47 #x6474732e #x3035342e #x00000000 #x0003000e #x00000000 #x00000001 
	 #x000a000f #x00000000 #x00000004 #x6e69616d #x00000000 #x0000000b #x0000000f #x00000015 
	 #x0000001b #x0000001c #x00030003 #x00000002 #x000001c2 #x00040005 #x00000004 #x6e69616d 
	 #x00000000 #x00030005 #x00000009 #x00000000 #x00050006 #x00000009 #x00000000 #x6f6c6f43 
	 #x00000072 #x00040006 #x00000009 #x00000001 #x00005655 #x00030005 #x0000000b #x0074754f 
	 #x00040005 #x0000000f #x6c6f4361 #x0000726f #x00030005 #x00000015 #x00565561 #x00060005 
	 #x00000019 #x505f6c67 #x65567265 #x78657472 #x00000000 #x00060006 #x00000019 #x00000000 
	 #x505f6c67 #x7469736f #x006e6f69 #x00030005 #x0000001b #x00000000 #x00040005 #x0000001c 
	 #x736f5061 #x00000000 #x00060005 #x0000001e #x73755075 #x6e6f4368 #x6e617473 #x00000074 
	 #x00050006 #x0000001e #x00000000 #x61635375 #x0000656c #x00060006 #x0000001e #x00000001 
	 #x61725475 #x616c736e #x00006574 #x00030005 #x00000020 #x00006370 #x00040047 #x0000000b 
	 #x0000001e #x00000000 #x00040047 #x0000000f #x0000001e #x00000002 #x00040047 #x00000015 
	 #x0000001e #x00000001 #x00050048 #x00000019 #x00000000 #x0000000b #x00000000 #x00030047 
	 #x00000019 #x00000002 #x00040047 #x0000001c #x0000001e #x00000000 #x00050048 #x0000001e 
	 #x00000000 #x00000023 #x00000000 #x00050048 #x0000001e #x00000001 #x00000023 #x00000008 
	 #x00030047 #x0000001e #x00000002 #x00020013 #x00000002 #x00030021 #x00000003 #x00000002 
	 #x00030016 #x00000006 #x00000020 #x00040017 #x00000007 #x00000006 #x00000004 #x00040017 
	 #x00000008 #x00000006 #x00000002 #x0004001e #x00000009 #x00000007 #x00000008 #x00040020 
	 #x0000000a #x00000003 #x00000009 #x0004003b #x0000000a #x0000000b #x00000003 #x00040015 
	 #x0000000c #x00000020 #x00000001 #x0004002b #x0000000c #x0000000d #x00000000 #x00040020 
	 #x0000000e #x00000001 #x00000007 #x0004003b #x0000000e #x0000000f #x00000001 #x00040020 
	 #x00000011 #x00000003 #x00000007 #x0004002b #x0000000c #x00000013 #x00000001 #x00040020 
	 #x00000014 #x00000001 #x00000008 #x0004003b #x00000014 #x00000015 #x00000001 #x00040020 
	 #x00000017 #x00000003 #x00000008 #x0003001e #x00000019 #x00000007 #x00040020 #x0000001a 
	 #x00000003 #x00000019 #x0004003b #x0000001a #x0000001b #x00000003 #x0004003b #x00000014 
	 #x0000001c #x00000001 #x0004001e #x0000001e #x00000008 #x00000008 #x00040020 #x0000001f 
	 #x00000009 #x0000001e #x0004003b #x0000001f #x00000020 #x00000009 #x00040020 #x00000021 
	 #x00000009 #x00000008 #x0004002b #x00000006 #x00000028 #x00000000 #x0004002b #x00000006 
	 #x00000029 #x3f800000 #x00050036 #x00000002 #x00000004 #x00000000 #x00000003 #x000200f8 
	 #x00000005 #x0004003d #x00000007 #x00000010 #x0000000f #x00050041 #x00000011 #x00000012 
	 #x0000000b #x0000000d #x0003003e #x00000012 #x00000010 #x0004003d #x00000008 #x00000016 
	 #x00000015 #x00050041 #x00000017 #x00000018 #x0000000b #x00000013 #x0003003e #x00000018 
	 #x00000016 #x0004003d #x00000008 #x0000001d #x0000001c #x00050041 #x00000021 #x00000022 
	 #x00000020 #x0000000d #x0004003d #x00000008 #x00000023 #x00000022 #x00050085 #x00000008 
	 #x00000024 #x0000001d #x00000023 #x00050041 #x00000021 #x00000025 #x00000020 #x00000013 
	 #x0004003d #x00000008 #x00000026 #x00000025 #x00050081 #x00000008 #x00000027 #x00000024 
	 #x00000026 #x00050051 #x00000006 #x0000002a #x00000027 #x00000000 #x00050051 #x00000006 
	 #x0000002b #x00000027 #x00000001 #x00070050 #x00000007 #x0000002c #x0000002a #x0000002b 
	 #x00000028 #x00000029 #x00050041 #x00000011 #x0000002d #x0000001b #x0000000d #x0003003e 
	 #x0000002d #x0000002c #x000100fd #x00010038)))

(defparameter *imgui-vertex-shader-binary-size* (* 324 4))
  

(defvar *imgui-fragment-shader-binary*
  (foreign-alloc
   :uint32
   :initial-contents
   (list #x07230203 #x00010000 #x00080001 #x0000001e #x00000000 #x00020011 #x00000001 #x0006000b 
	 #x00000001 #x4c534c47 #x6474732e #x3035342e #x00000000 #x0003000e #x00000000 #x00000001 
	 #x0007000f #x00000004 #x00000004 #x6e69616d #x00000000 #x00000009 #x0000000d #x00030010 
	 #x00000004 #x00000007 #x00030003 #x00000002 #x000001c2 #x00040005 #x00000004 #x6e69616d 
	 #x00000000 #x00040005 #x00000009 #x6c6f4366 #x0000726f #x00030005 #x0000000b #x00000000 
	 #x00050006 #x0000000b #x00000000 #x6f6c6f43 #x00000072 #x00040006 #x0000000b #x00000001 
	 #x00005655 #x00030005 #x0000000d #x00006e49 #x00050005 #x00000016 #x78655473 #x65727574 
	 #x00000000 #x00040047 #x00000009 #x0000001e #x00000000 #x00040047 #x0000000d #x0000001e 
	 #x00000000 #x00040047 #x00000016 #x00000022 #x00000000 #x00040047 #x00000016 #x00000021 
	 #x00000000 #x00020013 #x00000002 #x00030021 #x00000003 #x00000002 #x00030016 #x00000006 
	 #x00000020 #x00040017 #x00000007 #x00000006 #x00000004 #x00040020 #x00000008 #x00000003 
	 #x00000007 #x0004003b #x00000008 #x00000009 #x00000003 #x00040017 #x0000000a #x00000006 
	 #x00000002 #x0004001e #x0000000b #x00000007 #x0000000a #x00040020 #x0000000c #x00000001 
	 #x0000000b #x0004003b #x0000000c #x0000000d #x00000001 #x00040015 #x0000000e #x00000020 
	 #x00000001 #x0004002b #x0000000e #x0000000f #x00000000 #x00040020 #x00000010 #x00000001 
	 #x00000007 #x00090019 #x00000013 #x00000006 #x00000001 #x00000000 #x00000000 #x00000000 
	 #x00000001 #x00000000 #x0003001b #x00000014 #x00000013 #x00040020 #x00000015 #x00000000 
	 #x00000014 #x0004003b #x00000015 #x00000016 #x00000000 #x0004002b #x0000000e #x00000018 
	 #x00000001 #x00040020 #x00000019 #x00000001 #x0000000a #x00050036 #x00000002 #x00000004 
	 #x00000000 #x00000003 #x000200f8 #x00000005 #x00050041 #x00000010 #x00000011 #x0000000d 
	 #x0000000f #x0004003d #x00000007 #x00000012 #x00000011 #x0004003d #x00000014 #x00000017 
	 #x00000016 #x00050041 #x00000019 #x0000001a #x0000000d #x00000018 #x0004003d #x0000000a 
	 #x0000001b #x0000001a #x00050057 #x00000007 #x0000001c #x00000017 #x0000001b #x00050085 
	 #x00000007 #x0000001d #x00000012 #x0000001c #x0003003e #x00000009 #x0000001d #x000100fd 
	 #x00010038)))

(defparameter *imgui-fragment-shader-binary-size* (* 193 4))


(defun maybe-init-frame-data (app frame-count)
  (unless (frame-data app)
    (setf (frame-data app) (make-array frame-count :initial-element nil))))

(defun imgui-render-draw-lists (app draw-data command-buffer frame-index)

  (when (zerop (foreign-slot-value draw-data '(:struct ig::ImDrawData) 'ig::TotalVtxCount))
    (return-from imgui-render-draw-lists (values)))
  (with-slots (device allocator buffer-memory-alignment
		      descriptor-set pipeline-layout frame-data) app
    (maybe-init-frame-data app (frame-count app))
    (unless (elt frame-data frame-index)
      (setf (elt frame-data frame-index) (make-instance 'imgui-frame-data)))
    (with-slots (vertex-buffer
		 vertex-buffer-size
		 vertex-buffer-memory
		 index-buffer
		 index-buffer-size
		 index-buffer-memory) (elt frame-data frame-index)
		      
      (let ((io (ig:igGetIO)))
	;; create the vertex buffer:
	(let ((vertex-size (* (foreign-slot-value draw-data '(:struct ig::ImDrawData) 'ig::TotalVtxCount)
			      (foreign-type-size '(:struct ig::ImDrawVert))))
	      (index-size (* (foreign-slot-value draw-data  '(:struct ig::ImDrawData) 'ig::TotalIdxCount)
			     (foreign-type-size 'ig::ImDrawIdx))))
	  (when (or (not vertex-buffer) (< vertex-buffer-size vertex-size))
	    (when vertex-buffer
	      (vkDestroyBuffer (h device) (h vertex-buffer) (h allocator)))

	    (when vertex-buffer-memory
	      (vkFreeMemory (h device) (h vertex-buffer-memory) (h allocator)))

	    (let ((new-vertex-buffer-size (* (1+ (ceiling (/ (1- vertex-size) buffer-memory-alignment)))
					     buffer-memory-alignment)))
	      (setf vertex-buffer
		    (create-buffer-1 device new-vertex-buffer-size VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
				     :buffer-class 'vertex-buffer
				     :allocator allocator))
	      (setf vertex-buffer-memory
		    (allocate-buffer-memory device
					    vertex-buffer
					    VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
					    :allocator allocator))
	      (setf buffer-memory-alignment (max (alignment vertex-buffer-memory)
						 buffer-memory-alignment))
	      (bind-buffer-memory device
				  vertex-buffer
				  vertex-buffer-memory)
	      (setf vertex-buffer-size new-vertex-buffer-size)))

	  ;; create the index buffer:
	  (when (or (not index-buffer)
		    (< index-buffer-size index-size))
	    (when index-buffer
	      (vkDestroyBuffer (h device)
			       (h index-buffer)
			       (h allocator)))

	    (when index-buffer-memory
	      (vkFreeMemory (h device)
			    (h index-buffer-memory)
			    (h allocator)))

	    (let ((new-index-buffer-size (* (1+ (ceiling (/ (1- index-size) buffer-memory-alignment)))
					    buffer-memory-alignment)))
	      (setf index-buffer
		    (create-buffer-1 device new-index-buffer-size VK_BUFFER_USAGE_INDEX_BUFFER_BIT
				     :buffer-class 'index-buffer
				     :allocator allocator))
	      (setf index-buffer-memory
		    (allocate-buffer-memory device
					    index-buffer
					    VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
					    :allocator allocator))
	      (setf buffer-memory-alignment (max (alignment
						  index-buffer-memory)
						 buffer-memory-alignment))
	      (bind-buffer-memory device
				  index-buffer
				  index-buffer-memory)
	      (setf index-buffer-size new-index-buffer-size)))

	  (let ((p-vtx-dst) (p-idx-dst))
	    (with-foreign-objects ((pp-vtx-dst :pointer)
				   (pp-idx-dst :pointer))

	      (check-vk-result (vkMapMemory (h device)
					    (h vertex-buffer-memory)
					    0 vertex-size 0 pp-vtx-dst))
	      (setf p-vtx-dst (mem-aref pp-vtx-dst :pointer))

	      (check-vk-result (vkMapMemory (h device)
					    (h index-buffer-memory)
					    0 index-size 0 pp-idx-dst))
	      (setf p-idx-dst (mem-aref pp-idx-dst :pointer))

	      (loop for n from 0 below (foreign-slot-value draw-data '(:struct ig::ImDrawData) 'ig::CmdListsCount)
		 do (let* ((p-cmd-list
			    (mem-aref (foreign-slot-value draw-data '(:struct ig::ImDrawData) 'ig::CmdLists)
				      :pointer n))
			   (p-vtx-buffer
			    (foreign-slot-pointer p-cmd-list '(:struct ig::ImDrawList) 'ig::VtxBuffer))
			   (p-idx-buffer
			    (foreign-slot-pointer p-cmd-list '(:struct ig::ImDrawList) 'ig::IdxBuffer))
			   (p-vtx-data (ImVector_ImDrawVert_begin p-vtx-buffer))
			   (p-idx-data (ImVector_ImDrawIdx_begin p-idx-buffer)))

		      (memcpy p-vtx-dst p-vtx-data
			      (ig::ImVector_ImDrawVert_size_in_bytes p-vtx-buffer))

		      (memcpy p-idx-dst p-idx-data
			      (ig::ImVector_ImDrawIdx_size_in_bytes p-idx-buffer))

		      (incf-pointer p-vtx-dst (ig::ImVector_ImDrawVert_size_in_bytes p-vtx-buffer))

		      (incf-pointer p-idx-dst (ig::ImVector_ImDrawIdx_size_in_bytes p-idx-buffer))))))
      
	  (with-foreign-object (p-ranges '(:struct VkMappedMemoryRange) 2)
	    (zero-struct (mem-aptr p-ranges '(:struct VkMappedMemoryRange) 0)
			 '(:struct VkMappedMemoryRange))
	    (zero-struct (mem-aptr p-ranges '(:struct VkMappedMemoryRange) 1)
			 '(:struct VkMappedMemoryRange))
	    (with-foreign-slots ((vk::sType
				  vk::memory
				  vk::size)
				 (mem-aptr p-ranges '(:struct VkMappedMemoryRange) 0)
				 (:struct VkMappedMemoryRange))
	      (setf vk::sType VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
		    vk::memory (h vertex-buffer-memory)
		    vk::size VK_WHOLE_SIZE))

	    (with-foreign-slots ((vk::sType
				  vk::memory
				  vk::size)
				 (mem-aptr p-ranges '(:struct VkMappedMemoryRange) 1)
				 (:struct VkMappedMemoryRange))
	      (setf vk::sType VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
		    vk::memory (h index-buffer-memory)
		    vk::size VK_WHOLE_SIZE))

	    (check-vk-result (vkFlushMappedMemoryRanges (h device) 2 p-ranges))

	    (vkUnmapMemory (h device) (h vertex-buffer-memory))

	    (vkUnmapMemory (h device) (h index-buffer-memory)))

	  ;; bind pipeline and descriptor sets:
	  (vkCmdBindPipeline (h command-buffer) VK_PIPELINE_BIND_POINT_GRAPHICS (h (pipeline app)))
	
	  (with-foreign-object (p-desc-set 'VkDescriptorSet)
	    (setf (mem-aref p-desc-set 'VkDescriptorSet 0) (h descriptor-set))
	    (vkCmdBindDescriptorSets (h command-buffer) VK_PIPELINE_BIND_POINT_GRAPHICS
				     (h pipeline-layout) 0 1 p-desc-set 0 +nullptr+))
	  ;; bind Vertex and Index Buffer:
	  (with-foreign-object (p-vertex-buffers 'VkBuffer)
	    (setf (mem-aref p-vertex-buffers 'VkBuffer) (h vertex-buffer))
	    (with-foreign-object (p-vertex-offset 'VkDeviceSize)
	      (setf (mem-aref p-vertex-offset 'VkDeviceSize) 0)
	      (vkCmdBindVertexBuffers (h command-buffer) 0 1 p-vertex-buffers p-vertex-offset)))
	  (vkCmdBindIndexBuffer (h command-buffer) (h index-buffer)
				0 VK_INDEX_TYPE_UINT16)
	  ;; Setup Viewport
	  (with-vk-struct (p-viewport VkViewport)
	    (with-foreign-slots ((vk::x
				  vk::y
				  vk::width
				  vk::height
				  vk::minDepth
				  vk::maxDepth)
				 p-viewport
				 (:struct VkViewport))
	      (let* ((p-display-size (foreign-slot-pointer draw-data '(:struct ig::ImDrawData) 'ig::DisplaySize))
		     (p-display-pos (foreign-slot-pointer draw-data '(:struct ig::ImDrawData) 'ig::DisplayPos))
		     (display-pos-x (foreign-slot-value p-display-pos '(:struct ig::ImVec2) 'ig::x))
		     (display-pos-y (foreign-slot-value p-display-pos '(:struct ig::ImVec2) 'ig::y))
		     (display-size-x (foreign-slot-value p-display-size '(:struct ig::ImVec2) 'ig::x))
		     (display-size-y (foreign-slot-value p-display-size '(:struct ig::ImVec2) 'ig::y)))

		(setf vk::x 0.0f0
		      vk::y 0.0f0
		      vk::width (* #+darwin 2.0f0 #+(or windows linux) 1.0f0 display-size-x)
		      vk::height (* #+darwin 2.0f0 #+(or windows linux) 1.0f0 display-size-y)
		      vk::minDepth 0.0f0
		      vk::maxDepth 1.0f0)
		(vkCmdSetViewport (h command-buffer) 0 1 p-viewport)

		;; Setup scale and translation:
		(with-foreign-object (p-scale :float 2)
		  (setf (mem-aref p-scale :float 0)
			(/ 2.0f0 display-size-x)
			(mem-aref p-scale :float 1)
			(/ 2.0f0 display-size-y))

		  (with-foreign-object (p-translate :float 2)
		    (setf (mem-aref p-translate :float 0) (- -1.0f0 (* display-pos-x (mem-aref p-scale :float 0)))
			  (mem-aref p-translate :float 1) (- -1.0f0 (* display-pos-y (mem-aref p-scale :float 0))))

		    (vkCmdPushConstants (h command-buffer) (h pipeline-layout)
					VK_SHADER_STAGE_VERTEX_BIT
					(* (foreign-type-size :float) 0)
					(* (foreign-type-size :float) 2)
					p-scale)

		    (vkCmdPushConstants (h command-buffer) (h pipeline-layout)
					VK_SHADER_STAGE_VERTEX_BIT
					(* (foreign-type-size :float) 2)
					(* (foreign-type-size :float) 2)
					p-translate)))))))
	;; Render the command lists:
	(let ((vtx-offset 0)
	      (idx-offset 0))

	  (loop for n from 0 below (foreign-slot-value draw-data '(:struct ig::ImDrawData) 'ig::CmdListsCount)
	     do
	       (let ((p-cmd-list
		      (mem-aref (foreign-slot-value draw-data '(:struct ig::ImDrawData) 'ig::CmdLists)
				:pointer n)))
		 (loop for cmd-i from 0 below
		      (foreign-slot-value (foreign-slot-pointer p-cmd-list
								'(:struct ig::ImDrawList) 'ig::CmdBuffer)
					  '(:struct ig::ImVector_ImDrawCmd) 'ig::Size)
		    
		    do (let ((p-cmd
			      (mem-aptr
			       (foreign-slot-value
				(foreign-slot-pointer p-cmd-list '(:struct ig::ImDrawList) 'ig::CmdBuffer)
				'(:struct ig::ImVector_ImDrawCmd) 'ig::Data)
			       '(:struct ig::ImDrawCmd) cmd-i)))

			 (with-vk-struct (p-scissor VkRect2D)
			   (let ((p-offset
				  (foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::offset))
				 (p-extent
				  (foreign-slot-pointer p-scissor '(:struct VkRect2D) 'vk::extent)))
			       
			     (with-foreign-slots ((vk::x vk::y) p-offset (:struct VkOffset2D))
			       (with-foreign-slots ((vk::width vk::height) p-extent (:struct VkExtent2D))
				   
				 (let ((p-clip-rect (foreign-slot-pointer p-cmd '(:struct ig::ImDrawCmd) 'ig::ClipRect)))
				     
				   (setf vk::x (* (max
						   (round (foreign-slot-value p-clip-rect '(:struct ig::ImVec4) 'ig::x))
						   0) #+darwin 2 #+(or windows linux) 1)
					 vk::y (* (max
						   (round (foreign-slot-value p-clip-rect '(:struct ig::ImVec4) 'ig::y))
						   0) #+darwin 2 #+(or windows linux) 1)
					 vk::width (* (round
						       (- (foreign-slot-value p-clip-rect '(:struct ig::ImVec4) 'ig::z)
							  (foreign-slot-value p-clip-rect '(:struct ig::ImVec4) 'ig::x)))
						      #+darwin 2 #+(or windows linux)1)
					 vk::height (* (round
							(- (foreign-slot-value p-clip-rect '(:struct ig::ImVec4) 'ig::w)
							   (foreign-slot-value p-clip-rect '(:struct ig::ImVec4) 'ig::y)))
						       #+darwin 2 #+(or windows linux) 1)))

				 (vkCmdSetScissor (h command-buffer) 0 1 p-scissor)

				 (vkCmdDrawIndexed (h command-buffer)
						   (foreign-slot-value p-cmd '(:struct ig::ImDrawCmd) 'ig::ElemCount)
						   1 (+ (foreign-slot-value p-cmd '(:struct ig::ImDrawCmd) 'ig::IdxOffset) idx-offset) (+ (foreign-slot-value p-cmd '(:struct ig::ImDrawCmd) 'ig::VtxOffset) vtx-offset) 0)))))
			 #+NIL
			 (if (not (null-pointer-p
				   (foreign-slot-value p-cmd '(:struct ig::ImDrawCmd) 'ig::UserCallback)))
			     nil
			     #+IGNORE
			     (foreign-funcall-pointer
			      (prog1 (print (foreign-slot-value p-cmd '(:struct ig::ImDrawCmd) 'ig::UserCallback))
				(finish-output))
			      (:convention :cdecl)
			      :pointer p-cmd-list :pointer p-cmd :void)
			
			     )
			 #+NIL
			 (incf idx-offset (foreign-slot-value p-cmd '(:struct ig::ImDrawCmd) 'ig::ElemCount))))
		 (incf idx-offset
		       (ig::ImVector_ImDrawIdx_size
			(foreign-slot-pointer p-cmd-list '(:struct ig::ImDrawList) 'ig::IdxBuffer))
		       )
		 (incf vtx-offset
		       (ig::ImVector_ImDrawVert_size
			(foreign-slot-pointer p-cmd-list '(:struct ig::ImDrawList) 'ig::VtxBuffer))
		       ))))))))

(defclass sampler (handle-mixin logical-device-mixin)
  ())

(defun create-sampler (device &key (allocator +null-allocator+))
  (with-vk-struct (p-info VkSamplerCreateInfo)
    (with-foreign-slots ((vk::magFilter
			  vk::minFilter
			  vk::mipmapMode
			  vk::addressModeU
			  vk::addressModeV
			  vk::addressModeW
			  vk::minLod
			  vk::maxLod
			  vk::maxAnisotropy)
			 p-info
			 (:struct VkSamplerCreateInfo))
      (setf vk::magFilter VK_FILTER_LINEAR
	    vk::minFilter VK_FILTER_LINEAR
	    vk::mipmapMode VK_SAMPLER_MIPMAP_MODE_LINEAR
	    vk::addressModeU VK_SAMPLER_ADDRESS_MODE_REPEAT
	    vk::addressModeV VK_SAMPLER_ADDRESS_MODE_REPEAT
	    vk::addressModeW VK_SAMPLER_ADDRESS_MODE_REPEAT
	    vk::minLod -1000.0f0
	    vk::maxLod 1000.0f0
	    vk::maxAnisotropy 1.0f0)
      (with-foreign-object (p-sampler 'VkSampler)
	(check-vk-result (vkCreateSampler (h device) p-info (h allocator) p-sampler))
	(make-instance 'sampler
		       :handle (mem-aref p-sampler 'VkSampler)
		       :device device)))))
	    

(defun imgui-create-device-objects (app &key (allocator +null-allocator+))
  (multiple-value-bind (width height) (get-framebuffer-size (window app))
    (let ((vtx-shader (create-shader-module (device app) *imgui-vertex-shader-binary*
					    *imgui-vertex-shader-binary-size*))
	  (frag-shader (create-shader-module (device app) *imgui-fragment-shader-binary*
					    *imgui-fragment-shader-binary-size*)))
      (setf (font-sampler app) (create-sampler (device app) :allocator allocator))
      (setf (descriptor-set-layout app)
	    (create-descriptor-set-layout
	     (device app) :allocator allocator
	     :bindings
	     (list (make-instance 'descriptor-set-layout-binding
				  :type VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
				  :count 1
				  :flags VK_SHADER_STAGE_FRAGMENT_BIT
				  :samplers (list (font-sampler app))))))
      (setf (descriptor-set app)
	    (allocate-descriptor-set (device app) (list (descriptor-set-layout app)) (descriptor-pool app)))
      (setf (pipeline-layout app)
	    (create-pipeline-layout (device app) (list (descriptor-set-layout app)) :allocator allocator
				    :push-constant-ranges
				    (list (make-instance 'push-constant-range
							 :stage-flags VK_SHADER_STAGE_VERTEX_BIT
							 :offset 0
							 :size (* 4 (foreign-type-size :float))))))
      (setf (pipeline app) (create-graphics-pipeline (device app) (pipeline-cache app) (pipeline-layout app)
					    (render-pass app) 1 width height
					    vtx-shader frag-shader :allocator allocator
					    :vertex-type '(:struct ig::ImDrawVert)
					    :vertex-input-attribute-descriptions
					    (list (make-instance 'vertex-input-attribute-description
								 :location 0
								 :binding 0
								 :format VK_FORMAT_R32G32_SFLOAT
								 :offset (foreign-slot-offset
									  '(:struct ig::ImDrawVert) 'ig::pos))
						  (make-instance 'vertex-input-attribute-description
								 :location 1
								 :binding 0
								 :format VK_FORMAT_R32G32_SFLOAT
								 :offset (foreign-slot-offset
									  '(:struct ig::ImDrawVert) 'ig::uv))
						  (make-instance 'vertex-input-attribute-description
								 :location 2
								 :binding 0
								 :format VK_FORMAT_R8G8B8A8_UNORM
								 :offset (foreign-slot-offset
									  '(:struct ig::ImDrawVert) 'ig::col)))
					    :min-sample-shading 0.0f0
					    ;;:front-face VK_FRONT_FACE_CLOCKWISE
					    :depth-test-enable VK_FALSE
					    :depth-write-enable VK_FALSE
					    :depth-compare-op VK_COMPARE_OP_NEVER
					    :logic-op VK_LOGIC_OP_CLEAR
					    ;;:polygon-mode VK_POLYGON_MODE_LINE
					    :blend-enable VK_TRUE :depth-clamp-enable VK_FALSE
					    :src-alpha-blend-factor VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA))
      (vkDestroyShaderModule (h (device app)) (h vtx-shader) (h allocator))
      (vkDestroyShaderModule (h (device app)) (h frag-shader) (h allocator))
      (values))))
	  
(defun imgui-invalidate-font-upload-objects (app)
  (when (upload-buffer app)
    (vkDestroyBuffer (h (device app)) (h (upload-buffer app)) (h (allocator app)))
    (setf (upload-buffer app) nil))
  (when (upload-buffer-memory app)
    (vkFreeMemory (h (device app)) (h (upload-buffer-memory app)) (h (allocator app)))
    (setf (upload-buffer-memory app) nil))
  (values))

(defun imgui-invalidate-device-objects (app)
  (imgui-invalidate-font-upload-objects app)

  (loop for frame-datum across (frame-data app) for i from 0
     do (when frame-datum
	  (with-slots (vertex-buffer
		       vertex-buffer-memory
		       index-buffer
		       index-buffer-memory) frame-datum
	    (when vertex-buffer
	      (vkDestroyBuffer (h (device app)) (h vertex-buffer) (h (allocator app)))
	      (setf vertex-buffer nil))
	    (when vertex-buffer-memory
	      (vkFreeMemory (h (device app)) (h vertex-buffer-memory) (h (allocator app)))
	      (setf vertex-buffer-memory nil))
	    (when index-buffer
	      (vkDestroyBuffer (h (device app)) (h index-buffer) (h (allocator app)))
	      (setf index-buffer nil))
	    (when index-buffer-memory
	      (vkFreeMemory (h (device app)) (h index-buffer-memory) (h (allocator app)))
	      (setf index-buffer-memory nil))
	    (setf (elt (frame-data app) i) nil))))
  

  (when (font-view app)
    (vkDestroyImageView (h (device app)) (h (font-view app)) (h (allocator app)))
    (setf (font-view app) nil))
  (when (font-image app)
    (vkDestroyImage (h (device app)) (h (font-image app)) (h (allocator app)))
    (setf (font-image app) nil))
  (when (font-memory app)
    (vkFreeMemory (h (device app)) (h (font-memory app)) (h (allocator app)))
    (setf (font-memory app) nil))
  (when (font-sampler app)
    (vkDestroySampler (h (device app)) (h (font-sampler app)) (h (allocator app)))
    (setf (font-sampler app) nil))
  (when (descriptor-set-layout app)
    (vkDestroyDescriptorSetLayout (h (device app)) (h (descriptor-set-layout app)) (h (allocator app)))
    (setf (descriptor-set-layout app) nil))
  (when (pipeline-layout app)
    (vkDestroyPipelineLayout (h (device app)) (h (pipeline-layout app)) (h (allocator app)))
    (setf (pipeline-layout app) nil))
  (when (pipeline app)
    (vkDestroyPipeline (h (device app)) (h (pipeline app)) (h (allocator app)))
    (setf (pipeline app) nil))
  (values))

#+windows
(defcfun ("glfwGetWin32Window" glfwGetWin32Window) :pointer
  (window :pointer))

(defcallback imgui-get-clipboard-text-callback :string ((user-data :pointer))
  (glfwGetClipboardString user-data))

(defcallback imgui-set-clipboard-text-callback :void ((user-data :pointer) (text :string))
  (glfwSetClipboardString user-data text))

(defcallback imgui-mouse-button-callback :void ((user-data :pointer) (button :int) (action :int) (mods :int))
  (declare (ignorable user-data mods))
  (let* ((window (find-window user-data)))
    (on-mouse-button window button action))
  (values))

(defcallback imgui-scroll-callback :void ((user-data :pointer) (xoffset :double) (yoffset :double))
  (declare (ignorable user-data xoffset))
  (imgui-scroll-event (find-window user-data) yoffset)
  (values))

(defcallback imgui-key-callback :void ((user-data :pointer) (key :int) (arg2 :int) (action :int) (mods :int))
  (declare (ignorable user-data arg2 mods))
  (imgui-key-callback-function key action))

(defun imgui-key-callback-function (key action)
  (let* ((io (ig::igGetIO))
	 (p-keys-down (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::KeysDown)))
    (when (eq action GLFW_PRESS)
      (setf (mem-aref p-keys-down :bool key) t))
    (when (eq action GLFW_RELEASE)
      (setf (mem-aref p-keys-down :bool key) nil))

    (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::KeyCtrl)
	  (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_CONTROL)
	      (mem-aref p-keys-down :bool GLFW_KEY_RIGHT_CONTROL))
		   
	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::KeyShift)
	  (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_SHIFT)
	      (mem-aref p-keys-down :bool GLFW_KEY_RIGHT_SHIFT))

	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::KeyAlt)
	  (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_ALT)
	      (mem-aref p-keys-down :bool GLFW_KEY_RIGHT_ALT))

	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::KeySuper)
	  (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_SUPER)
	      (mem-aref p-keys-down :bool GLFW_KEY_RIGHT_SUPER)))
	       
    (values)))

(defcallback imgui-char-callback :void ((user-data :pointer) (c :uint32))
  (declare (ignorable user-data))
  (let ((io (ig::igGetIO)))
    (when (and (> c 0) (< c #x10000))
      (ig::ImGuiIO_AddInputCharacter io c)))
  (values))



(defun imgui-create-fonts-texture (app command-buffer &key (style-colors :classic))

  (let ((io (ig::igGetIO)))

    (case style-colors
      (:light (ig::igStyleColorsLight (ig::igGetStyle)))
      (:dark (ig::igStyleColorsDark (ig::igGetStyle)))
      (t (ig::igStyleColorsClassic (ig::igGetStyle))))
    
    (with-foreign-objects ((p-pixels :pointer)
			   (p-width :int)
			   (p-height :int)
			   (p-bpp :int))
      (ig::ImFontAtlas_GetTexDataAsRGBA32 (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::Fonts)
					  p-pixels p-width p-height p-bpp)

      (let* ((width (mem-aref p-width :int))
	     (height (mem-aref p-height :int))
	     (upload-size (* width height (mem-aref p-bpp :int) (foreign-type-size :char))))

	(setf (font-image app) (create-image (device app) width height :allocator (allocator app))
	      (font-view app) (create-image-view (device app) (font-image app) :allocator (allocator app)))

	;; update the descriptor set:
	(with-vk-struct (p-desc-image VkDescriptorImageInfo)
	  (with-foreign-slots ((vk::sampler
				vk::imageView
				vk::imageLayout)
			       p-desc-image (:struct VkDescriptorImageInfo))
	    (setf vk::sampler (h (font-sampler app))
		  vk::imageView (h (font-view app))
		  vk::imageLayout VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
	    (with-vk-struct (p-write-desc VkWriteDescriptorSet)
	      (with-foreign-slots ((vk::dstSet
				    vk::descriptorCount
				    vk::descriptorType
				    vk::pImageInfo)
				   p-write-desc (:struct VkWriteDescriptorSet))
		(setf vk::dstSet (h (descriptor-set app))
		      vk::descriptorCount 1
		      vk::descriptorType VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
		      vk::pImageInfo p-desc-image))
	      (vkUpdateDescriptorSets (h (device app)) 1 p-write-desc 0 +nullptr+)

	      ;; create the upload buffer:
	      (setf (upload-buffer app) (create-buffer-1 (device app)
							 upload-size
							 VK_BUFFER_USAGE_TRANSFER_SRC_BIT
							 :allocator (allocator app))
		    (upload-buffer-memory app) (allocate-buffer-memory (device app)
								       (upload-buffer app)
								       VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
								       :allocator (allocator app))
		    (buffer-memory-alignment app) (max (alignment (upload-buffer-memory app))
						       (buffer-memory-alignment app)))
	      (vkBindBufferMemory (h (device app)) (h (upload-buffer app)) (h (upload-buffer-memory app)) 0)

	      ;; upload to buffer
	      (with-foreign-object (p-map :pointer)

		(check-vk-result
		 (vkMapMemory (h (device app)) (h (upload-buffer-memory app)) 0 upload-size 0 p-map))

		(memcpy (mem-aref p-map :pointer) (mem-aref p-pixels :pointer) upload-size)

		(with-vk-struct (p-range VkMappedMemoryRange)
		  (with-foreign-slots ((vk::memory
					vk::size)
				       p-range (:struct VkMappedMemoryRange))
		    (setf vk::memory (h (upload-buffer-memory app))
			  vk::size upload-size))
		  (check-vk-result
		   (vkFlushMappedMemoryRanges (h (device app)) 1 p-range))

		  (vkUnmapMemory (h (device app)) (h (upload-buffer-memory app)))))

	      ;; copy to image
	      (with-vk-struct (p-copy-barrier VkImageMemoryBarrier)
		(with-foreign-slots ((vk::dstAccessMask
				      vk::oldLayout
				      vk::newLayout
				      vk::srcQueueFamilyIndex
				      vk::dstQueueFamilyIndex
				      vk::image)
				     p-copy-barrier (:struct VkImageMemoryBarrier))
		  (setf vk::dstAccessMask VK_ACCESS_TRANSFER_WRITE_BIT
			vk::oldLayout VK_IMAGE_LAYOUT_UNDEFINED
			vk::newLayout VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
			vk::srcQueueFamilyIndex VK_QUEUE_FAMILY_IGNORED
			vk::dstQueueFamilyIndex VK_QUEUE_FAMILY_IGNORED
			vk::image (h (font-image app)))

		  (let ((p-subresource-range
			 (foreign-slot-pointer p-copy-barrier '(:struct VkImageMemoryBarrier) 'vk::subresourceRange)))
		    (with-foreign-slots ((vk::aspectMask
					  vk::levelCount
					  vk::layerCount)
					 p-subresource-range
					 (:struct VkImageSubresourceRange))
		      (setf vk::aspectMask VK_IMAGE_ASPECT_COLOR_BIT
			    vk::levelCount 1
			    vk::layerCount 1)))
		  (vkCmdPipelineBarrier (h command-buffer)
					VK_PIPELINE_STAGE_HOST_BIT
					VK_PIPELINE_STAGE_TRANSFER_BIT
					0 0 +nullptr+ 0 +nullptr+ 1
					p-copy-barrier)

		  (with-vk-struct (p-region VkBufferImageCopy)
		    (let ((p-image-subresource
			   (foreign-slot-pointer p-region '(:struct VkBufferImageCopy) 'vk::imageSubresource))
			  (p-image-extent
			   (foreign-slot-pointer p-region '(:struct VkBufferImageCopy) 'vk::imageExtent)))
		      (with-foreign-slots ((vk::aspectMask
					    vk::layerCount)
					   p-image-subresource (:struct VkImageSubresourceLayers))
			(setf vk::aspectMask VK_IMAGE_ASPECT_COLOR_BIT
			      vk::layerCount 1))
		      (with-foreign-slots ((vk::width
				       vk::height
				       vk::depth)
				      p-image-extent (:struct VkExtent3D))
			(setf vk::width width
			      vk::height height
			      vk::depth 1)))
		    (vkCmdCopyBufferToImage (h command-buffer) (h (upload-buffer app))
					    (h (font-image app))
					    VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL 1 p-region))

		  (with-vk-struct (p-use-barrier VkImageMemoryBarrier)
		    (with-foreign-slots ((vk::srcAccessMask
					  vk::dstAccessMask
					  vk::oldLayout
					  vk::newLayout
					  vk::srcQueueFamilyIndex
					  vk::dstQueueFamilyIndex
					  vk::image)
					 p-use-barrier (:struct VkImageMemoryBarrier))
		      (setf vk::srcAccessMask VK_ACCESS_TRANSFER_WRITE_BIT
			    vk::dstAccessMask VK_ACCESS_SHADER_READ_BIT
			    vk::oldLayout VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
			    vk::newLayout VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
			    vk::srcQueueFamilyIndex VK_QUEUE_FAMILY_IGNORED
			    vk::dstQueueFamilyIndex VK_QUEUE_FAMILY_IGNORED
			    vk::image (h (font-image app))))
		    (let ((p-subresource-range
			   (foreign-slot-pointer p-use-barrier '(:struct VkImageMemoryBarrier) 'vk::subresourceRange)))
		      (with-foreign-slots ((vk::aspectMask
					    vk::levelCount
					    vk::layerCount)
					   p-subresource-range
					   (:struct VkImageSubresourceRange))
			(setf vk::aspectMask VK_IMAGE_ASPECT_COLOR_BIT
			      vk::levelCount 1
			      vk::layerCount 1)))

		    (vkCmdPipelineBarrier (h command-buffer) VK_PIPELINE_STAGE_TRANSFER_BIT
					  VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
					  0 0 +nullptr+ 0 +nullptr+ 1 p-use-barrier)))))))))
    (ig::ImFontAtlas_SetTexID (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::Fonts)
			      (h (font-image app)))))
	

(defun imgui-init (app window
		   &key (install-callbacks-p t)
		     (allocator +null-allocator+)
		     device render-pass
		     (pipeline-cache +null-pipeline-cache+)
		     descriptor-pool
		     frame-count)		   

  (setf (allocator app) allocator
	(device app) device
	(render-pass app) render-pass
	(pipeline-cache app) pipeline-cache
	(descriptor-pool app) descriptor-pool
	(frame-count app) frame-count
	(window app) window)

  (setf (imgui-context app) (ig::igCreateContext +nullptr+))
  (let* ((io (ig::igGetIO))
	 (p-key-map (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::KeyMap)))
    
    (setf (mem-aref p-key-map :int ImGuiKey_Tab) GLFW_KEY_TAB
	  (mem-aref p-key-map :int ImGuiKey_LeftArrow) GLFW_KEY_LEFT
	  (mem-aref p-key-map :int ImGuiKey_RightArrow) GLFW_KEY_RIGHT
	  (mem-aref p-key-map :int ImGuiKey_UpArrow) GLFW_KEY_UP
	  (mem-aref p-key-map :int ImGuiKey_DownArrow) GLFW_KEY_DOWN
	  (mem-aref p-key-map :int ImGuiKey_PageUp) GLFW_KEY_PAGE_UP
	  (mem-aref p-key-map :int ImGuiKey_PageDown) GLFW_KEY_PAGE_DOWN
	  (mem-aref p-key-map :int ImGuiKey_Home) GLFW_KEY_HOME
	  (mem-aref p-key-map :int ImGuiKey_End) GLFW_KEY_END
	  (mem-aref p-key-map :int ig::ImGuiKey_Insert) GLFW_KEY_INSERT
	  (mem-aref p-key-map :int ImGuiKey_Delete) GLFW_KEY_DELETE
	  (mem-aref p-key-map :int ImGuiKey_Backspace) GLFW_KEY_BACKSPACE
	  (mem-aref p-key-map :int ig::ImGuiKey_Space) GLFW_KEY_SPACE
	  (mem-aref p-key-map :int ImGuiKey_Enter) GLFW_KEY_ENTER
	  (mem-aref p-key-map :int ImGuiKey_Escape) GLFW_KEY_ESCAPE
	  (mem-aref p-key-map :int ImGuiKey_A) GLFW_KEY_A
	  (mem-aref p-key-map :int ImGuiKey_C) GLFW_KEY_C
	  (mem-aref p-key-map :int ImGuiKey_V) GLFW_KEY_V
	  (mem-aref p-key-map :int ImGuiKey_X) GLFW_KEY_X
	  (mem-aref p-key-map :int ImGuiKey_Y) GLFW_KEY_Y
	  (mem-aref p-key-map :int ImGuiKey_Z) GLFW_KEY_Z

	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::SetClipboardTextFn)
	  (callback imgui-set-clipboard-text-callback)
	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::GetClipboardTextFn)
	  (callback imgui-get-clipboard-text-callback)
	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::ClipBoardUserData) (h (window app))
	  #+windows
	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::ImeWindowHandle)
	  #+windows
	  (glfwGetWin32Window (h (window app)))) 

    (when install-callbacks-p
      (glfwSetMouseButtonCallback (h (window app)) (callback imgui-mouse-button-callback))
      (glfwSetScrollCallback (h (window app)) (callback imgui-scroll-callback))
      (glfwSetKeyCallback (h (window app)) (callback imgui-key-callback))
      (glfwSetCharCallback (h (window app)) (callback imgui-char-callback)))

    (imgui-create-device-objects app)

    t))
  
(defun imgui-shutdown (app)
  (imgui-invalidate-device-objects app)
  (ig::igDestroyContext (imgui-context app))
  (setf (imgui-context app) nil)
  (values))

(defun imgui-new-frame (app)
  (let ((io (ig:igGetIO)))
    (multiple-value-bind (width height) (get-window-size (window app))
      (let ((p-display-size (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::DisplaySize)))
	(setf (foreign-slot-value p-display-size '(:struct ig::ImVec2) 'ig::x) (coerce width 'single-float)
	      (foreign-slot-value p-display-size '(:struct ig::ImVec2) 'ig::y) (coerce height 'single-float)))

      (multiple-value-bind (display-w display-h) (get-framebuffer-size (window app))
	(let ((p-framebuffer-scale (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::DisplayFramebufferScale)))

	  (setf (foreign-slot-value p-framebuffer-scale '(:struct ig::ImVec2) 'ig::x)
		(if (zerop width)
		    (coerce (/ display-w width) 'single-float)
		    0.0f0)
		(foreign-slot-value p-framebuffer-scale '(:struct ig::ImVec2) 'ig::y)
		(if (zerop height)
		    (coerce (/ display-h height) 'single-float)
		    0.0f0)))))

    (let ((current-time (glfwGetTime)))
      (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::DeltaTime)
	    (coerce (if (> (imgui-time app) 0.0f0)
			(if (zerop (- current-time (imgui-time app)))
			    (/ 1.0f0 60.0f0)
			    (- current-time (imgui-time app)))
			(/ 1.0f0 60.0f0))
		    'single-float))

      (setf (imgui-time app) current-time))


    (if (not (zerop (glfwGetWindowAttrib (h (window app)) GLFW_FOCUSED)))
	(multiple-value-bind (mouse-x mouse-y) (get-cursor-pos (window app))

	  (let ((p-mouse-pos (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::MousePos)))
	    (setf (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::x) (coerce mouse-x 'single-float)
		  (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::y) (coerce mouse-y 'single-float))))
	
	(let ((p-mouse-pos (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::MousePos))
	      (-FLT_MAX (- (igGet_FLT_MAX))))

	    (setf (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::x) -FLT_MAX
		  (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::y) -FLT_MAX)))
    (loop for i from 0 below 3
       do (setf (mem-aref (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::MouseDown)
			  :bool i)
		(or (elt (mouse-pressed app) i)
		    (not (zerop (glfwGetMouseButton (h (window app)) i)))))
	 (setf (elt (mouse-pressed app) i) nil))

    (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::Mousewheel) (mouse-wheel app))

    (setf (mouse-wheel app) 0.0f0)

    (glfwSetInputMode (h (window app)) GLFW_CURSOR
		      (if (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::MouseDrawCursor)
			  GLFW_CURSOR_HIDDEN
			  GLFW_CURSOR_NORMAL))
    (ig::igNewFrame)

    (values)))
	
			     


      
(defun imgui-render (app command-buffer frame-index)
  (ig:igRender)
  (imgui-render-draw-lists app (ig::igGetDrawData) command-buffer frame-index)
  (values))
		     
