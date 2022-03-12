(in-package :vktk)

(cffi:defcstruct ImGui_ImplVulkanH_Window
  (Width :int)
  (Height :int)
  (Swapchain VkSwapchainKHR)
  (Surface VkSurfaceKHR)
  (SurfaceFormat (:struct VkSurfaceFormatKHR))
  (PresentMode VkPresentModeKHR)
  (ClearEnable :bool)
  (FrameIndex :uint32)
  (ImageCount :uint32)
  (SemaphoreIndex :uint32)
  (Frames :pointer)
  (FrameSemaphores :pointer))

(cffi:defcstruct ImGui_ImplVulkanH_FrameRenderBuffers
  (VertexBufferMemory VkDeviceMemory)
  (IndexBufferMemory VkDeviceMemory)
  (VertexBufferSize VkDeviceSize)
  (IndexBufferSize VkDeviceSize)
  (VertexBuffer VkBuffer)
  (IndexBuffer VkBuffer))

(cffi:defcstruct ImGui_ImplVulkanH_WindowRenderBuffers
  (Index :uint32)
  (Count :uint32)
  (FrameRenderBuffers :pointer))

(cffi:defcstruct ImGuiViewportDataVulkan
  (WindowOwned :bool)
  (Window (:struct ImGui_ImplVulkanH_Window));; Used by secondary viewports only
  (RenderBuffers (:struct ImGui_ImplVulkanH_WindowRenderBuffers))) ;; Used by all viewports

(defun renderer-user-data-window (renderer-user-data)
  (foreign-slot-value renderer-user-data '(:struct ImGuiViewportDataVulkan) 'Window))

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


(defun imgui-vulkan-setup-render-state (imgui draw-data command-buffer vertex-buffer index-buffer
					fb-width fb-height)
  (with-slots (descriptor-set pipeline-layout pipeline) imgui
    ;; bind pipeline and descriptor sets:
    (vkCmdBindPipeline (h command-buffer) VK_PIPELINE_BIND_POINT_GRAPHICS (h pipeline))
	
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
      (with-foreign-slots ((%vk::x
			    %vk::y
			    %vk::width
			    %vk::height
			    %vk::minDepth
			    %vk::maxDepth)
			   p-viewport
			   (:struct VkViewport))
	(let* ((p-display-size
		(foreign-slot-pointer draw-data '(:struct ig::ImDrawData) 'ig::DisplaySize))
	       (p-display-pos
		(foreign-slot-pointer draw-data '(:struct ig::ImDrawData) 'ig::DisplayPos))
	       (display-pos-x (foreign-slot-value p-display-pos '(:struct ig::ImVec2) 'ig::x))
	       (display-pos-y (foreign-slot-value p-display-pos '(:struct ig::ImVec2) 'ig::y))
	       (display-size-x (foreign-slot-value p-display-size '(:struct ig::ImVec2) 'ig::x))
	       (display-size-y (foreign-slot-value p-display-size '(:struct ig::ImVec2) 'ig::y)))

	  ;;(print display-size-x) (print display-size-y)

	  (setf %vk::x 0.0f0
		%vk::y 0.0f0
		%vk::width fb-width #+NIL(* #+darwin 2.0f0 #+(or windows linux) 1.0f0
					     display-size-x)
		%vk::height fb-height #+NIL(* #+darwin 2.0f0 #+(or windows linux) 1.0f0
					     display-size-y)
		%vk::minDepth 0.0f0
		%vk::maxDepth 1.0f0)
	  (vkCmdSetViewport (h command-buffer) 0 1 p-viewport)

	  ;; Setup scale and translation:
	  (with-foreign-object (p-scale :float 2)
	    (setf (mem-aref p-scale :float 0)
		  (/ 2.0f0 display-size-x)
		  (mem-aref p-scale :float 1)
		  (/ 2.0f0 display-size-y))

	    (with-foreign-object (p-translate :float 2)
	      (setf (mem-aref p-translate :float 0) (- -1.0f0 (* display-pos-x (mem-aref p-scale :float 0)))
		    (mem-aref p-translate :float 1) (- -1.0f0 (* display-pos-y (mem-aref p-scale :float 1))))

	      (vkCmdPushConstants (h command-buffer) (h pipeline-layout)
				  VK_SHADER_STAGE_VERTEX_BIT
				  (* #.(foreign-type-size :float) 0)
				  (* #.(foreign-type-size :float) 2)
				  p-scale)

	      (vkCmdPushConstants (h command-buffer) (h pipeline-layout)
				  VK_SHADER_STAGE_VERTEX_BIT
				  (* #.(foreign-type-size :float) 2)
				  (* #.(foreign-type-size :float) 2)
				  p-translate))))))))

(defun imgui-vulkan-render-draw-data (imgui viewport command-buffer frame-index)
  (let* ((draw-data (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::DrawData))
	     (window (get-viewport-window viewport))
	 
	     (p-display-size
	       (foreign-slot-pointer draw-data '(:struct ig::ImDrawData) 'ig::DisplaySize))
	     #+NOTYET(p-framebuffer-scale
	               (foreign-slot-pointer draw-data '(:struct ig::ImDrawData) 'ig::FramebufferScale))
	     (framebuffer-scale-x #+(or windows linux) 1
			                  #+darwin 2
			                  #+NIL(foreign-slot-value p-framebuffer-scale
						                               '(:struct ig::ImVec2) 'ig::x))
	     (framebuffer-scale-y #+(or windows linux) 1 #+darwin 2
			                                         #+NIL(foreign-slot-value p-framebuffer-scale
						                                                      '(:struct ig::ImVec2) 'ig::y))
	     (display-size-x (foreign-slot-value p-display-size '(:struct ig::ImVec2) 'ig::x))
	     (display-size-y (foreign-slot-value p-display-size '(:struct ig::ImVec2) 'ig::y))
	     (fb-width (* display-size-x framebuffer-scale-x))
	     (fb-height (* display-size-y framebuffer-scale-y)))

    (when (or (<= fb-width 0) (<= fb-height 0))
      (return-from imgui-vulkan-render-draw-data))

    (when (zerop (foreign-slot-value draw-data '(:struct ig::ImDrawData) 'ig::TotalVtxCount))
      (return-from imgui-vulkan-render-draw-data (values)))
    
    (let ((device (logical-device imgui))
	      (allocator (allocator imgui)))
      (with-slots (buffer-memory-alignment descriptor-set pipeline-layout) imgui
	    (maybe-init-frame-data window (frame-count imgui))
	    (let ((frame-data (window-frame-data window)))
	      (unless (elt frame-data frame-index)
	        (setf (elt frame-data frame-index) (make-instance 'imgui-frame-data)))
	      (with-slots (vertex-buffer
		               vertex-buffer-size
		               vertex-buffer-memory
		               index-buffer
		               index-buffer-size
		               index-buffer-memory) (elt frame-data frame-index)
		      
	        (let (#+NIL(io (ig:igGetIO)))
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

		          (let ((new-buffer-size (* (1+ (ceiling (/ (1- vertex-size) buffer-memory-alignment)))
					                        buffer-memory-alignment)))
		            (setq vertex-buffer
			              (create-buffer-1 device new-buffer-size VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
					                       :buffer-class 'vertex-buffer
					                       :allocator allocator))
		            (setq vertex-buffer-size new-buffer-size)
		            (setf vertex-buffer-memory
			              (allocate-buffer-memory device
						                          vertex-buffer
						                          VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
						                          :allocator allocator))
		            (setf buffer-memory-alignment (max (vk::alignment vertex-buffer-memory)
						                               buffer-memory-alignment))
		            (bind-buffer-memory device vertex-buffer vertex-buffer-memory)))

		        (when (or (not index-buffer) (< index-buffer-size index-size))
		          (when index-buffer
		            (vkDestroyBuffer (h device) (h index-buffer) (h allocator)))

		          (when index-buffer-memory
		            (vkFreeMemory (h device) (h index-buffer-memory) (h allocator)))

		          (let ((new-buffer-size (* (1+ (ceiling (/ (1- index-size) buffer-memory-alignment)))
					                        buffer-memory-alignment)))
		            (setq index-buffer
			              (create-buffer-1 device new-buffer-size VK_BUFFER_USAGE_INDEX_BUFFER_BIT
					                       :buffer-class 'index-buffer
					                       :allocator allocator))
		            (setq index-buffer-size new-buffer-size)
		            (setf index-buffer-memory
			              (allocate-buffer-memory device
						                          index-buffer
						                          VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
						                          :allocator allocator))
		            (setf buffer-memory-alignment (max (vk::alignment index-buffer-memory)
						                               buffer-memory-alignment))
		            (bind-buffer-memory device index-buffer index-buffer-memory)))

	      
	    

		        (let ((p-vtx-dst) (p-idx-dst))
		          (with-foreign-objects ((pp-vtx-dst :pointer)
					                     (pp-idx-dst :pointer))

		            (check-vk-result (vkMapMemory (h device)
						                          (h vertex-buffer-memory)
						                          0 vertex-buffer-size 0 pp-vtx-dst))
		            (setf p-vtx-dst (mem-aref pp-vtx-dst :pointer))

		            (check-vk-result (vkMapMemory (h device)
						                          (h index-buffer-memory)
						                          0 index-buffer-size 0 pp-idx-dst))
		            (setf p-idx-dst (mem-aref pp-idx-dst :pointer))

		            (loop for n from 0 below (foreign-slot-value draw-data '(:struct ig::ImDrawData) 'ig::CmdListsCount)
		                  do (let* ((p-cmd-list
				                      (mem-aref (foreign-slot-value draw-data '(:struct ig::ImDrawData) 'ig::CmdLists)
					                            :pointer n))
				                    (p-vtx-buffer
				                      (foreign-slot-pointer p-cmd-list '(:struct ig::ImDrawList) 'ig::VtxBuffer))
				                    (p-idx-buffer
				                      (foreign-slot-pointer p-cmd-list '(:struct ig::ImDrawList) 'ig::IdxBuffer))
				                    (p-vtx-data (ig::ImVector_ImDrawVert_begin p-vtx-buffer))
				                    (p-idx-data (ig::ImVector_ImDrawIdx_begin p-idx-buffer)))

			                   (vk::memcpy p-vtx-dst p-vtx-data
					                       (ig::ImVector_ImDrawVert_size_in_bytes p-vtx-buffer))

			                   (vk::memcpy p-idx-dst p-idx-data
					                       (ig::ImVector_ImDrawIdx_size_in_bytes p-idx-buffer))

			                   (incf-pointer p-vtx-dst (ig::ImVector_ImDrawVert_size_in_bytes p-vtx-buffer))

			                   (incf-pointer p-idx-dst (ig::ImVector_ImDrawIdx_size_in_bytes p-idx-buffer))))))
      
		        (with-foreign-object (p-ranges '(:struct VkMappedMemoryRange) 2)
		          (zero-struct (mem-aptr p-ranges '(:struct VkMappedMemoryRange) 0)
			                   '(:struct VkMappedMemoryRange))
		          (zero-struct (mem-aptr p-ranges '(:struct VkMappedMemoryRange) 1)
			                   '(:struct VkMappedMemoryRange))
		          (with-foreign-slots ((%vk::sType
                                        %vk::memory
                                        %vk::offset
					                    %vk::size)
				                       (mem-aptr p-ranges '(:struct VkMappedMemoryRange) 0)
				                       (:struct VkMappedMemoryRange))
		            (setf %vk::sType VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
			              %vk::memory (h vertex-buffer-memory)
                          %vk::offset 0
			              %vk::size VK_WHOLE_SIZE))

		          (with-foreign-slots ((%vk::sType
                                        %vk::memory
                                        %vk::offset
					                    %vk::size)
				                       (mem-aptr p-ranges '(:struct VkMappedMemoryRange) 1)
				                       (:struct VkMappedMemoryRange))
		            (setf %vk::sType VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
			              %vk::memory (h index-buffer-memory)
                          %vk::offset 0
			              %vk::size VK_WHOLE_SIZE))

		          (check-vk-result (vkFlushMappedMemoryRanges (h device) 2 p-ranges))

		          (vkUnmapMemory (h device) (h vertex-buffer-memory))

		          (vkUnmapMemory (h device) (h index-buffer-memory)))

		        (imgui-vulkan-setup-render-state imgui draw-data command-buffer vertex-buffer index-buffer
						                         fb-width fb-height))
              ;; Render the command lists:
	          (let ((p-clip-off (foreign-slot-pointer draw-data '(:struct ig::ImDrawData) 'ig::DisplayPos))
		            (p-clip-scale (foreign-slot-pointer draw-data '(:struct ig::ImDrawData) 'ig::FramebufferScale))
		            (vtx-offset 0)
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
					                            (foreign-slot-pointer p-scissor '(:struct VkRect2D) '%vk::offset))
				                              (p-extent
					                            (foreign-slot-pointer p-scissor '(:struct VkRect2D) '%vk::extent)))
			       
				                          (with-foreign-slots ((%vk::x %vk::y) p-offset (:struct VkOffset2D))
				                            (with-foreign-slots ((%vk::width %vk::height) p-extent (:struct VkExtent2D))
				   
				                              (let* ((p-clip-rect
					                                   (foreign-slot-pointer p-cmd '(:struct ig::ImDrawCmd) 'ig::ClipRect))
					                                 (clip-rect-x
					                                   (foreign-slot-value p-clip-rect '(:struct ig::ImVec4) 'ig::x))
					                                 (clip-rect-y
					                                   (foreign-slot-value p-clip-rect '(:struct ig::ImVec4) 'ig::y))
					                                 (clip-rect-z
					                                   (foreign-slot-value p-clip-rect '(:struct ig::ImVec4) 'ig::z))
					                                 (clip-rect-w
					                                   (foreign-slot-value p-clip-rect '(:struct ig::ImVec4) 'ig::w))
					                                 (clip-off-x
					                                   (foreign-slot-value p-clip-off '(:struct ig::ImVec2) 'ig::x))
					                                 (clip-off-y
					                                   (foreign-slot-value p-clip-off '(:struct ig::ImVec2) 'ig::y))
					                                 (clip-scale-x
					                                   (foreign-slot-value p-clip-scale '(:struct ig::ImVec2) 'ig::x))
					                                 (clip-scale-y
					                                   (foreign-slot-value p-clip-scale '(:struct ig::ImVec2) 'ig::y))
					                                 (cr-x (* (- clip-rect-x clip-off-x) clip-scale-x))
					                                 (cr-y (* (- clip-rect-y clip-off-y) clip-scale-y))
					                                 (cr-z (* (- clip-rect-z clip-off-x) clip-scale-x))
					                                 (cr-w (* (- clip-rect-w clip-off-y) clip-scale-y)))

					                            (when (and (< cr-x fb-width)
						                                   (< cr-y fb-height)
						                                   (>= cr-z 0.0f0)
						                                   (>= cr-w 0.0f0))

                                                  ;;(print cr-x)
                                                  ;;(print cr-y)
                                                  ;;(print cr-z)
                                                  ;;(print cr-w)
					   
					                              (when (< cr-x 0.0f0)
					                                (setq cr-x 0.0f0))
					                              (when (< cr-y 0.0f0)
					                                (setq cr-y 0.0f0))
					   
					                              (setf %vk::x (round cr-x)
						                                %vk::y (round cr-y)
						                                %vk::width (round (- cr-z cr-x))
						                                %vk::height (round (- cr-w cr-y))))
					 
					                            (vkCmdSetScissor (h command-buffer) 0 1 p-scissor)

					                            (vkCmdDrawIndexed (h command-buffer)
							                                      (foreign-slot-value p-cmd '(:struct ig::ImDrawCmd) 'ig::ElemCount)
							                                      1
							                                      (+ (foreign-slot-value p-cmd '(:struct ig::ImDrawCmd) 'ig::IdxOffset) idx-offset)
							                                      (+ (foreign-slot-value p-cmd '(:struct ig::ImDrawCmd) 'ig::VtxOffset) vtx-offset)
							                                      0))))))
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
			                     )))))))))))

(defun imgui-vulkan-create-fonts-texture (imgui command-buffer)

  (let* ((io (ig::igGetIO))
	 (application (vk::application imgui))
	 (device (default-logical-device application))
	 (allocator (allocator application)))

    (with-foreign-objects ((p-pixels :pointer)
			   (p-width :int)
			   (p-height :int)
			   (p-bpp :int))
      (ig::ImFontAtlas_GetTexDataAsRGBA32 (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::Fonts)
					  p-pixels p-width p-height p-bpp)

      (let* ((width (mem-aref p-width :int))
	     (height (mem-aref p-height :int))
	     (upload-size (* width height (mem-aref p-bpp :int) #.(foreign-type-size :char))))

	(setf (font-image imgui) (create-image device width height :allocator allocator)
	      (font-view imgui) (create-image-view device (font-image imgui) :allocator allocator))

	;; update the descriptor set:
	(with-vk-struct (p-desc-image VkDescriptorImageInfo)
	  (with-foreign-slots ((%vk::sampler
				%vk::imageView
				%vk::imageLayout)
			       p-desc-image (:struct VkDescriptorImageInfo))
	    (setf %vk::sampler (h (font-sampler imgui))
		  %vk::imageView (h (font-view imgui))
		  %vk::imageLayout VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
	    (with-vk-struct (p-write-desc VkWriteDescriptorSet)
	      (with-foreign-slots ((%vk::dstSet
				    %vk::descriptorCount
				    %vk::descriptorType
				    %vk::pImageInfo)
				   p-write-desc (:struct VkWriteDescriptorSet))
		(setf %vk::dstSet (h (descriptor-set imgui))
		      %vk::descriptorCount 1
		      %vk::descriptorType VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
		      %vk::pImageInfo p-desc-image))
	      (vkUpdateDescriptorSets (h device) 1 p-write-desc 0 +nullptr+)

	      ;; create the upload buffer:
	      (setf (upload-buffer imgui) (create-buffer-1 device
							 upload-size
							 VK_BUFFER_USAGE_TRANSFER_SRC_BIT
							 :allocator allocator)
		    (upload-buffer-memory imgui) (allocate-buffer-memory device
									 (upload-buffer imgui)
									 VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
									 :allocator allocator)
		    (buffer-memory-alignment imgui) (max (vk::alignment (upload-buffer-memory imgui))
							 (buffer-memory-alignment imgui)))
	      (vkBindBufferMemory (h device) (h (upload-buffer imgui)) (h (upload-buffer-memory imgui)) 0)

	      ;; upload to buffer
	      (with-foreign-object (p-map :pointer)

		(check-vk-result
		 (vkMapMemory (h device) (h (upload-buffer-memory imgui)) 0 upload-size 0 p-map))

		(vk::memcpy (mem-aref p-map :pointer) (mem-aref p-pixels :pointer) upload-size)

		(with-vk-struct (p-range VkMappedMemoryRange)
		  (with-foreign-slots ((%vk::memory
					%vk::size)
				       p-range (:struct VkMappedMemoryRange))
		    (setf %vk::memory (h (upload-buffer-memory imgui))
			  %vk::size upload-size))
		  (check-vk-result
		   (vkFlushMappedMemoryRanges (h device) 1 p-range))

		  (vkUnmapMemory (h device) (h (upload-buffer-memory imgui)))))

	      ;; copy to image
	      (with-vk-struct (p-copy-barrier VkImageMemoryBarrier)
		(with-foreign-slots ((%vk::dstAccessMask
				      %vk::oldLayout
				      %vk::newLayout
				      %vk::srcQueueFamilyIndex
				      %vk::dstQueueFamilyIndex
				      %vk::image)
				     p-copy-barrier (:struct VkImageMemoryBarrier))
		  (setf %vk::dstAccessMask VK_ACCESS_TRANSFER_WRITE_BIT
			%vk::oldLayout VK_IMAGE_LAYOUT_UNDEFINED
			%vk::newLayout VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
			%vk::srcQueueFamilyIndex vk::VK_QUEUE_FAMILY_IGNORED
			%vk::dstQueueFamilyIndex vk::VK_QUEUE_FAMILY_IGNORED
			%vk::image (h (font-image imgui)))

		  (let ((p-subresource-range
			 (foreign-slot-pointer p-copy-barrier '(:struct VkImageMemoryBarrier) '%vk::subresourceRange)))
		    (with-foreign-slots ((%vk::aspectMask
					  %vk::levelCount
					  %vk::layerCount)
					 p-subresource-range
					 (:struct VkImageSubresourceRange))
		      (setf %vk::aspectMask VK_IMAGE_ASPECT_COLOR_BIT
			    %vk::levelCount 1
			    %vk::layerCount 1)))
		  (vkCmdPipelineBarrier (h command-buffer)
					VK_PIPELINE_STAGE_HOST_BIT
					VK_PIPELINE_STAGE_TRANSFER_BIT
					0 0 +nullptr+ 0 +nullptr+ 1
					p-copy-barrier)

		  (with-vk-struct (p-region VkBufferImageCopy)
		    (let ((p-image-subresource
			   (foreign-slot-pointer p-region '(:struct VkBufferImageCopy) '%vk::imageSubresource))
			  (p-image-extent
			   (foreign-slot-pointer p-region '(:struct VkBufferImageCopy) '%vk::imageExtent)))
		      (with-foreign-slots ((%vk::aspectMask
					    %vk::layerCount)
					   p-image-subresource (:struct VkImageSubresourceLayers))
			(setf %vk::aspectMask VK_IMAGE_ASPECT_COLOR_BIT
			      %vk::layerCount 1))
		      (with-foreign-slots ((%vk::width
				       %vk::height
				       %vk::depth)
				      p-image-extent (:struct VkExtent3D))
			(setf %vk::width width
			      %vk::height height
			      %vk::depth 1)))
		    (vkCmdCopyBufferToImage (h command-buffer) (h (upload-buffer imgui))
					    (h (font-image imgui))
					    VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL 1 p-region))

		  (with-vk-struct (p-use-barrier VkImageMemoryBarrier)
		    (with-foreign-slots ((%vk::srcAccessMask
					  %vk::dstAccessMask
					  %vk::oldLayout
					  %vk::newLayout
					  %vk::srcQueueFamilyIndex
					  %vk::dstQueueFamilyIndex
					  %vk::image)
					 p-use-barrier (:struct VkImageMemoryBarrier))
		      (setf %vk::srcAccessMask VK_ACCESS_TRANSFER_WRITE_BIT
			    %vk::dstAccessMask VK_ACCESS_SHADER_READ_BIT
			    %vk::oldLayout VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
			    %vk::newLayout VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
			    %vk::srcQueueFamilyIndex vk::VK_QUEUE_FAMILY_IGNORED
			    %vk::dstQueueFamilyIndex vk::VK_QUEUE_FAMILY_IGNORED
			    %vk::image (h (font-image imgui))))
		    (let ((p-subresource-range
			   (foreign-slot-pointer p-use-barrier '(:struct VkImageMemoryBarrier) '%vk::subresourceRange)))
		      (with-foreign-slots ((%vk::aspectMask
					    %vk::levelCount
					    %vk::layerCount)
					   p-subresource-range
					   (:struct VkImageSubresourceRange))
			(setf %vk::aspectMask VK_IMAGE_ASPECT_COLOR_BIT
			      %vk::levelCount 1
			      %vk::layerCount 1)))

		    (vkCmdPipelineBarrier (h command-buffer) VK_PIPELINE_STAGE_TRANSFER_BIT
					  VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
					  0 0 +nullptr+ 0 +nullptr+ 1 p-use-barrier)))))))))
    (ig::ImFontAtlas_SetTexID (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::Fonts)
			      (h (font-image imgui)))
    (values)))

(defun imgui-vulkan-create-device-objects (imgui &key (allocator +null-allocator+))
  (let* ((device (logical-device imgui))
	 (vtx-shader (create-shader-module device *imgui-vertex-shader-binary*
					   *imgui-vertex-shader-binary-size*))
	 (frag-shader (create-shader-module device *imgui-fragment-shader-binary*
					    *imgui-fragment-shader-binary-size*)))
    (setf (font-sampler imgui) (create-sampler device :allocator allocator))
    (setf (descriptor-set-layout imgui)
	  (create-descriptor-set-layout
	   device
	   :allocator allocator
	   :bindings
	   (list (make-instance 'descriptor-set-layout-binding
				:type VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
				:count 1
				:flags VK_SHADER_STAGE_FRAGMENT_BIT
				:samplers (list (font-sampler imgui))))))
    (setf (descriptor-set imgui)
	  (allocate-descriptor-set device (list (descriptor-set-layout imgui)) (descriptor-pool imgui)))
    (setf (pipeline-layout imgui)
	  (create-pipeline-layout device (list (descriptor-set-layout imgui)) :allocator allocator
				  :push-constant-ranges
				  (list (make-instance 'push-constant-range
						       :stage-flags VK_SHADER_STAGE_VERTEX_BIT
						       :offset 0
						       :size (* 4 (foreign-type-size :float))))))
    (setf (pipeline imgui) (create-graphics-pipeline device (pipeline-cache imgui) (pipeline-layout imgui)
						     (render-pass imgui) 1
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
    (vkDestroyShaderModule (h device) (h vtx-shader) (h allocator))
    (vkDestroyShaderModule (h device) (h frag-shader) (h allocator))
    (values)))

(defun imgui-vulkan-destroy-font-upload-objects (imgui)
  (let* ((device (logical-device imgui)))
    (when (upload-buffer imgui)
      (vkDestroyBuffer (h device) (h (upload-buffer imgui)) (h (allocator imgui)))
      (setf (upload-buffer imgui) nil))
    (when (upload-buffer-memory imgui)
      (vkFreeMemory (h device) (h (upload-buffer-memory imgui)) (h (allocator imgui)))
    (setf (upload-buffer-memory imgui) nil))
    (values)))

(defun imgui-vulkan-destroy-device-objects (imgui)
  (imgui-vulkan-destroy-font-upload-objects imgui)
  
  (let* ((application (vk::application imgui))
	 (device (logical-device imgui))
	 (allocator (allocator application)))

    (when (frame-data imgui)
      (loop for frame-datum across (frame-data imgui) for i from 0
	 do (when frame-datum
	      (with-slots (vertex-buffer
			   vertex-buffer-memory
			   index-buffer
			   index-buffer-memory) frame-datum
		(when vertex-buffer
		  (vkDestroyBuffer (h device) (h vertex-buffer) (h allocator))
		  (setf vertex-buffer nil))
		(when vertex-buffer-memory
		  (vkFreeMemory (h device) (h vertex-buffer-memory) (h allocator))
		  (setf vertex-buffer-memory nil))
		(when index-buffer
		  (vkDestroyBuffer (h device) (h index-buffer) (h allocator))
		  (setf index-buffer nil))
		(when index-buffer-memory
		  (vkFreeMemory (h device) (h index-buffer-memory) (h allocator))
		  (setf index-buffer-memory nil))
		(setf (elt (frame-data imgui) i) nil)))))

    (when (font-view imgui)
      (vkDestroyImageView (h device) (h (font-view imgui)) (h allocator))
      (setf (font-view imgui) nil))
    
    (when (font-image imgui)
      (vkDestroyImage (h device) (h (font-image imgui)) (h allocator))
      (setf (font-image imgui) nil))
    
    (when (font-memory imgui)
      (vkFreeMemory (h device) (h (font-memory imgui)) (h allocator))
      (setf (font-memory imgui) nil))
    
    (when (font-sampler imgui)
      (vkDestroySampler (h device) (h (font-sampler imgui)) (h allocator))
      (setf (font-sampler imgui) nil))
    
    (when (descriptor-set-layout imgui)
      (vkDestroyDescriptorSetLayout (h device) (h (descriptor-set-layout imgui)) (h allocator))
      (setf (descriptor-set-layout imgui) nil))
    
    (when (pipeline-layout imgui)
      (vkDestroyPipelineLayout (h device) (h (pipeline-layout imgui)) (h allocator))
      (setf (pipeline-layout imgui) nil))
    
    (when (pipeline imgui)
      (vkDestroyPipeline (h device) (h (pipeline imgui)) (h allocator))
      (setf (pipeline imgui) nil))
    
    (values)))

(defun imgui-vulkan-init (module render-pass)
  (let ((io (ig:igGetIO)))
    (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::BackendRendererName)
	  "lisp-imgui-vulkan")
    (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::BackendFlags)
	  (logior (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::BackendFlags)
		  ig::ImGuiBackendFlags_RendererHasVtxOffset
		  ig::ImGuiBackendFlags_RendererHasViewports))
    (assert (vulkan-instance module))
    (assert (physical-device module))
    (assert (logical-device module))
    ;;(assert (queue module))
    (assert (descriptor-pool module))
    ;;(assert (>= (min-image-count module) 2))
    ;;(assert (>= (image-count module) (min-image-count module)))
    (assert render-pass)

    (imgui-vulkan-create-device-objects module)

    (let ((viewport (ig::igGetMainViewport)))
      (setf (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::RendererUserData)
	    (foreign-alloc '(:struct ImGuiViewportDataVulkan)))

      (when (not (zerop (logand (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::ConfigFlags)
				ig::ImGuiConfigFlags_ViewportsEnable)))
	(imgui-vulkan-init-platform-interface module))

      t)))

(defun imgui-vulkan-shutdown (imgui)
  (imgui-vulkan-destroy-device-objects imgui)
  (let* ((main-viewport (ig::igGetMainViewport))
	 (data (foreign-slot-value main-viewport '(:struct ig::ImGuiViewport)
				   'ig::RendererUserData)))
    (foreign-free data)
    (setf (foreign-slot-value main-viewport '(:struct ig::ImGuiViewport) 'ig::RendererUserData)
	  +nullptr+))
  (imgui-vulkan-shutdown-platform-interface imgui)
  (values))

(defun imgui-vulkan-new-frame (imgui)
  (declare (ignore imgui)))

(defun imgui-vulkan-set-min-image-count (imgui min-image-count)
  (declare (ignore imgui min-image-count))
  (warn "set-min-image-count unimplemented."))

(defun imgui-vulkan-helper-select-surface-format (gpu surface
						  &key
						    (request-formats
						     (list VK_FORMAT_B8G8R8A8_UNORM
							   VK_FORMAT_R8G8B8A8_UNORM
							   VK_FORMAT_B8G8R8_UNORM
							   VK_FORMAT_R8G8B8_UNORM))
						    (request-color-space
						     VK_COLOR_SPACE_SRGB_NONLINEAR_KHR))
  (with-foreign-object (p-count :uint32)
    (check-vk-result (vkGetPhysicalDeviceSurfaceFormatsKHR (h gpu) (h surface) p-count +nullptr+))
    (let ((count (mem-aref p-count :uint32)))
      (let ((p-formats (foreign-alloc '(:struct VkSurfaceFormatKHR) :count count)))
	(check-vk-result (vkGetPhysicalDeviceSurfaceFormatsKHR (h gpu) (h surface) p-count p-formats))
	(if (eq count 1)
	    (if (eq (foreign-slot-value p-formats '(:struct VkSurfaceFormatKHR) '%vk::format)
		    VK_FORMAT_UNDEFINED)
		(make-instance 'surface-format
			       :format (elt request-formats 0)
			       :color-space request-color-space)
		(make-instance 'surface-format
			       :format (foreign-slot-value p-formats
							   '(:struct VkSurfaceFormatKHR) '%vk::format)
			       :color-space (foreign-slot-value p-formats
								'(:struct VkSurfaceFormatKHR) '%vk::colorSpace)))
	    (loop for request-format in request-formats
	       do (loop for i from 0 below count
		     do (let ((p-format (mem-aptr p-formats '(:struct VkSurfaceFormatKHR) i)))
			  (when (and (eq (foreign-slot-value p-format
							     '(:struct VkSurfaceFormatKHR) '%vk::format)
					 request-format)
				     (eq (foreign-slot-value p-format
							     '(:struct VkSurfaceFormatKHR) '%vk::colorSpace)
					 request-color-space))
			    (return-from imgui-vulkan-helper-select-surface-format
			      (make-instance 'surface-format
					     :format request-format
					     :color-space request-color-space)))))))))))


(defun imgui-vulkan-helper-select-present-mode (gpu surface
						&key (request-modes
						      (list VK_PRESENT_MODE_MAILBOX_KHR
							    VK_PRESENT_MODE_IMMEDIATE_KHR
							    VK_PRESENT_MODE_FIFO_KHR)))
						      
  (with-foreign-object (p-avail-count :unsigned-int)
    (vkGetPhysicalDeviceSurfacePresentModesKHR (h gpu) (h surface) p-avail-count +nullptr+)
    (let ((avail-count (mem-aref p-avail-count :unsigned-int)))
      (with-foreign-object (p-avail-modes 'VkPresentModeKHR avail-count)
	(vkGetPhysicalDeviceSurfacePresentModesKHR (h gpu) (h surface) p-avail-count p-avail-modes)
	(loop for mode in request-modes
	     do (loop for i from 0 below avail-count
		   do (when (eq mode (mem-aref p-avail-modes 'VkPresentModeKHR i))
			(return-from imgui-vulkan-helper-select-present-mode mode)))
	   finally (return VK_PRESENT_MODE_FIFO_KHR))))))

(defun imgui-vulkan-init-platform-interface (module)
  (declare (ignore module))
  (let ((platform-io (ig::igGetPlatformIO)))
    (unless (zerop
	     (logand (foreign-slot-value (ig:igGetIO) '(:struct ig::ImGuiIO) 'ig::ConfigFlags)
		     ig::ImGuiConfigFlags_ViewportsEnable))
      (assert (not (null-pointer-p (foreign-slot-value platform-io
						       '(:struct ig::ImGuiPlatformIO)
						       'ig::Platform_CreateVkSurface))))
      (setf (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
				'ig::Renderer_CreateWindow)
	    (callback imgui-vulkan-create-window-callback)
	    
	    (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
				'ig::Renderer_DestroyWindow)
	    (callback imgui-vulkan-destroy-window-callback)

	    (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
				'ig::Renderer_RenderWindow)
	    (callback imgui-vulkan-render-window-callback)

	    (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
				'ig::Renderer_SwapBuffers)
	    (callback imgui-vulkan-swap-buffers-callback))

      (ig::install_Renderer_SetWindowSize_callback
       platform-io (callback imgui-vulkan-set-window-size-callback-internal))

      (values))))

(defun imgui-vulkan-shutdown-platform-interface (imgui)
  (declare (ignore imgui))
  (ig::igDestroyPlatformWindows)
  (values))

(defcallback imgui-vulkan-set-window-size-callback-internal :void
    ((viewport :pointer)
     (w :float)
     (h :float))
  (imgui-vulkan-set-window-size viewport w h))

(defun imgui-vulkan-set-window-size (viewport w h)
  (let ((window (get-viewport-window viewport)))
    (vkDeviceWaitIdle (h (default-logical-device *app*)))
    (recreate-swapchain window (swapchain window) (round w) (round h))
    (values)))

(defcallback imgui-vulkan-render-window-callback :void
    ((viewport :pointer)
     (ignore :pointer))
  (declare (ignore ignore))
  (imgui-vulkan-render-window viewport))

(defun imgui-vulkan-render-window (viewport)
  (let* ((window (get-viewport-window viewport))
	 (surface (render-surface window))
	 (device (default-logical-device *app*))
	 (index (queue-family-index surface))
	 (current-frame (slot-value window 'current-frame)))

    (let* ((swapchain (swapchain window))
	   (queue (find-queue device index))
	   (command-buffer (frame-command-buffer (elt (frame-resources swapchain) current-frame))))
      
      (setf (slot-value window 'image-index)
	    (frame-begin swapchain (render-pass swapchain) current-frame (clear-value window) nil))
      
      (imgui-vulkan-render-draw-data (imgui-module *app*) viewport command-buffer current-frame)
      
      (frame-end swapchain queue current-frame)
      
      (values))))

(defcallback imgui-vulkan-swap-buffers-callback :void
    ((viewport :pointer))
  (imgui-vulkan-swap-buffers viewport))

(defun imgui-vulkan-swap-buffers (viewport)
    (let* ((window (get-viewport-window viewport))
	   (surface (render-surface window))
	   (index (queue-family-index surface))
	   (device (default-logical-device *app*))
	   (queue (find-queue device index))
	   (current-frame (slot-value window 'current-frame))
	   (image-index (slot-value window 'image-index))
	   (swapchain (swapchain window))
	   (image-count (number-of-images swapchain)))
      
      (frame-present swapchain queue current-frame image-index window)
      
      (setf (slot-value window 'current-frame) (mod (1+ current-frame) image-count))
      
      (values)))


(defcallback imgui-vulkan-create-window-callback :void
    ((viewport :pointer))
  (imgui-vulkan-create-window viewport)
  (values))

(defun imgui-vulkan-create-window (viewport)
  (let* ((window (get-viewport-window viewport)))

    (let* ((instance (vulkan-instance *app*))
	   (surface (create-window-surface instance window))
	   (device (default-logical-device *app*))
	   (gpu (physical-device device))
	   (qfi (get-queue-family-index-with-wsi-support gpu surface)))    ;; this one could be more efficient
    
      (initialize-window-surface surface gpu qfi)
      
      (let ((command-pool (find-command-pool device qfi))
	    (format (find-supported-format surface))
	    (present-mode VK_PRESENT_MODE_FIFO_KHR #+NOTYET(get-physical-device-surface-present-mode gpu surface))
	    #+NIL(clear-enable (zerop (logand (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::Flags)
					      ig::ImGuiViewport_NoRenderClear)))
	    (p-size (foreign-slot-pointer viewport '(:struct ig::ImGuiViewport) 'ig::Size)))
	
	(setf (command-pool window) command-pool)
	
	(imgui-vulkan-create-or-resize-window device window
					      (foreign-slot-value p-size '(:struct ig::ImVec2) 'ig::x)
					      (foreign-slot-value p-size '(:struct ig::ImVec2) 'ig::y)
					      format
					      present-mode)			     
			     
	(setf (foreign-slot-value (platform-user-data viewport) '(:struct ImGuiViewportDataVulkan) 'WindowOwned) t)
      
	(values)))))

(defun imgui-vulkan-create-or-resize-window (device window width height format present-mode)
  (let ((swapchain (create-swapchain device window width height format
				     present-mode)))
    (setup-framebuffers device (render-pass swapchain) swapchain)
    (create-frame-resources swapchain (queue-family-index (render-surface window)))
    (values)))    

(defun vulkan-create-window (window width height)
  ;;(vkDeviceWaitIdle (h (default-logical-device *app*)))
  (recreate-swapchain window (swapchain window) width height))

(defcallback imgui-vulkan-destroy-window-callback :void
    ((viewport :pointer))
  (let ((window (get-viewport-window viewport)))
    (when window
      (vulkan-destroy-window window)))
  (values))

(defun vulkan-destroy-window (window)
  (when (swapchain window)
    (destroy-swapchain (swapchain window)))
  (vkDestroySurfaceKHR (h *vulkan-instance*) (h (render-surface window)) (h (allocator (render-surface window))))
  (values))

(defun imgui-vulkan-render (imgui viewport command-buffer frame-index)
  ;;(ig:igRender)
  (imgui-vulkan-render-draw-data
   imgui (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::DrawData)
   command-buffer frame-index)
  (values))
