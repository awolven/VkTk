(in-package :vktk)

(defmacro with-vertex-input-binding-description ((var)
						 &body body)
  
  `(with-vk-struct (,var VkVertexInputBindingDescription)
     
     ,@body))

(defmacro with-pipeline-vertex-input-state-create-info ((var)
							&body body)
  `(with-vk-struct (,var VkPipelineVertexInputStateCreateInfo)
     
     ,@body))

(defmacro with-pipeline-input-assembly-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineInputAssemblyStateCreateInfo)
     ,@body))

(defmacro with-viewport-structure ((var) &body body)
  `(with-vk-struct (,var VkViewport)
     ,@body))

(defmacro with-scissor-structure ((var) &body body)
  `(with-vk-struct (,var VkRect2D)
     ,@body))

(defmacro with-pipeline-viewport-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineViewportStateCreateInfo)
     ,@body))

(defmacro with-pipeline-rasterization-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineRasterizationStateCreateInfo)
     
     ,@body))

(defmacro with-pipeline-multisample-state-create-info ((var) &body body)

  `(with-vk-struct (,var VKPipelineMultisampleStateCreateInfo)
     ,@body))

(defmacro with-graphics-pipeline-create-info ((var) &body body)
  `(with-vk-struct (,var VkGraphicsPipelineCreateInfo)
     ,@body))

(defmacro with-pipeline-depth-stencil-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineDepthStencilStateCreateInfo)
     ,@body))

(defmacro with-descriptor-set-layouts ((var count) &body body)
  `(with-foreign-object (,var 'VkDescriptorSetLayout ,count)
     ,@body))

(defmacro with-pipeline-layout-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineLayoutCreateInfo)
     ,@body))

(defmacro with-pipeline-dynamic-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineDynamicStateCreateInfo)
     ,@body))

(defmacro with-dynamic-states ((var count) &body body)
  `(with-foreign-object (,var 'VkDynamicState ,count)
     ,@body))

(defmacro with-pipeline-color-blend-state-create-info ((var) &body body)
  `(with-vk-struct (,var VkPipelineColorBlendStateCreateInfo)
     ,@body))

(defmacro api-version (major minor patch)
  `(logior (ash ,major 22) (ash ,minor 12) ,patch))
