;; Copyright 2019 Andrew Kenneth Wolven
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
