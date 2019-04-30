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

(in-package :cl-user)

(defpackage :imgui-glfw-vulkan
  (:nicknames :vkappui)
  (:use)
  (:export
   #:IMGUI_VK_QUEUED_FRAMES
   #:ImGui_ImplGlfwVulkan_Init_Data
   #:ImGui_ImplGlfwVulkan_Init
   #:ImGui_ImplGlfwVulkan_Shutdown
   #:ImGui_ImplGlfwVulkan_NewFrame
   #:ImGui_ImplGlfwVulkan_Render
   #:ImGui_ImplGlfwVulkan_InvalidateFontUploadObjects
   #:ImGui_ImplGlfwVulkan_InvalidateDeviceObjects
   #:ImGui_ImplGlfwVulkan_CreateFontsTexture
   #:ImGui_ImplGlfwVulkan_CreateDeviceObjects
   #:ImGui_ImplGlfwVulkan_MouseButtonCallback
   #:ImGui_ImplGlfwVulkan_ScrollCallback
   #:ImGui_ImplGlfwVulkan_KeyCallback
   #:ImGui_ImplGlfwVulkan_CharCallback))
