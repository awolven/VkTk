#!/bin/bash
set -x
/cygdrive/c/VulkanSDK/1.1.97.0/Bin32/glslangValidator.exe -V shader.vert
/cygdrive/c/VulkanSDK/1.1.97.0/Bin32/glslangValidator.exe -V shader.frag
/cygdrive/c/VulkanSDK/1.1.97.0/Bin32/glslangValidator.exe -V shader.selection.geom -o selection.geom.spv
/cygdrive/c/VulkanSDK/1.1.97.0/Bin32/glslangValidator.exe -V shader.comp
/cygdrive/c/VulkanSDK/1.1.97.0/Bin32/glslangValidator.exe -V ray-tri.comp -o ray-tri.spv
/cygdrive/c/VulkanSDK/1.1.97.0/Bin32/glslangValidator.exe -V lines.vert -o lines.vert.spv
/cygdrive/c/VulkanSDK/1.1.97.0/Bin32/glslangValidator.exe -V lines.frag -o lines.frag.spv
