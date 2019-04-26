#!/bin/bash
set -x
/cygdrive/c/VulkanSDK/1.1.97.0/Bin32/glslangValidator.exe -V shader.vert

/cygdrive/c/VulkanSDK/1.1.97.0/Bin32/glslangValidator.exe -V shader.frag

/cygdrive/c/VulkanSDK/1.1.97.0/Bin32/glslangValidator.exe -V shader.selection.geom -o selection.geom.spv
