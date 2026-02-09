#!/bin/bash
# scripts/forge_build.sh
mkdir -p build

CPU_MODEL=$(grep "model name" /proc/cpuinfo | head -1)

if [[ "$CPU_MODEL" == *"Cortex-A76"* ]]; then
    echo "Grounding on Pi 5 substrate..."
    ARCH_FLAGS="-march=armv8.2-a+fp16+dotprod"
else
    echo "Grounding on Pi 4 substrate..."
    ARCH_FLAGS="-march=armv8-a+crc -mtune=cortex-a72"
fi

# 1. Forge the C++ Topology Engine
g++ -O3 $ARCH_FLAGS -shared -fPIC \
    muscle/src/topology.cpp -o build/libmuscle.so -lgudhi

# 2. Forge the Pascal Grid Utilities (if present)
if [ -f "muscle/src/grid_math.pas" ]; then
    fpc -O3 -PiAARCH64 muscle/src/grid_math.pas -FEbuild/
fi

echo "Muscle Forge Complete."
