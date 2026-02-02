#!/bin/bash
# Smeddum AGI - Universal Compiler Script

echo "--- Starting Smeddum Forge Build ---"
mkdir -p binaries

for file in *.pas; do
    echo -n "Compiling $file... "
    fpc -O3 "$file" > /dev/null 2>&1
    
    if [ $? -eq 0 ]; then
        echo "SUCCESS"
        # Optional: Move the executable to a binaries folder to keep things clean
        mv "${file%.pas}" binaries/ 2>/dev/null
    else
        echo "FAILED"
    fi
done

# Clean up the clutter (object files and units)
rm -f *.o *.ppu

echo "--- Build Complete. Binaries are in the /binaries folder ---"
