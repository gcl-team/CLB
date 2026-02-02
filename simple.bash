#!/bin/bash

# Check if an input file was provided
if [ -z "$1" ]; then
    echo "Usage: ./simple.bash <input_file.clb>"
    echo "Example: ./simple.bash programs/simple.clb"
    exit 1
fi

INPUT_FILE=$1
# Get the filename without path and extension for the output names
BASE_NAME=$(basename "$INPUT_FILE" .clb)
OUTPUT_BAS="build/${BASE_NAME}.bas"
OUTPUT_PRG="build/${BASE_NAME}.prg"

# Ensure build directory exists
mkdir -p build

# 1. Run your Prolog Transpiler (CLB -> BASIC Text)
echo "Compiling $INPUT_FILE to BASIC..."
swipl -s src/compiler.pl -g "compile_file('$INPUT_FILE', '$OUTPUT_BAS'), halt"

# Check if compilation succeeded
if [ $? -ne 0 ]; then
    echo "ERROR: Prolog compilation failed."
    exit 1
fi

# 2. Convert BASIC Text to C64 Binary (using petcat)
# Check if petcat is installed
if ! command -v petcat &> /dev/null; then
    echo "----------------------------------------"
    echo "WARNING: 'petcat' (part of VICE) not found."
    echo "PRG creation skipped. Only BASIC source created."
    echo ""
    echo "To install petcat:"
    echo "  macOS:  brew install vice"
    echo "  Ubuntu: sudo apt-get install vice"
    echo "----------------------------------------"
    echo "BASIC source is available at: $OUTPUT_BAS"
    exit 0
fi

echo "Converting to C64 PRG..."
petcat -w2 -o "$OUTPUT_PRG" -- "$OUTPUT_BAS"

if [ $? -eq 0 ]; then
    echo "----------------------------------------"
    echo "SUCCESS: $OUTPUT_PRG created!"
    echo "Now copy it to your Commodore 64."
    echo "----------------------------------------"
else
    echo "ERROR: petcat conversion failed."
    exit 1
fi