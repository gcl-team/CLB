#!/bin/bash

# 1. Run your Prolog Transpiler (CLB -> BASIC Text)
swipl -s src/compiler.pl -g "compile_file('programs/simple.clb', 'build/simple.bas'), halt"

# 2. Convert BASIC Text to C64 Binary (using petcat)
# This assumes petcat is in your PATH
petcat -w2 -o build/simple.prg -- build/simple.bas

echo "----------------------------------------"
echo "SUCCESS: build/simple.prg created!"
echo "Now copy it to your Commodore 64."
echo "----------------------------------------"