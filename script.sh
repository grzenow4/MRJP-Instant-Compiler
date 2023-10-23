#!/bin/bash

EXAMPLES_DIIR="instant231015/examples"

if [ "$1" == "clean" ]; then
    rm "$EXAMPLES_DIIR"/*.bc
    rm "$EXAMPLES_DIIR"/*.ll
    rm "$EXAMPLES_DIIR"/*.class
    rm "$EXAMPLES_DIIR"/*.j
elif [ "$1" == "exec" ]; then
    echo "Executing files with LLVM"
    echo "========================="
    for file in "$EXAMPLES_DIIR"/*.bc; do
        filename="${file%.*}"
        echo "Executing file ${filename}"
        lli "$file"
    done
    echo "Executing files with JVM"
    echo "========================"
    for file in "$EXAMPLES_DIIR"/*.class; do
        filename=$(basename "$file")
        echo "Executing file ${filename}"
        java -cp "$EXAMPLES_DIIR" "${filename%.*}"
    done
else
    for file in "$EXAMPLES_DIIR"/*.ins; do
        echo "Compiling file ${file}"
        ./insc_llvm ${file}
        ./insc_jvm ${file}
    done
fi
