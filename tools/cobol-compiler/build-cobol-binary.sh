#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <source_file.cbl>"
    exit 1
fi

# Assign the first argument to SOURCE_FILE
SOURCE_FILE=$1

# Check if the source file exists
if [ ! -f "$SOURCE_FILE" ]; then
    echo "Source file not found: $SOURCE_FILE"
    exit 1
fi

# Extract the base name of the source file (without extension)
BASE_NAME=$(basename "$SOURCE_FILE" .cbl)

# Compile the COBOL source file
cobc -x -o "$BASE_NAME" "$SOURCE_FILE"

# Check if the compilation was successful
if [ $? -eq 0 ]; then
    echo "Compilation successful. Executable created: $BASE_NAME"
else
    echo "Compilation failed."
    exit 1
fi