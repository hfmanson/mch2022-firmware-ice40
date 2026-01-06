#!/bin/sh

# Usage: ./hex-to-bin-le.sh input.txt output.bin

sed 's/\(..\)\(..\)/\2\1/' "$1" | xxd -r -p > "$2"
