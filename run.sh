#!/usr/bin/env bash
set -euo pipefail

if [ $# -ne 3 ]; then
	echo "Usage: ./run <day> <easy|hard> <input>"
	exit 1
fi

DAY="$1"
PART="$2"
INPUT="$3"

DIR="$DAY"
BIN="_build/default/$DAY/${PART}.exe"
INPUT="$DAY/inputs/$INPUT"

if [ ! -d "$DIR" ]; then
	echo "Day '$DAY' does not exist"
	exit 1
fi

dune build "$BIN"

echo "Running day $DAY/$PART with $INPUT..."
time "$BIN" < "$INPUT"
