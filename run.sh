#!/usr/bin/env bash
set -euo pipefail

if [ $# -ne 2 ]; then
	echo "Usage: ./run <day> <easy|hard>"
	exit 1
fi

DAY="$1"
PART="$2"

DIR="$DAY"
BIN="_build/default/$DAY/${PART}.exe"
INPUT="$DAY/inputs/$PART"

if [ ! -d "$DIR" ]; then
	echo "Day '$DAY' does not exist"
	exit 1
fi

dune build "$BIN"

echo "Running day $DAY/$PART with $INPUT..."
"$BIN" < "$INPUT"
