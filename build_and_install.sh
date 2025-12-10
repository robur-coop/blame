#!/bin/bash

set -x
echo " BUILD main.exe"
dune build -p blame --profile=release --root . ./main.exe
echo " DESCR main.exe"
dune describe location --context solo5 --no-print-directory --root . ./main.exe &> unikernel.path
UNIKERNEL=$(cat unikernel.path)
echo " STRIP $UNIKERNEL"
strip $UNIKERNEL -o blame.hvt
cat >blame.install<<EOF
bin: [
  "blame.hvt"
]
EOF
