#!/bin/bash

set -x
echo " BUILD main.exe"
dune build --profile=release ./main.exe
echo " DESCR main.exe"
dune describe location --context solo5 --no-print-directory ./main.exe &> unikernel.path
UNIKERNEL=$(cat unikernel.path)
echo " STRIP $UNIKERNEL"
strip $UNIKERNEL -o blame.hvt
cat >blame.install<<EOF
bin: [
  "blame.hvt"
]
EOF
