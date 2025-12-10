#!/bin/bash

dune describe location --context solo5 --no-print-directory ./main.exe &> unikernel.path
UNIKERNEL=$(cat unikernel.path)
echo " STRIP $UNIKERNEL"
strip $UNIKERNEL -o blame.hvt
cat >blame.install<<EOF
bin: [
  "blame.hvt"
]
