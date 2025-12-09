#!/bin/bash

dune describe location --context solo5 --no-print-directory ./main.exe &> unikernel.path
UNIKERNEL=$(cat unikernel.path)
strip $UNIKERNEL -o blame.hvt
