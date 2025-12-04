#!/bin/bash

dune build --profile=release
solo5-hvt --mem=512 --net:service=tap0 --block:archive=pack.pack -- \
  _build/solo5/main.exe --ipv4=10.0.0.2/24 --color=always
