#!/bin/bash

dune build --profile=release
cp _build/solo5/main.exe blame.hvt
chmod +w blame.hvt
strip blame.hvt
solo5-hvt --mem=256 --net:service=tap0 --block:archive=pack.pack -- \
  blame.hvt --ipv4=10.0.0.2/24 --color=always
