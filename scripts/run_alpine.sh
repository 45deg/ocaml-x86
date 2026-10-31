#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
exec dune exec ocaml-x86 -- \
  --kernel fixtures/alpine/vmlinuz-lts \
  --initrd fixtures/alpine/initramfs.cpio.gz \
  --memory-mib 512 \
  --append 'console=ttyS0 earlyprintk=serial,ttyS0 nokaslr rdinit=/init' \
  --steps 18000000000 --interactive "$@"
