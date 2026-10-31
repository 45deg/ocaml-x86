#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
mode=native
prepare_only=false
while [ "$#" -gt 0 ]; do
  case "$1" in
    --native) mode=native; shift ;;
    --ocamlrun) mode=ocamlrun; shift ;;
    --prepare-only) prepare_only=true; shift ;;
    --help|-h)
      cat <<'EOF'
Usage: scripts/run_self_compile.sh [--native|--ocamlrun] [--prepare-only] [emulator options]

Build current sources inside the 32-bit Linux guest, then run the result.
  --native       Use ocamlopt and GNU as/ld (default).
  --ocamlrun     Use ocamlrun + ocamlc.byte; run the resulting bytecode.
  --prepare-only Create the initramfs without booting the guest.

Requires a prepared toolchain rootfs:
  native:   artifacts/selfcompile-native/rootfs.tar.gz
  ocamlrun: artifacts/selfcompile/rootfs.tar.gz
Set SELF_COMPILE_ROOTFS to use another compatible rootfs tarball.
Compiler/dependency libraries are prebuilt; project sources are refreshed.
Further options, e.g. --steps N, are passed to ocaml-x86.
After the test the guest opens a shell. Use host Ctrl-C to exit.
EOF
      exit 0 ;;
    --) shift; break ;;
    *) break ;;
  esac
done

case "$mode" in
  native) rootfs=artifacts/selfcompile-native/rootfs.tar.gz; ram=1024 ;;
  ocamlrun) rootfs=artifacts/selfcompile/rootfs.tar.gz; ram=512 ;;
esac
rootfs=${SELF_COMPILE_ROOTFS:-$rootfs}
output="artifacts/selfcompile-$mode-run"
python3 scripts/prepare_self_compile.py "$mode" "$rootfs" "$output"
if [ "$prepare_only" = true ]; then exit 0; fi

kernel=fixtures/alpine/vmlinuz-lts
if [ ! -f "$kernel" ]; then
  echo "Missing $kernel; see the Alpine setup in README.md." >&2
  exit 1
fi
# Refresh the extracted ELF too, in case the kernel fixture was replaced.
python3 scripts/extract_kernel.py "$kernel" "$output/vmlinux"
exec dune exec --profile release ocaml-x86 -- \
  --kernel "$kernel" --kernel-elf "$output/vmlinux" \
  --initrd "$output/initramfs.cpio.gz" --memory-mib "$ram" \
  --append 'console=ttyS0 earlyprintk=serial,ttyS0 nokaslr rdinit=/self-compile-start.sh' \
  --steps 60000000000 --progress-every 250000000 --interactive "$@"
