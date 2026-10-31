#!/bin/sh
# Compiler and dependency archives are supplied by the prepared guest rootfs.
set -eu
mode=$1
case "$mode" in
  native) root=/native; suffix=cmx; archive=cmxa; result=ocaml-x86.native ;;
  ocamlrun) root=/selfcompile; suffix=cmo; archive=cma; result=ocaml-x86.byte ;;
  *) echo "Unknown compiler mode: $mode" >&2; exit 2 ;;
esac
export OCAMLLIB="$root/stdlib"
export PATH=/native/tools:/usr/bin:/bin:/usr/sbin:/sbin
export CAML_LD_LIBRARY_PATH="$root/stubs"
mkdir -p "$root/work"
cd "$root/work"
compiler() {
  if [ "$mode" = native ]; then
    set -- /native/ocamlopt.opt -cc /native/tools/link -no-alias-deps "$@"
  else
    set -- /usr/bin/ocamlrun "$root/ocamlc.byte" "$@"
  fi
  "$@" -I . -I "$root/deps/base/base_internalhash_types" \
    -I "$root/deps/base/caml" -I "$root/deps/base/shadow_stdlib" \
    -I "$root/deps/sexplib0" -I "$root/deps/base" -I "$root/deps/zarith"
}
echo "SELF_COMPILE_BEGIN=$mode"
compiler -version
objects=''
for unit in extended memory register width physical_bus operand multiply mmu linux_boot fpu elf_loader devices alu bit_operation shift system_state cpu; do
  echo "COMPILE_BEGIN=$unit"
  if [ "$mode" = native ]; then
    compiler -for-pack Ocaml_x86 -c -o "$unit.$suffix" "$root/src/lib/$unit.ml"
  else
    compiler -c -o "$unit.$suffix" "$root/src/lib/$unit.ml"
  fi
  objects="$objects $unit.$suffix"
  echo "COMPILE_OK=$unit"
done
# Intentional word splitting: objects contains only the fixed module names above.
compiler -pack -o "ocaml_x86.$suffix" $objects
echo PACK_OK
compiler -c -o "main.$suffix" "$root/src/bin/main.ml"
echo COMPILE_OK=main
compiler -o "$result" "base_internalhash_types.$archive" "caml.$archive" \
  "shadow_stdlib.$archive" "sexplib0.$archive" "base.$archive" \
  "zarith.$archive" "unix.$archive" "ocaml_x86.$suffix" "main.$suffix"
echo LINK_OK
sha256sum "$result"
if [ "$mode" = native ]; then
  od -An -tx1 -N20 "$result"
  "./$result" --memory-mib 32 --steps 100 --dump-state "$root/hello.bin"
else
  /usr/bin/ocamlrun "./$result" --memory-mib 32 --steps 100 --dump-state "$root/hello.bin"
fi
echo SELF_COMPILE_EXIT=0
echo SELF_COMPILE_DONE
