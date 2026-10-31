open Ocaml_x86

let check b message = if not b then failwith message

let kernel () =
  let bytes = Bytes.make 4096 '\000' in
  let byte offset value = Bytes.set bytes offset (Char.chr value) in
  let word offset value = Bytes.set_int32_le bytes offset (Int32.of_int value) in
  byte 0x1f1 4;
  byte 0x1fe 0x55;
  byte 0x1ff 0xaa;
  byte 0x201 0x66;
  Bytes.blit_string "HdrS" 0 bytes 0x202 4;
  byte 0x206 0x0f;
  byte 0x207 2;
  byte 0x211 1;
  word 0x1f4 96;
  word 0x230 0x100000;
  word 0x260 0x200000;
  word 0x238 4095;
  word 0x22c 0x37ffffff;
  byte 2560 0xf4;
  Bytes.to_string bytes
;;

let () =
  let memory = Memory.create ~size:(8 * 1024 * 1024) in
  let image =
    Linux_boot.load
      ~memory
      ~kernel:(kernel ())
      ~initrd:"ramdisk"
      ~command_line:"rdinit=/bin/sh"
  in
  check (image.entry = 0x100000l) "Kernel entry";
  check (Memory.read_u8 memory ~address:image.entry = 0xf4) "Payload";
  check (Memory.read_u32 memory ~address:0x10228l = 0x20000l) "Command line pointer";
  check (Memory.read_u8 memory ~address:0x101e8l = 3) "E820 count";
  check (Memory.read_u32 memory ~address:0x10300l = 0x700000l) "Extended RAM length";
  check (Memory.read_u32 memory ~address:0x10218l = 0x7ff000l) "Initrd placement";
  check (Memory.read_u8 memory ~address:0x7ff000l = Char.code 'r') "Initrd content";
  check (Memory.read_u32 memory ~address:0x1021cl = 7l) "Initrd size";
  let invalid image =
    match Linux_boot.load ~memory ~kernel:image ~initrd:"" ~command_line:"" with
    | _ -> failwith "Expected invalid image"
    | exception Linux_boot.Invalid_image _ -> ()
  in
  invalid "";
  let bytes = Bytes.of_string (kernel ()) in
  Bytes.set bytes 0x236 '\x01';
  invalid (Bytes.to_string bytes);
  Bytes.set bytes 0x236 '\x00';
  Bytes.set_int32_le bytes 0x260 0x10000000l;
  invalid (Bytes.to_string bytes);
  Bytes.set_int32_le bytes 0x260 0x200000l;
  Bytes.set_int32_le bytes 0x1f4 100000l;
  invalid (Bytes.to_string bytes);
  print_endline "Linux boot memory layout tests passed"
;;
