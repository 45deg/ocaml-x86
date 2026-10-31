open Ocaml_x86

let () =
  let bytes = Bytes.make 260 '\000' in
  let put offset value =
    for index = 0 to 3 do
      Bytes.set bytes (offset + index) (Char.chr ((value lsr (index * 8)) land 255))
    done
  in
  Bytes.blit_string "\x7fELF\x01\x01\x01" 0 bytes 0 7;
  put 16 0x30002;
  put 24 0xc0100000;
  put 28 52;
  put 40 0x200034;
  put 44 1;
  put 52 1;
  put 56 256;
  put 60 0xc0100000;
  put 64 0x100000;
  put 68 4;
  put 72 8;
  put 76 5;
  Bytes.blit_string "CODE" 0 bytes 256 4;
  let memory = Memory.create ~size:0x200000 in
  Memory.load memory ~address:0x100000l "XXXXXXXX";
  let load () = Elf_loader.load ~memory ~image:(Bytes.to_string bytes) ~initrd_start:0l in
  assert (load () = 0x100000l);
  assert (Memory.read_u32 memory ~address:0x100000l = 0x45444f43l);
  assert (Memory.read_u32 memory ~address:0x100004l = 0l);
  let rejects () =
    try
      ignore (load ());
      assert false
    with
    | Linux_boot.Invalid_image _ -> ()
  in
  put 72 2;
  rejects ();
  put 72 8;
  put 24 0xc0100004;
  rejects ();
  put 24 0xc0100000;
  put 64 0x1ffffc;
  rejects ();
  assert (Memory.read_u32 memory ~address:0x100000l = 0x45444f43l)
;;
