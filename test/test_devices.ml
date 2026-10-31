open Ocaml_x86

let () =
  let output = Buffer.create 16 in
  let devices = Devices.create ~output:(Buffer.add_char output) in
  let write port value = Devices.write devices ~port ~width:Byte (Int32.of_int value) in
  let read port = Int32.to_int (Devices.read devices ~port ~width:Byte) in
  write 0x20 0x11;
  write 0x21 0x20;
  write 0x21 4;
  write 0x21 1;
  write 0x21 0xfe;
  write 0x43 0x34;
  write 0x40 100;
  write 0x40 0;
  Devices.advance devices ~cycles:10000;
  assert (Devices.interrupt devices = Some 0x20);
  Devices.advance devices ~cycles:10000;
  assert (Devices.interrupt devices = None);
  write 0x20 0x20;
  assert (Devices.interrupt devices = Some 0x20);
  write 0x20 0x20;
  write 0x21 0xef;
  write 0x3fb 0x80;
  write 0x3f8 12;
  assert (read 0x3f8 = 12);
  assert (Buffer.length output = 0);
  write 0x3fb 3;
  write 0x3f8 65;
  assert (Buffer.contents output = "A");
  write 0x3fc 8;
  write 0x3f9 1;
  Devices.input devices "Z";
  assert (Devices.interrupt devices = Some 0x24);
  assert (read 0x3fd land 1 = 1);
  assert (read 0x3f8 = 90);
  assert (read 0x3fd land 1 = 0);
  Devices.write devices ~port:0xcf8 ~width:Dword 0x80001000l;
  assert (Devices.read devices ~port:0xcf8 ~width:Dword = 0x80001000l);
  assert (Devices.read devices ~port:0xcfc ~width:Dword = Int32.minus_one)
;;
