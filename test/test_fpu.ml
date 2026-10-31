open Ocaml_os

let () =
  let fpu = Fpu.create () in
  assert (Fpu.control fpu = 0x37f);
  assert (Fpu.status fpu = 0);
  List.iter
    (fun width ->
       let size = if width = Width.Word then 94 else 108 in
       let saved = Bytes.init size (fun index -> Char.chr (index * 17 land 255)) in
       (* Keep opcode's reserved bits zero; TOP=5 exercises logical register order. *)
       if width = Width.Word
       then Bytes.set saved 3 '\040'
       else (
         Bytes.set saved 5 '\040';
         Bytes.set saved 19 '\003');
       Fpu.restore fpu ~operand_size:width (Bytes.to_string saved);
       let first = Fpu.save fpu ~operand_size:width in
       Fpu.reset fpu;
       Fpu.restore fpu ~operand_size:width first;
       assert (Fpu.save fpu ~operand_size:width = first);
       let header = size - 80 in
       assert (String.sub first header 80 = Bytes.sub_string saved header 80))
    [ Width.Word; Dword ];
  let memory = Memory.create ~size:8192 in
  Memory.load
    memory
    ~address:0l
    "\xdb\xe3\xd9\x3d\x00\x10\x00\x00\xdd\x35\x10\x10\x00\x00\xdd\x25\x10\x10\x00\x00\xdf\xe0\xf4";
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  assert (Cpu.run cpu ~max_steps:6 = Halted);
  assert (Memory.read_u16 memory ~address:0x1000l = 0x37f);
  assert (Memory.read_u16 memory ~address:0x1018l = 0xffff);
  assert (Cpu.register cpu Eax = 0l);
  print_endline "x87 environment and save/restore tests passed"
;;

let () =
  let negative_zero = "\x00\x00\x00\x00\x00\x00\x00\x80" in
  let value = Extended.of_ieee negative_zero in
  assert (Extended.to_ieee ~control:0x37f ~size:8 value = negative_zero);
  let product =
    Extended.binary ~control:0x37f ~operation:1 value (Extended.of_int64 2L)
  in
  assert (Extended.to_ieee ~control:0x37f ~size:8 product = negative_zero);
  let three = Extended.of_int64 3L
  and two = Extended.of_int64 2L in
  let half = Extended.binary ~control:0x37f ~operation:6 three two in
  assert (Extended.to_integer ~control:0x37f ~bits:32 half = "\x02\x00\x00\x00");
  assert (Extended.to_integer ~control:0xf7f ~bits:32 half = "\x01\x00\x00\x00")
;;

let () =
  let memory = Memory.create ~size:8192 in
  Memory.load memory ~address:0l "\xb8\x19\x00\x00\x00\x0f\x22\xc0\xdb\xe3";
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  (match Cpu.run cpu ~max_steps:3 with
   | Fault (System_fault { eip = 8l; message = "Guest exception #7" }) -> ()
   | _ -> failwith "TS must raise #NM before FNINIT");
  Memory.load memory ~address:0l "\xdb\xe3\xdd\x35\xfc\x1f\x00\x00";
  Memory.write_u32 memory ~address:0x1ffcl 0x12345678l;
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  (match Cpu.run cpu ~max_steps:2 with
   | Fault (Memory_fault _) -> ()
   | _ -> failwith "FNSAVE crossing RAM must fault");
  assert (Memory.read_u32 memory ~address:0x1ffcl = 0x12345678l)
;;
