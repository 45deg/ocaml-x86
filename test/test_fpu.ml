open Ocaml_x86

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

let () =
  let ieee size bits =
    let bytes = Bytes.create 8 in
    Bytes.set_int64_le bytes 0 bits;
    Bytes.sub_string bytes 0 size
  in
  let cases =
    [
      (4, 0x7f800000L, 0x8000000000000000L, 0x7fff, 0x7f800000L);
      (4, 0xff800000L, 0x8000000000000000L, 0xffff, 0xff800000L);
      (4, 0x7fc12345L, 0xc123450000000000L, 0x7fff, 0x7fc12345L);
      (4, 0xffc12345L, 0xc123450000000000L, 0xffff, 0xffc12345L);
      (4, 0x7f800001L, 0xc000010000000000L, 0x7fff, 0x7fc00001L);
      (4, 0xff800001L, 0xc000010000000000L, 0xffff, 0xffc00001L);
      (8, 0x7ff0000000000000L, 0x8000000000000000L, 0x7fff, 0x7ff0000000000000L);
      (8, 0xfff0000000000000L, 0x8000000000000000L, 0xffff, 0xfff0000000000000L);
      (8, 0x7ff8000000000001L, 0xc000000000000800L, 0x7fff, 0x7ff8000000000001L);
      (8, 0xfff8000000000001L, 0xc000000000000800L, 0xffff, 0xfff8000000000001L);
      (8, 0x7ff0000000000001L, 0xc000000000000800L, 0x7fff, 0x7ff8000000000001L);
      (8, 0xfff0000000000001L, 0xc000000000000800L, 0xffff, 0xfff8000000000001L);
    ]
  in
  List.iter
    (fun (size, bits, significand, exponent, expected_bits) ->
      let input = ieee size bits in
      let expected = ieee size expected_bits in
      let extended = Extended.of_ieee input in
      assert (String.sub extended 0 8 = ieee 8 significand);
      assert (Char.code extended.[8] = exponent land 255);
      assert (Char.code extended.[9] = exponent lsr 8);
      List.iter
        (fun control ->
          assert (Extended.to_ieee ~control ~size extended = expected))
        [ 0x37f; 0x77f; 0xb7f; 0xf7f ];
      let memory = Memory.create ~size:16384 in
      let opcode = if size = 4 then "\xd9" else "\xdd" in
      Memory.load memory ~address:0l
        (opcode ^ "\x05\x00\x10\x00\x00" ^ opcode
       ^ "\x1d\x00\x20\x00\x00\xdf\xe0\xf4");
      Memory.load memory ~address:0x1000l input;
      let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
      assert (Cpu.run cpu ~max_steps:4 = Halted);
      assert (
        Int32.logand (Cpu.register cpu Eax) 1l
        = if bits = expected_bits then 0l else 1l);
      for index = 0 to size - 1 do
        assert (
          Memory.read_u8 memory ~address:(Int32.of_int (0x2000 + index))
          = Char.code expected.[index])
      done)
    cases;
  List.iter
    (fun input ->
      let fpu = Fpu.create () in
      Fpu.set_control fpu 0x37e;
      (match Fpu.load_ieee fpu input with
      | _ ->
          failwith "Unmasked signaling NaN must remain explicitly unsupported"
      | exception Extended.Unsupported "x87 unmasked signaling NaN load" -> ());
      assert (Fpu.status fpu = 0))
    [ ieee 4 0x7f800001L; ieee 8 0x7ff0000000000001L ];
  (match
     Extended.binary ~control:0x37f ~operation:0
       (Extended.of_ieee (ieee 8 0x7ff0000000000000L))
       (Extended.of_int64 1L)
   with
  | _ -> failwith "Nonfinite arithmetic must remain explicitly unsupported"
  | exception Extended.Unsupported _ -> ());
  print_endline
    "x87 NaN/infinity encodings, FLD/FSTP and masked invalid flag passed"

let () =
  let ieee size bits =
    let bytes = Bytes.create 8 in
    Bytes.set_int64_le bytes 0 bits;
    Bytes.sub_string bytes 0 size
  in
  List.iter
    (fun (size, bits) ->
      let input = ieee size bits in
      let value = Extended.of_ieee input in
      List.iter
        (fun control -> assert (Extended.to_ieee ~control ~size value = input))
        [ 0x37f; 0x77f; 0xb7f; 0xf7f ])
    [
      (4, 1L);
      (4, 0x80000001L);
      (4, 0x007fffffL);
      (4, 0x807fffffL);
      (8, 1L);
      (8, 0x8000000000000001L);
      (8, 0x000fffffffffffffL);
      (8, 0x800fffffffffffffL);
    ];
  List.iter
    (fun (size, sign) ->
      let smallest = Extended.of_ieee (ieee size 1L) in
      let half =
        Extended.binary ~control:0x37f ~operation:6 smallest
          (Extended.of_int64 2L)
      in
      let negative_half =
        Extended.binary ~control:0x37f ~operation:1 half
          (Extended.of_int64 (-1L))
      in
      List.iter
        (fun (control, positive_bits, negative_bits) ->
          assert (Extended.to_ieee ~control ~size half = ieee size positive_bits);
          assert (
            Extended.to_ieee ~control ~size negative_half
            = ieee size negative_bits))
        [
          (0x37f, 0L, sign);
          (0x77f, 0L, Int64.logor sign 1L);
          (0xb7f, 1L, sign);
          (0xf7f, 0L, sign);
        ];
      let above_half =
        Extended.binary ~control:0x37f ~operation:0 half
          (Extended.binary ~control:0x37f ~operation:6 smallest
             (Extended.of_int64 4096L))
      in
      assert (Extended.to_ieee ~control:0x37f ~size above_half = ieee size 1L);
      let normal_bits = if size = 4 then 0x00800000L else 0x0010000000000000L in
      let largest_subnormal = Int64.pred normal_bits in
      let midpoint =
        Extended.binary ~control:0x37f ~operation:6
          (Extended.binary ~control:0x37f ~operation:0
             (Extended.of_ieee (ieee size normal_bits))
             (Extended.of_ieee (ieee size largest_subnormal)))
          (Extended.of_int64 2L)
      in
      assert (
        Extended.to_ieee ~control:0x37f ~size midpoint = ieee size normal_bits);
      assert (
        Extended.to_ieee ~control:0x77f ~size midpoint
        = ieee size largest_subnormal))
    [ (4, 0x80000000L); (8, 0x8000000000000000L) ];
  print_endline
    "x87 subnormal transfers, signed underflow and rounding tests passed"
