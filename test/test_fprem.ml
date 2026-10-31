open Ocaml_x86

let check condition message = if not condition then failwith message

let () =
  for quotient = 0 to 7 do
    List.iter
      (fun (left_sign, right_sign) ->
        let left = Int64.of_int (left_sign * ((quotient * 3) + 1)) in
        let right = Int64.of_int (right_sign * 3) in
        let fpu = Fpu.create () in
        Fpu.push fpu (Extended.of_int64 right);
        Fpu.push fpu (Extended.of_int64 left);
        let before = Fpu.status fpu land lnot 0x4700 in
        Fpu.remainder fpu;
        check
          (Extended.compare (Fpu.get fpu 0)
             (Extended.of_int64 (Int64.of_int left_sign))
          = 0)
          "FPREM truncates toward zero and preserves the dividend sign";
        check
          (Extended.compare (Fpu.get fpu 1) (Extended.of_int64 right) = 0)
          "FPREM retains ST(1)";
        let flags =
          ((quotient land 4) lsl 6)
          lor ((quotient land 2) lsl 13)
          lor ((quotient land 1) lsl 9)
        in
        check
          (Fpu.status fpu = before lor flags)
          "Completed quotient flags and TOP")
      [ (1, 1); (-1, 1); (1, -1); (-1, -1) ]
  done;
  let truncated =
    Extended.remainder (Extended.of_int64 7L) (Extended.of_int64 2L)
  in
  check
    (Extended.compare truncated.value (Extended.of_int64 1L) = 0
    && truncated.quotient_bits = 3)
    "FPREM uses truncation rather than nearest-even";
  let negative_zero = "\x00\x00\x00\x00\x00\x00\x00\x80" in
  let result =
    Extended.remainder (Extended.of_int64 (-6L)) (Extended.of_int64 3L)
  in
  check
    (Extended.to_ieee ~control:0x37f ~size:8 result.value = negative_zero)
    "FPREM exact zero retains the dividend sign";
  let fpu = Fpu.create () in
  Fpu.push fpu (Extended.of_int64 3L);
  Fpu.push fpu
    (Extended.encode ~control:0x37f (Q.of_bigint (Z.shift_left Z.one 300)));
  Fpu.remainder fpu;
  check
    (Fpu.status fpu land 0x400 <> 0)
    "Large exponent difference needs reduction";
  let iterations = ref 1 in
  while Fpu.status fpu land 0x400 <> 0 && !iterations < 20 do
    Fpu.remainder fpu;
    incr iterations
  done;
  check (!iterations > 1 && !iterations < 20) "FPREM reduction converges";
  check
    (Extended.compare (Fpu.get fpu 0) (Extended.of_int64 1L) = 0)
    "Exact repeated reduction of 2^300 modulo 3";
  check
    (Fpu.status fpu land 0x4700 = 0x300)
    "Repeated reduction retains the final quotient bits";
  List.iter
    (fun exponent ->
      let value =
        Extended.encode ~control:0x37f
          (Q.of_bigint (Z.shift_left Z.one exponent))
      in
      let result = Extended.remainder value (Extended.of_int64 1L) in
      check (result.complete = (exponent = 63)) "FPREM 63/64 exponent boundary")
    [ 63; 64 ];
  let exact = Extended.of_int64 (Int64.succ (Int64.shift_left 1L 60)) in
  let fpu = Fpu.create () in
  Fpu.set_control fpu 0x47f;
  Fpu.push fpu (Extended.of_int64 (Int64.shift_left 1L 61));
  Fpu.push fpu exact;
  Fpu.remainder fpu;
  check (Fpu.get fpu 0 = exact) "FPREM ignores PC=24 and directed rounding";
  let fpu = Fpu.create () in
  Fpu.push fpu (Extended.of_int64 0L);
  Fpu.push fpu (Extended.of_int64 7L);
  let saved = Fpu.save fpu ~operand_size:Dword in
  (match Fpu.remainder fpu with
  | () -> failwith "Zero divisor must remain explicitly unsupported"
  | exception Extended.Unsupported "x87 FPREM zero divisor" -> ());
  check
    (Fpu.save fpu ~operand_size:Dword = saved)
    "Rejected FPREM leaves FPU state intact";
  let memory = Memory.create ~size:8192 in
  (* FILD divisor; FILD dividend; FPREM; FNSTSW AX; test C2; repeat; FSTP. *)
  Memory.write_u32 memory ~address:0x1000l 3l;
  Memory.write_u32 memory ~address:0x1004l (-16l);
  Memory.load memory ~address:0l
    "\xdb\x05\x00\x10\x00\x00\xdb\x05\x04\x10\x00\x00\xd9\xf8\xdf\xe0\xf6\xc4\x04\x75\xf7\xdd\x1d\x08\x10\x00\x00\xf4";
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  check (Cpu.run cpu ~max_steps:20 = Halted) "musl FPREM status loop decodes";
  check
    (Memory.read_u32 memory ~address:0x100cl = 0xbff00000l)
    "musl loop stores -1.0";
  check
    (Int32.logand (Cpu.register cpu Eax) 0x4700l = 0x300l)
    "FNSTSW exposes quotient bits";
  print_endline
    "FPREM signs, quotient flags, partial reduction and musl loop passed"
