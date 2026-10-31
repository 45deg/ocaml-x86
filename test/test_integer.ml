open Ocaml_os

let check b message = if not b then failwith message
let word a b = check (a = b) (Printf.sprintf "%lx <> %lx" a b)
let signed8 n = if n < 128 then n else n - 256

let () =
  for left = 0 to 255 do
    for right = 0 to 255 do
      List.iter
        (fun signed ->
           let product =
             Multiply.multiply
               ~width:Byte
               ~signed
               (Int32.of_int left)
               (Int32.of_int right)
           in
           let expected = if signed then signed8 left * signed8 right else left * right in
           word product.low (Int32.of_int (expected land 255));
           word product.high (Int32.of_int ((expected lsr 8) land 255));
           check
             (product.overflow
              = if signed then expected < -128 || expected > 127 else expected > 255)
             "Multiply overflow")
        [ false; true ]
    done
  done;
  let q, r = Multiply.divide ~width:Dword ~signed:false ~high:1l ~low:0l 3l in
  word q 0x55555555l;
  word r 1l;
  let q, r = Multiply.divide ~width:Dword ~signed:true ~high:(-1l) ~low:(-7l) 3l in
  word q (-2l);
  word r (-1l);
  List.iter
    (fun (high, low, divisor) ->
       match Multiply.divide ~width:Dword ~signed:false ~high ~low divisor with
       | _ -> failwith "Expected #DE"
       | exception Multiply.Divide_error -> ())
    [ 0l, 1l, 0l; 1l, 0l, 1l ];
  let memory = Memory.create ~size:65536 in
  let run code setup =
    Memory.load memory ~address:0l code;
    let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
    Cpu.set_register cpu Esp 0xe000l;
    setup cpu;
    check (Cpu.run cpu ~max_steps:100 = Halted) "Integer guest";
    cpu
  in
  let cpu =
    run "\xb8\x07\x00\x00\x00\xb9\x03\x00\x00\x00\xf7\xe1\xf7\xf1\xf4" (fun _ -> ())
  in
  word (Cpu.register cpu Eax) 7l;
  word (Cpu.register cpu Edx) 0l;
  let cpu =
    run "\x60\x31\xc0\x31\xc9\x61\xf4" (fun cpu ->
      Cpu.set_register cpu Eax 123l;
      Cpu.set_register cpu Ecx 456l)
  in
  word (Cpu.register cpu Eax) 123l;
  word (Cpu.register cpu Ecx) 456l;
  word (Cpu.register cpu Esp) 0xe000l;
  let cpu =
    run "\x67\x8b\x40\xfe\xf4" (fun cpu ->
      Cpu.set_register cpu Ebx 0xdead4000l;
      Cpu.set_register cpu Esi 0xbeef0004l;
      Memory.write_u32 memory ~address:0x4002l 0x12345678l)
  in
  word (Cpu.register cpu Eax) 0x12345678l;
  let cpu =
    run "\x67\xf3\xa4\xf4" (fun cpu ->
      Cpu.set_register cpu Ecx 0xaaaa0002l;
      Cpu.set_register cpu Esi 0xbbbb4000l;
      Cpu.set_register cpu Edi 0xcccc4100l;
      Memory.load memory ~address:0x4000l "AB")
  in
  word (Cpu.register cpu Ecx) 0xaaaa0000l;
  word (Cpu.register cpu Esi) 0xbbbb4002l;
  check (Memory.read_u16 memory ~address:0x4100l = 0x4241) "16-bit address REP";
  let cpu =
    run "\x0f\xab\x0b\x0f\xa3\x0b\xf4" (fun cpu ->
      Cpu.set_register cpu Ebx 0x4204l;
      Cpu.set_register cpu Ecx (-1l);
      Memory.write_u32 memory ~address:0x4200l 0l)
  in
  word (Memory.read_u32 memory ~address:0x4200l) Int32.min_int;
  check (Int32.logand (Cpu.eflags cpu) 1l = 1l) "Negative memory bit index";
  let cpu = run "\x68\x2a\x00\x00\x00\x8f\x04\x24\xf4" (fun _ -> ()) in
  word (Cpu.register cpu Esp) 0xe000l;
  word (Memory.read_u32 memory ~address:0xe000l) 42l;
  let cpu = run "\xb9\x03\x00\x00\x00\x40\xe2\xfd\xf4" (fun _ -> ()) in
  word (Cpu.register cpu Eax) 3l;
  print_endline "Integer families, stack and address-size tests passed"
;;
