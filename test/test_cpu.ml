open Ocaml_os

let check condition message = if not condition then failwith message

let word actual expected =
  check
    (Int32.equal actual expected)
    (Printf.sprintf "Expected %lx, got %lx" expected actual)
;;

let machine ?(size = 4096) program =
  let memory = Memory.create ~size in
  Memory.load memory ~address:0l program;
  let output = Buffer.create 16 in
  let cpu =
    Cpu.create ~memory ~output_u8:(fun ~port value ->
      check (port = 0x3f8) "COM1 port";
      Buffer.add_char output (Char.chr value))
  in
  Cpu.set_register cpu Esp (Int32.of_int size);
  cpu, memory, output
;;

let halted cpu = check (Cpu.run cpu ~max_steps:100 = Halted) "Expected HLT"

let () =
  let memory = Memory.create ~size:16 in
  word (Memory.read_u32 memory ~address:0l) 0l;
  Memory.write_u32 memory ~address:1l 0xdead_beefl;
  check (Memory.read_u8 memory ~address:1l = 0xef) "Little endian";
  word (Memory.read_u32 memory ~address:1l) 0xdead_beefl;
  List.iter
    (fun address ->
       match Memory.write_u32 memory ~address 0l with
       | () -> failwith "Expected RAM boundary fault"
       | exception Memory.Access_fault _ -> ())
    [ 13l; -1l; Int32.min_int ];
  word (Memory.read_u32 memory ~address:1l) 0xdead_beefl;
  let sum = Alu.add (-1l) 1l in
  word sum.value 0l;
  word sum.flags 0x57l;
  let sum = Alu.add 0x7fff_ffffl 1l in
  word sum.value Int32.min_int;
  word sum.flags 0x896l;
  let difference = Alu.sub 0l 1l in
  word difference.value (-1l);
  word difference.flags 0x97l;
  let difference = Alu.sub Int32.min_int 1l in
  word difference.flags 0x816l;
  let cpu, _, output = machine "\xba\xf8\x03\x00\x00\xb0O\xee\xb0K\xee\xf4" in
  halted cpu;
  check (Buffer.contents output = "OK") "Guest serial output";
  let cpu, _, _ = machine "\xb8\xff\xff\xff\xff\x05\x01\x00\x00\x00\x74\x01\x0f\xf4" in
  halted cpu;
  word (Cpu.register cpu Eax) 0l;
  let cpu, _, _ = machine "\x68\x78\x56\x34\x12\x58\x50\x5b\xf4" in
  halted cpu;
  word (Cpu.register cpu Ebx) 0x1234_5678l;
  word (Cpu.register cpu Esp) 4096l;
  let cpu, _, _ = machine "\xe8\x01\x00\x00\x00\xf4\xb8\x2a\x00\x00\x00\xc3" in
  halted cpu;
  word (Cpu.register cpu Eax) 42l;
  word (Cpu.register cpu Esp) 4096l;
  let cpu, _, _ = machine "\xb8\x78\x56\x34\x12\xb4\xab\xb0\xcd\xf4" in
  halted cpu;
  word (Cpu.register cpu Eax) 0x1234_abcdl;
  let cpu, _, _ =
    machine
      "\xb8\x78\x56\x34\x12\xa3\x80\x00\x00\x00\xb8\x00\x00\x00\x00\xa1\x80\x00\x00\x00\xf4"
  in
  halted cpu;
  word (Cpu.register cpu Eax) 0x1234_5678l;
  let cpu, _, _ = machine "\xeb\xfe" in
  check (Cpu.run cpu ~max_steps:100 = Budget_exhausted) "Bound infinite guest loop";
  word (Cpu.eip cpu) 0l;
  let cpu, _, _ = machine "\x0f\x0b" in
  check
    (Cpu.run cpu ~max_steps:1 = Fault (Unsupported_opcode { eip = 0l; opcode = 15 }))
    "Unknown instruction";
  word (Cpu.eip cpu) 0l;
  let cpu, _, _ = machine ~size:2 "\xb8\x00" in
  check
    (Cpu.run cpu ~max_steps:1 = Fault (Memory_fault { eip = 0l; address = 2l; width = 1 }))
    "Truncated instruction";
  word (Cpu.eip cpu) 0l;
  let cpu, memory, _ = machine "\x50" in
  Cpu.set_register cpu Esp 2l;
  check
    (match Cpu.run cpu ~max_steps:1 with
     | Fault (System_fault { message = "Guest exception #12"; _ }) -> true
     | _ -> false)
    "Stack segment boundary fault";
  word (Cpu.register cpu Esp) 2l;
  word (Cpu.eip cpu) 0l;
  check (Memory.read_u8 memory ~address:0l = 0x50) "Fault must not corrupt RAM";
  print_endline "CPU and memory tests passed"
;;

let () =
  let memory = Memory.create ~size:17 in
  for offset = 0 to 13 do
    let address = Int32.of_int offset in
    Memory.write_u32 memory ~address 0x89abcdefl;
    List.iteri
      (fun i byte ->
         check
           (Memory.read_u8 memory ~address:(Int32.of_int (offset + i)) = byte)
           "Unaligned little-endian bytes")
      [ 0xef; 0xcd; 0xab; 0x89 ];
    word (Memory.read_u32 memory ~address) 0x89abcdefl
  done;
  List.iter
    (fun address ->
       let before = Memory.read_u32 memory ~address:13l in
       (match Memory.write_u32 memory ~address 0l with
        | () -> failwith "Expected whole-width RAM fault"
        | exception Memory.Access_fault _ -> ());
       word (Memory.read_u32 memory ~address:13l) before)
    [ 14l; 16l; 17l; -1l; Int32.min_int ];
  Memory.write_u16 memory ~address:15l 0x80ff;
  check (Memory.read_u16 memory ~address:15l = 0x80ff) "Final unaligned word";
  print_endline "Native RAM widths, alignment and atomic bounds tests passed"
;;
