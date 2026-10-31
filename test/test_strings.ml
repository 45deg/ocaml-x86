open Ocaml_x86

let check b message = if not b then failwith message
let word a b = check (a = b) (Printf.sprintf "%lx <> %lx" a b)

let machine program =
  let memory = Memory.create ~size:4096 in
  Memory.load memory ~address:0l program;
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  cpu, memory
;;

let () =
  let cpu, memory = machine "\xf3\xa4\xf4" in
  Memory.load memory ~address:256l "hello";
  Cpu.set_register cpu Esi 256l;
  Cpu.set_register cpu Edi 512l;
  Cpu.set_register cpu Ecx 5l;
  check (Cpu.run cpu ~max_steps:2 = Budget_exhausted) "REP budget";
  word (Cpu.eip cpu) 0l;
  word (Cpu.register cpu Ecx) 3l;
  check (Memory.read_u8 memory ~address:513l = Char.code 'e') "Partial copy";
  check (Cpu.run cpu ~max_steps:4 = Halted) "REP resume";
  String.iteri
    (fun offset byte ->
       check
         (Memory.read_u8 memory ~address:(Int32.of_int (512 + offset)) = Char.code byte)
         "Copy")
    "hello";
  let cpu, memory = machine "\xfd\xf3\xa4\xf4" in
  Memory.load memory ~address:256l "abc";
  Cpu.set_register cpu Esi 258l;
  Cpu.set_register cpu Edi 514l;
  Cpu.set_register cpu Ecx 3l;
  check (Cpu.run cpu ~max_steps:5 = Halted) "Backward REP";
  word (Cpu.register cpu Esi) 255l;
  check (Memory.read_u8 memory ~address:512l = Char.code 'a') "Backward copy";
  let cpu, memory = machine "\xf2\xae\xf4" in
  Memory.load memory ~address:256l "abc";
  Cpu.set_register cpu Eax (Int32.of_int (Char.code 'b'));
  Cpu.set_register cpu Edi 256l;
  Cpu.set_register cpu Ecx 3l;
  check (Cpu.run cpu ~max_steps:3 = Halted) "REPNE SCAS";
  word (Cpu.register cpu Ecx) 1l;
  word (Cpu.register cpu Edi) 258l;
  let cpu, _ = machine "\xf3\xa4\xf4" in
  Cpu.set_register cpu Esi (-1l);
  check (Cpu.run cpu ~max_steps:2 = Halted) "Zero count does not access RAM";
  let cpu, memory = machine "\xf3\xa4" in
  Memory.load memory ~address:256l "ab";
  Cpu.set_register cpu Esi 256l;
  Cpu.set_register cpu Edi 4095l;
  Cpu.set_register cpu Ecx 2l;
  check
    (match Cpu.run cpu ~max_steps:3 with
     | Fault (Memory_fault _) -> true
     | _ -> false)
    "Fault on second iteration";
  word (Cpu.eip cpu) 0l;
  word (Cpu.register cpu Ecx) 1l;
  word (Cpu.register cpu Esi) 257l;
  let cpu, _ = machine "\xb4\x80\x0f\xbe\xcc\x0f\xb6\xd4\xf4" in
  check (Cpu.run cpu ~max_steps:4 = Halted) "MOVSX/MOVZX";
  word (Cpu.register cpu Ecx) (-128l);
  word (Cpu.register cpu Edx) 128l;
  let cpu, _ = machine "\xff\xd0\xf4\xb8\x2a\x00\x00\x00\xc3" in
  Cpu.set_register cpu Eax 3l;
  Cpu.set_register cpu Esp 4096l;
  check (Cpu.run cpu ~max_steps:4 = Halted) "Indirect CALL";
  word (Cpu.register cpu Eax) 42l;
  word (Cpu.register cpu Esp) 4096l;
  print_endline "String, extension and indirect branch tests passed"
;;
