open Ocaml_x86

let check condition message = if not condition then failwith message

let word actual expected =
  check (Int32.equal actual expected) (Printf.sprintf "%lx <> %lx" actual expected)
;;

let machine program =
  let memory = Memory.create ~size:4096 in
  Memory.load memory ~address:0l program;
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  cpu, memory
;;

let run cpu = check (Cpu.run cpu ~max_steps:100 = Halted) "Expected halt"

let () =
  (* MOV [EBX+ECX*4-8], EAX; MOV EDX, [EBX+ECX*4-8]. *)
  let cpu, memory = machine "\x89\x44\x8b\xf8\x8b\x54\x8b\xf8\xf4" in
  Cpu.set_register cpu Eax 0xdead_beefl;
  Cpu.set_register cpu Ebx 256l;
  Cpu.set_register cpu Ecx 3l;
  run cpu;
  word (Memory.read_u32 memory ~address:260l) 0xdead_beefl;
  word (Cpu.register cpu Edx) 0xdead_beefl;
  (* Absolute disp32 without a base; immediate follows displacement. *)
  let cpu, memory =
    machine "\xc7\x05\x00\x01\x00\x00\x78\x56\x34\x12\x8b\x05\x00\x01\x00\x00\xf4"
  in
  run cpu;
  word (Cpu.register cpu Eax) 0x1234_5678l;
  word (Memory.read_u32 memory ~address:256l) 0x1234_5678l;
  (* SIB without a base, then SIB with ESP and no index. *)
  let cpu, memory = machine "\x8b\x04\xcd\x00\x01\x00\x00\x89\x04\x24\xf4" in
  Cpu.set_register cpu Ecx 2l;
  Cpu.set_register cpu Esp 512l;
  Memory.write_u32 memory ~address:272l 42l;
  run cpu;
  word (Memory.read_u32 memory ~address:512l) 42l;
  (* LEA never reads RAM, and address computation wraps. *)
  let cpu, _ = machine "\x8d\x43\x04\xf4" in
  Cpu.set_register cpu Ebx (-1l);
  run cpu;
  word (Cpu.register cpu Eax) 3l;
  (* Register mode, sign-extended immediate, subtraction, non-storing compare. *)
  let cpu, _ = machine "\x89\xd8\x83\xc0\xff\x29\xc8\x83\xf8\x04\x75\x01\xf4\x0f" in
  Cpu.set_register cpu Ebx 8l;
  Cpu.set_register cpu Ecx 3l;
  run cpu;
  word (Cpu.register cpu Eax) 4l;
  (* Read/modify/write fault leaves flags and EIP unchanged. *)
  let cpu, _ = machine "\x83\x03\x01" in
  Cpu.set_register cpu Ebx 4094l;
  check
    (match Cpu.run cpu ~max_steps:1 with
     | Fault (Memory_fault _) -> true
     | _ -> false)
    "RAM fault";
  word (Cpu.eip cpu) 0l;
  word (Cpu.eflags cpu) 2l;
  print_endline "Addressing tests passed"
;;

let () =
  let cpu, _ =
    machine "\x31\xc0\xb9\x0a\x00\x00\x00\x01\xc8\x49\x75\xfb\x83\xf8\x37\x75\x01\xf4\x0f"
  in
  run cpu;
  word (Cpu.register cpu Eax) 55l;
  word (Cpu.register cpu Ecx) 0l;
  (* SUB sets CF, INC must preserve it, TEST clears it without changing EAX. *)
  let cpu, _ = machine "\x2d\x01\x00\x00\x00\x40\x73\x05\x85\xc0\x72\x01\xf4\x0f" in
  run cpu;
  word (Cpu.register cpu Eax) 0l;
  word (Int32.logand (Cpu.eflags cpu) 0x8d5l) 0x44l
;;
