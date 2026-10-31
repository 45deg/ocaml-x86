open Ocaml_os

let make code address =
  let memory = Memory.create ~size:8192 in
  Memory.load memory ~address:0l code;
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  Cpu.set_register cpu Esi address;
  Cpu.set_register cpu Eax 0x89abcdefl;
  Cpu.set_register cpu Edx 0xfedcba98l;
  Cpu.set_register cpu Ebx 0x12345678l;
  Cpu.set_register cpu Ecx 0x87654321l;
  cpu, memory
;;

let () =
  List.iter
    (fun prefix ->
       List.iter
         (fun equal ->
            let cpu, memory = make (prefix ^ "\x0f\xc7\x0e\xf4") 0x1003l in
            Memory.write_u32 memory ~address:0x1003l (if equal then 0x89abcdefl else 7l);
            Memory.write_u32 memory ~address:0x1007l 0xfedcba98l;
            assert (Cpu.run cpu ~max_steps:2 = Halted);
            assert (Cpu.eflags cpu = if equal then 0x42l else 2l);
            assert (
              Memory.read_u32 memory ~address:0x1003l = if equal then 0x12345678l else 7l);
            assert (
              Memory.read_u32 memory ~address:0x1007l
              = if equal then 0x87654321l else 0xfedcba98l);
            assert (Cpu.register cpu Eax = if equal then 0x89abcdefl else 7l);
            assert (Cpu.register cpu Edx = 0xfedcba98l))
         [ true; false ])
    [ ""; "\xf0"; "\x66"; "\x66\xf0" ];
  let cpu, memory = make "\xf0\x0f\xc7\x0e" 0x1ffcl in
  Memory.write_u32 memory ~address:0x1ffcl 0x89abcdefl;
  (match Cpu.run cpu ~max_steps:1 with
   | Fault (Memory_fault _) -> ()
   | _ -> assert false);
  assert (Memory.read_u32 memory ~address:0x1ffcl = 0x89abcdefl);
  assert (Cpu.register cpu Eax = 0x89abcdefl);
  assert (Cpu.eflags cpu = 2l);
  List.iter
    (fun code ->
       let cpu, _ = make code 0x1000l in
       match Cpu.run cpu ~max_steps:1 with
       | Fault (System_fault { message = "Guest exception #6"; _ }) -> ()
       | _ -> assert false)
    [ "\x0f\xc7\xc8"; "\xf0\x0f\xc7\xc8"; "\x0f\xc7\x06" ];
  print_endline
    "CMPXCHG8B: success, mismatch, prefixes, unaligned access and faults passed"
;;

let () =
  List.iter
    (fun equal ->
       let memory = Memory.create ~size:65536 in
       Memory.load memory ~address:0l "\x0f\x22\xdf\x0f\x22\xc7\xf0\x0f\xc7\x0e";
       (* CR3 via EDI, then CR0 via EDI are executed in separate calls. *)
       Memory.write_u32 memory ~address:0x1000l 0x2003l;
       for page = 0 to 15 do
         Memory.write_u32
           memory
           ~address:(Int32.of_int (0x2000 + (page * 4)))
           (Int32.of_int ((page * 4096) + if page = 4 then 1 else 3))
       done;
       Memory.write_u32 memory ~address:0x3ffcl 7l;
       Memory.write_u32 memory ~address:0x4000l 8l;
       let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
       Cpu.set_register cpu Edi 0x1000l;
       ignore (Cpu.run cpu ~max_steps:1);
       Cpu.set_register cpu Edi 0x80010011l;
       Cpu.set_register cpu Esi 0x3ffcl;
       Cpu.set_register cpu Eax (if equal then 7l else 9l);
       Cpu.set_register cpu Edx 8l;
       (match Cpu.run cpu ~max_steps:2 with
        | Fault (System_fault { message = "Guest exception #14"; _ }) -> ()
        | _ -> failwith "Read-only second page must fault even on mismatch");
       assert (Cpu.control cpu 2 = 0x4000l);
       assert (Memory.read_u32 memory ~address:0x3ffcl = 7l);
       assert (Cpu.register cpu Eax = if equal then 7l else 9l);
       assert (Cpu.eflags cpu = 2l))
    [ true; false ]
;;
