open Ocaml_x86

let check b message = if not b then failwith message

let rejected f =
  match f () with
  | () -> failwith "Expected system fault"
  | exception System_state.Unsupported _ -> ()
  | exception System_state.Guest_fault _ -> ()
;;

let () =
  let memory = Memory.create ~size:4096 in
  let system = System_state.create ~memory in
  Memory.load
    memory
    ~address:256l
    "\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\x00\x00\x00\x9a\xcf\x00\xff\xff\x00\x00\x00\x92\xcf\x00";
  System_state.set_gdt system ~base:256l ~limit:23;
  System_state.load_segment system ~memory ~index:1 ~selector:8;
  System_state.load_segment system ~memory ~index:2 ~selector:16;
  check (System_state.selector system 2 = 16) "SS selector";
  check (Memory.read_u8 memory ~address:277l = 0x93) "Descriptor accessed bit";
  rejected (fun () -> System_state.load_segment system ~memory ~index:2 ~selector:8);
  rejected (fun () -> System_state.load_segment system ~memory ~index:1 ~selector:0);
  rejected (fun () -> System_state.load_segment system ~memory ~index:3 ~selector:24);
  System_state.write_control system 0 0x80000011l;
  check (System_state.read_control system 0 = 0x80000011l) "Paging enabled";
  System_state.write_control system 0 0x11l;
  System_state.write_control system 3 0x1000l;
  check (System_state.read_control system 3 = 0x1000l) "CR3";
  System_state.set_idt system ~base:512l ~limit:255;
  check (System_state.idt system = (512l, 255)) "IDTR";
  Memory.load
    memory
    ~address:0l
    "\x0f\x01\x15\x80\x00\x00\x00\xb8\x10\x00\x00\x00\x8e\xd8\x8c\xdb\x0f\x20\xc1\xfa\xfb\xf4";
  Memory.write_u16 memory ~address:128l 23;
  Memory.write_u32 memory ~address:130l 256l;
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  check (Cpu.run cpu ~max_steps:10 = Halted) "System instructions";
  check (Cpu.register cpu Ebx = 16l) "MOV segment";
  check (Cpu.register cpu Ecx = 0x11l) "MOV CR0";
  check (Int32.logand (Cpu.eflags cpu) 0x200l <> 0l) "STI";
  System_state.write_debug system 7 0l;
  check (System_state.read_debug system 7 = 0x400l) "DR7 reserved bit";
  System_state.write_debug system 6 0l;
  check (System_state.read_debug system 6 = 0xffff0ff0l) "DR6 reserved bits";
  System_state.write_debug system 4 7l;
  check (System_state.read_debug system 6 = 0xffff0ff7l) "DR4 alias";
  System_state.write_control system 4 8l;
  rejected (fun () -> ignore (System_state.read_debug system 4));
  check (System_state.verify_segment system ~write:false 8) "VERR readable code";
  check (not (System_state.verify_segment system ~write:true 8)) "VERW rejects code";
  check (System_state.verify_segment system ~write:true 16) "VERW writable data";
  check (not (System_state.verify_segment system ~write:true 0)) "VERW null";
  check (not (System_state.verify_segment system ~write:false 24)) "VERR bounds";
  check (not (System_state.verify_segment system ~write:false 19)) "VERR RPL";
  check
    (System_state.query_segment system ~limit:true 16 = Some Int32.minus_one)
    "LSL granularity";
  check (System_state.query_segment system ~limit:false 16 = Some 0xc09300l) "LAR access";
  print_endline "Bootstrap system-state tests passed"
;;
