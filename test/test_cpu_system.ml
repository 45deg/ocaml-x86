open Ocaml_x86

let check b message = if not b then failwith message
let word a b = check (a = b) (Printf.sprintf "%lx <> %lx" a b)

let machine code =
  let memory = Memory.create ~size:131072 in
  Memory.load memory ~address:0l code;
  Memory.load
    memory
    ~address:0x3000l
    (String.make 16 '\000'
     ^ "\xff\xff\x00\x00\x00\x9a\xcf\x00\xff\xff\x00\x00\x00\x92\xcf\x00");
  Memory.write_u16 memory ~address:0x3500l 2047;
  Memory.write_u32 memory ~address:0x3502l 0x4000l;
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  Cpu.initialize_linux
    cpu
    { Linux_boot.entry = 0l; boot_params = 0x10000l; stack = 0xa000l; gdt = 0x3000l };
  cpu, memory
;;

let gate memory vector =
  let address = Int32.of_int (0x4000 + (vector * 8)) in
  Memory.write_u32 memory ~address 0x00102000l;
  Memory.write_u32 memory ~address:(Int32.add address 4l) 0x8e00l
;;

let () =
  let cpu, memory =
    machine
      "\x0f\x01\x1d\x00\x35\x00\x00\xb8\x00\x10\x00\x00\x0f\x22\xd8\xb8\x11\x00\x01\x80\x0f\x22\xc0\xa1\x00\x50\x00\x00\xa3\x00\x70\x00\x00\xf4"
  in
  gate memory 14;
  Memory.load
    memory
    ~address:0x2000l
    "\x0f\x20\xd2\xc7\x05\x14\x10\x01\x00\x03\x80\x00\x00\x83\xc4\x04\xcf";
  Memory.write_u32 memory ~address:0x1000l 0x11003l;
  for page = 0 to 31 do
    if page <> 5
    then
      Memory.write_u32
        memory
        ~address:(Int32.of_int (0x11000 + (page * 4)))
        (Int32.of_int ((page * 4096) + 3))
  done;
  Memory.write_u32 memory ~address:0x8000l 0xcafebabel;
  check (Cpu.run cpu ~max_steps:50 = Halted) "#PF handler and restart";
  word (Cpu.register cpu Edx) 0x5000l;
  word (Cpu.register cpu Eax) 0xcafebabel;
  word (Memory.read_u32 memory ~address:0x7000l) 0xcafebabel;
  word (Cpu.register cpu Esp) 0xa000l;
  let cpu, memory = machine "\x0f\x01\x1d\x00\x35\x00\x00\xfb\x40\xf4" in
  gate memory 32;
  Memory.load memory ~address:0x2000l "\x89\xc3\xcf";
  Cpu.request_interrupt cpu 32;
  check (Cpu.run cpu ~max_steps:20 = Halted) "External interrupt";
  word (Cpu.register cpu Ebx) 1l;
  word (Cpu.register cpu Esp) 0xa000l;
  (* FS override with a nonzero base and a bounded segment. *)
  let cpu, memory =
    machine
      "\x0f\x01\x15\x10\x35\x00\x00\xb8\x20\x00\x00\x00\x8e\xe0\x64\xa1\x04\x00\x00\x00\xf4"
  in
  Memory.write_u16 memory ~address:0x3510l 39;
  Memory.write_u32 memory ~address:0x3512l 0x3000l;
  Memory.write_u32 memory ~address:0x3020l 0x90000fffl;
  Memory.write_u32 memory ~address:0x3024l 0x00409200l;
  Memory.write_u32 memory ~address:0x9004l 42l;
  check (Cpu.run cpu ~max_steps:10 = Halted) "Segment base/override";
  word (Cpu.register cpu Eax) 42l;
  print_endline "CPU paging fault/restart, IRQ shadow and segmentation tests passed"
;;

let () =
  let cpu, _ = machine "\x0e\x58\x1e\x07\x0f\xa0\x0f\xa9\xf4" in
  check (Cpu.run cpu ~max_steps:7 = Halted) "Segment push/pop";
  word (Cpu.register cpu Eax) 0x10l;
  word (Cpu.register cpu Esp) 0xa000l;
  let cpu, _ = machine "\xba\xf8\x0c\x00\x00\xb8\x00\x10\x00\x80\xef\x31\xc0\xed\xf4" in
  Cpu.attach_devices cpu (Devices.create ~output:(fun _ -> ()));
  check (Cpu.run cpu ~max_steps:6 = Budget_exhausted) "Device HLT waits";
  word (Cpu.register cpu Eax) 0x80001000l
;;
