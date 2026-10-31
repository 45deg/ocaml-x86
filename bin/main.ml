open Ocaml_os

let demo = "\xba\xf8\x03\x00\x00\xb0O\xee\xb0K\xee\xb0\x0a\xee\xf4"

let () =
  let input = ref None in
  let budget = ref 1_000_000 in
  let memory_mib = ref None in
  let progress_every = ref 0 in
  let dump_state = ref false in
  let kernel = ref None in
  let kernel_elf = ref None in
  let initrd = ref None in
  let interactive = ref false in
  let serial_input = ref None in
  let input_after = ref "~ # " in
  let command_line =
    ref "console=ttyS0 earlyprintk=serial,ttyS0 nokaslr rdinit=/bin/sh"
  in
  Arg.parse
    [ "--dump-state", Arg.Set dump_state, " Print registers and instruction bytes on stop"
    ; "--steps", Arg.Set_int budget, "N Maximum instruction count"
    ; ( "--progress-every"
      , Arg.Set_int progress_every
      , "N Report EIP/CR3 every N budget units (0 disables)" )
    ; ( "--memory-mib"
      , Arg.Int (fun size -> memory_mib := Some size)
      , "N RAM size in MiB (default: kernel 128, raw 16)" )
    ; ( "--kernel"
      , Arg.String (fun path -> kernel := Some path)
      , "PATH Linux bzImage (experimental)" )
    ; ( "--kernel-elf"
      , Arg.String (fun path -> kernel_elf := Some path)
      , "PATH Extracted ELF for development boots" )
    ; ( "--initrd"
      , Arg.String (fun path -> initrd := Some path)
      , "PATH Initial RAM filesystem" )
    ; "--interactive", Arg.Set interactive, " Forward standard input to COM1"
    ; ( "--serial-input"
      , Arg.String (fun path -> serial_input := Some path)
      , "PATH Send file once after serial prompt" )
    ; ( "--input-after"
      , Arg.Set_string input_after
      , "TEXT Prompt for --serial-input (default: ~ # )" )
    ; "--append", Arg.Set_string command_line, "TEXT Kernel command line"
    ]
    (fun path ->
       match !input with
       | None -> input := Some path
       | Some _ -> raise (Arg.Bad "Only one raw binary is accepted"))
    "ocaml-os [--steps N] [raw-32-bit.bin]";
  if !progress_every < 0
  then (
    prerr_endline "--progress-every must be nonnegative";
    exit 2);
  if !budget < 0
  then (
    prerr_endline "--steps must be nonnegative";
    exit 2);
  try
    let program =
      match !input with
      | None -> demo
      | Some path -> In_channel.with_open_bin path In_channel.input_all
    in
    if Option.is_some !kernel && Option.is_some !input
    then failwith "Choose --kernel or a raw binary";
    if Option.is_some !kernel_elf && Option.is_none !kernel
    then failwith "--kernel-elf requires --kernel for boot metadata";
    if Option.is_some !initrd && Option.is_none !kernel
    then failwith "--initrd requires --kernel";
    let ram_mib =
      Option.value !memory_mib ~default:(if Option.is_some !kernel then 128 else 16)
    in
    if ram_mib < 1 || ram_mib > 4095
    then failwith "--memory-mib must be between 1 and 4095";
    let memory = Memory.create ~size:(ram_mib * 1024 * 1024) in
    let output_u8 ~port value =
      if port = 0x3f8
      then (
        output_char stdout (Char.chr value);
        flush stdout)
      else failwith (Printf.sprintf "Unsupported output port 0x%04x" port)
    in
    if (!interactive || Option.is_some !serial_input) && Option.is_none !kernel
    then failwith "Serial input requires --kernel";
    if !input_after = "" then failwith "--input-after must not be empty";
    let pending_input =
      ref
        (Option.map
           (fun path -> In_channel.with_open_bin path In_channel.input_all)
           !serial_input)
    in
    let devices = ref None in
    let output_tail = ref "" in
    let serial_output byte =
      output_char stdout byte;
      flush stdout;
      match !pending_input with
      | None -> ()
      | Some text ->
        output_tail := !output_tail ^ String.make 1 byte;
        let extra = String.length !output_tail - String.length !input_after in
        if extra > 0
        then output_tail := String.sub !output_tail extra (String.length !input_after);
        if !output_tail = !input_after
        then (
          pending_input := None;
          Option.iter (fun device -> Devices.input device text) !devices)
    in
    let cpu = Cpu.create ~memory ~output_u8 in
    (match !kernel with
     | None ->
       Memory.load memory ~address:0x1000l program;
       Cpu.set_eip cpu 0x1000l;
       Cpu.set_register cpu Esp (Int32.of_int (Memory.size memory))
     | Some path ->
       let read path = In_channel.with_open_bin path In_channel.input_all in
       let image =
         Linux_boot.load
           ~memory
           ~kernel:(read path)
           ~initrd:
             (match !initrd with
              | None -> ""
              | Some path -> read path)
           ~command_line:!command_line
       in
       let image =
         match !kernel_elf with
         | None -> image
         | Some path ->
           let entry =
             Elf_loader.load
               ~memory
               ~image:(read path)
               ~initrd_start:
                 (Memory.read_u32 memory ~address:(Int32.add image.boot_params 0x218l))
           in
           { image with entry }
       in
       Cpu.initialize_linux cpu image;
       let device = Devices.create ~output:serial_output in
       devices := Some device;
       Cpu.attach_devices cpu device);
    if !interactive && Unix.isatty Unix.stdin
    then (
      let original = Unix.tcgetattr Unix.stdin in
      Unix.tcsetattr
        Unix.stdin
        Unix.TCSANOW
        { original with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 };
      at_exit (fun () -> Unix.tcsetattr Unix.stdin Unix.TCSANOW original));
    let input_open = ref !interactive in
    let since_progress = ref 0 in
    let rec run remaining =
      if !input_open
      then (
        let ready, _, _ = Unix.select [ Unix.stdin ] [] [] 0. in
        if ready <> []
        then (
          let bytes = Bytes.create 4096 in
          let count = Unix.read Unix.stdin bytes 0 4096 in
          if count = 0
          then input_open := false
          else
            Option.iter
              (fun device -> Devices.input device (Bytes.sub_string bytes 0 count))
              !devices));
      let steps = min remaining 10000 in
      match Cpu.run cpu ~max_steps:steps with
      | Cpu.Budget_exhausted when remaining > steps ->
        since_progress := !since_progress + steps;
        if !progress_every > 0 && !since_progress >= !progress_every
        then (
          Printf.eprintf
            "Progress budget=%d eip=%08lx cr3=%08lx\n%!"
            (!budget - remaining + steps)
            (Cpu.eip cpu)
            (Cpu.control cpu 3);
          since_progress := 0);
        run (remaining - steps)
      | stop -> stop
    in
    let stop = run !budget in
    if !dump_state
    then (
      List.iter
        (fun r -> Printf.eprintf "r%d=%08lx " (Register.encoding r) (Cpu.register cpu r))
        [ Eax; Ecx; Edx; Ebx; Esp; Ebp; Esi; Edi ];
      Printf.eprintf "eflags=%08lx\n" (Cpu.eflags cpu);
      List.iter
        (fun index -> Printf.eprintf "cr%d=%08lx " index (Cpu.control cpu index))
        [ 0; 2; 3; 4 ];
      Printf.eprintf "\n";
      for offset = 0 to 31 do
        try Printf.eprintf "%02x" (Cpu.read_code_byte cpu ~offset) with
        | Memory.Access_fault _ | System_state.Guest_fault _ -> ()
      done;
      Printf.eprintf "\n");
    match stop with
    | Halted -> ()
    | Budget_exhausted ->
      Printf.eprintf "Instruction budget exhausted at 0x%08lx\n" (Cpu.eip cpu);
      exit 1
    | Fault (System_fault { eip; message }) ->
      Printf.eprintf "System fault at 0x%08lx: %s\n" eip message;
      exit 1
    | Fault (Unsupported_opcode { eip; opcode }) ->
      Printf.eprintf "Unsupported opcode 0x%02x at 0x%08lx\n" opcode eip;
      exit 1
    | Fault (Memory_fault { eip; address; width }) ->
      Printf.eprintf
        "RAM fault at EIP=0x%08lx: address=0x%08lx width=%d\n"
        eip
        address
        width;
      exit 1
  with
  | Sys_error message | Failure message | Linux_boot.Invalid_image message ->
    prerr_endline message;
    exit 1
  | Memory.Access_fault _ ->
    prerr_endline "Program does not fit in RAM";
    exit 1
;;
