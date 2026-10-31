open Ocaml_os

let () =
  let kernel = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  let steps = int_of_string Sys.argv.(2) in
  let destination = Sys.argv.(3) in
  let memory = Memory.create ~size:(128 * 1024 * 1024) in
  let image =
    Linux_boot.load
      ~memory
      ~kernel
      ~initrd:""
      ~command_line:"console=ttyS0 earlyprintk=serial,ttyS0 nokaslr rdinit=/bin/sh"
  in
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  Cpu.initialize_linux cpu image;
  let stop = Cpu.run cpu ~max_steps:steps in
  Out_channel.with_open_bin (destination ^ ".ram") (fun output ->
    for page = 0 to (Memory.size memory / 65536) - 1 do
      output_string
        output
        (String.init 65536 (fun offset ->
           Char.chr
             (Memory.read_u8 memory ~address:(Int32.of_int ((page * 65536) + offset)))))
    done);
  Out_channel.with_open_text (destination ^ ".regs") (fun output ->
    Printf.fprintf output "%lx" (Cpu.eip cpu);
    for index = 0 to 7 do
      Printf.fprintf output " %lx" (Cpu.register cpu (Register.of_encoding index))
    done;
    Printf.fprintf output " %lx\n" (Cpu.eflags cpu));
  Printf.printf
    "Snapshot after budget %d at %08lx: %s\n"
    steps
    (Cpu.eip cpu)
    (match stop with
     | Budget_exhausted -> "budget"
     | Halted -> "halted"
     | Fault _ -> "fault")
;;
