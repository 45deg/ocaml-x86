open Ocaml_x86

let () =
  try
    while true do
      let hex = read_line () in
      let code =
        String.init
          (String.length hex / 2)
          (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))
      in
      let memory = Memory.create ~size:65536 in
      Memory.load memory ~address:0x1000l code;
      let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
      Cpu.set_eip cpu 0x1000l;
      match Cpu.run cpu ~max_steps:100 with
      | Halted ->
        for i = 0 to 9 do
          Printf.printf
            "%02x"
            (Memory.read_u8 memory ~address:(Int32.of_int (0x4000 + i)))
        done;
        print_newline ();
        flush stdout
      | _ ->
        print_endline "fault";
        flush stdout
    done
  with
  | End_of_file -> ()
;;
