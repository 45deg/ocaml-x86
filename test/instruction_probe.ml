open Ocaml_x86

let decode_hex text =
  String.init
    (String.length text / 2)
    (fun index -> Char.chr (int_of_string ("0x" ^ String.sub text (index * 2) 2)))
;;

let () =
  try
    while true do
      let fields = String.split_on_char ' ' (read_line ()) in
      match fields with
      | code :: flags :: registers when List.length registers = 8 ->
        let memory = Memory.create ~size:65536 in
        let setup = Bytes.of_string "\x68\x00\x00\x00\x00\x9d" in
        Bytes.set_int32_le setup 1 (Int32.of_string flags);
        Memory.load memory ~address:0x1000l (Bytes.to_string setup ^ decode_hex code);
        Memory.load
          memory
          ~address:0x4000l
          (String.init 512 (fun i -> Char.chr (((i * 37) + 11) land 255)));
        let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
        List.iteri
          (fun index value ->
             Cpu.set_register cpu (Register.of_encoding index) (Int32.of_string value))
          registers;
        Cpu.set_eip cpu 0x1000l;
        ignore (Cpu.run cpu ~max_steps:2);
        let stop = Cpu.run cpu ~max_steps:1 in
        let status =
          match stop with
          | Fault _ -> "fault"
          | _ -> "ok"
        in
        Printf.printf "%s %lx" status (Cpu.eip cpu);
        for index = 0 to 7 do
          Printf.printf " %lx" (Cpu.register cpu (Register.of_encoding index))
        done;
        Printf.printf " %lx " (Cpu.eflags cpu);
        for index = 0 to 511 do
          Printf.printf
            "%02x"
            (Memory.read_u8 memory ~address:(Int32.of_int (0x4000 + index)))
        done;
        print_newline ();
        flush stdout
      | _ -> failwith "Malformed probe input"
    done
  with
  | End_of_file -> ()
;;
