open Ocaml_os

let check b message = if not b then failwith message

let test ~width ~kind ~count ~flags value expected mask expected_flags =
  let result = Shift.execute ~width ~kind ~count ~flags value in
  check (result.value = expected) "Shift value";
  check (Int32.logand result.flags mask = expected_flags) "Shift flags"
;;

let () =
  test ~width:Byte ~kind:4 ~count:1 ~flags:2l 0x80l 0l 0x8c5l 0x845l;
  test ~width:Byte ~kind:5 ~count:1 ~flags:2l 0x81l 0x40l 0x8c5l 0x801l;
  test ~width:Byte ~kind:7 ~count:1 ~flags:2l 0x81l 0xc0l 0x8c5l 0x85l;
  test ~width:Dword ~kind:4 ~count:32 ~flags:0x8d7l 0x12345678l 0x12345678l 0xffffl 0x8d7l;
  test ~width:Byte ~kind:2 ~count:1 ~flags:3l 0x80l 1l 0x801l 0x801l;
  test ~width:Byte ~kind:3 ~count:1 ~flags:3l 0l 0x80l 0x801l 0x800l;
  test ~width:Word ~kind:0 ~count:1 ~flags:2l 0x8000l 1l 0x801l 0x801l;
  let memory = Memory.create ~size:4096 in
  Memory.load memory ~address:0l "\x68\x03\x02\x20\x00\x9d\x9c\x58\xb1\x04\xd3\xe0\xf4";
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  Cpu.set_register cpu Esp 4096l;
  check (Cpu.run cpu ~max_steps:10 = Halted) "POPF/SHL guest";
  check (Cpu.register cpu Eax = 0x02002030l) "PUSHF value and CL shift";
  print_endline "Shift and flags-stack tests passed"
;;

let () =
  let compare width kind count flags value =
    let expected = Shift_reference.execute ~width ~kind ~count ~flags value in
    let actual = Shift.execute ~width ~kind ~count ~flags value in
    if actual <> expected
    then
      failwith
        (Printf.sprintf
           "Shift mismatch width=%d kind=%d count=%d flags=%lx value=%lx"
           (Width.bits width)
           kind
           count
           flags
           value)
  in
  for value = 0 to 255 do
    for kind = 0 to 7 do
      for count = 0 to 63 do
        List.iter
          (fun flags -> compare Width.Byte kind count flags (Int32.of_int value))
          [ 2l; 0x8d7l ]
      done
    done
  done;
  let rng = Random.State.make [| 386 |] in
  List.iter
    (fun width ->
       for _ = 1 to 100000 do
         compare
           width
           (Random.State.int rng 8)
           (Random.State.int rng 256)
           (Int32.of_int (Random.State.int rng 4096))
           (Random.State.int32 rng Int32.max_int);
         compare
           width
           (Random.State.int rng 8)
           (Random.State.int rng 256)
           0x8d7l
           (Int32.lognot (Random.State.int32 rng Int32.max_int))
       done)
    [ Width.Word; Width.Dword ];
  print_endline "Shift values and all flags match the iterative reference"
;;
