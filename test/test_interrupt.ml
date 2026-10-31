open Ocaml_x86

let check b message = if not b then failwith message
let word a b = check (a = b) (Printf.sprintf "%lx <> %lx" a b)

let descriptor memory address ~base ~limit ~access ~flags =
  let open Int32 in
  let low = logor (of_int (limit land 65535)) (shift_left (logand base 65535l) 16) in
  let high =
    logor
      (logand (shift_right_logical base 16) 255l)
      (logor
         (shift_left (of_int access) 8)
         (logor (of_int (limit land 0xf0000 lor flags)) (logand base 0xff000000l)))
  in
  Memory.write_u32 memory ~address low;
  Memory.write_u32 memory ~address:(add address 4l) high
;;

let () =
  let memory = Memory.create ~size:65536 in
  let system = System_state.create ~memory in
  List.iter
    (fun (offset, access) ->
       descriptor
         memory
         (Int32.of_int (0x1000 + offset))
         ~base:0l
         ~limit:0xfffff
         ~access
         ~flags:0xc00000)
    [ 8, 0x9a; 16, 0x92; 24, 0xfa; 32, 0xf2 ];
  descriptor memory 0x1028l ~base:0x2000l ~limit:103 ~access:0x89 ~flags:0;
  System_state.set_gdt system ~base:0x1000l ~limit:47;
  System_state.load_segment system ~memory ~index:1 ~selector:8;
  System_state.load_segment system ~memory ~index:2 ~selector:16;
  System_state.load_task system 40;
  check (Memory.read_u8 memory ~address:0x102dl = 0x8b) "TSS busy bit";
  Memory.write_u32 memory ~address:0x2004l 0x9000l;
  Memory.write_u16 memory ~address:0x2008l 16;
  System_state.set_idt system ~base:0x3000l ~limit:2047;
  Memory.write_u32 memory ~address:0x3400l 0x00085000l;
  Memory.write_u32 memory ~address:0x3404l 0x0000ee00l;
  (* IRET from ring 0 to ring 3. *)
  List.iteri
    (fun index value ->
       Memory.write_u32 memory ~address:(Int32.of_int (0x8000 + (index * 4))) value)
    [ 0x4000l; 0x1bl; 0x202l; 0x7000l; 0x23l ];
  let eip, esp, flags = System_state.iret system ~esp:0x8000l ~eflags:2l in
  word eip 0x4000l;
  word esp 0x7000l;
  word flags 0x202l;
  check (System_state.cpl system = 3) "User CPL";
  (match System_state.write_control system 3 0l with
   | () -> failwith "Ring 3 MOV CR must fault"
   | exception System_state.Guest_fault { vector; _ } ->
     check (vector = 13) "Privileged #GP");
  (* Software INT 0x80 switches to the TSS ring-0 stack. *)
  let eip, esp, flags =
    System_state.interrupt
      system
      ~vector:128
      ~software:true
      ~error:None
      ~eip:0x4002l
      ~esp
      ~eflags:flags
  in
  word eip 0x5000l;
  word esp 0x8fecl;
  word flags 2l;
  check (System_state.cpl system = 0) "Kernel CPL";
  List.iteri
    (fun index value ->
       word
         (Memory.read_u32 memory ~address:(Int32.add esp (Int32.of_int (index * 4))))
         value)
    [ 0x4002l; 0x1bl; 0x202l; 0x7000l; 0x23l ];
  let eip, esp, flags = System_state.iret system ~esp ~eflags:flags in
  word eip 0x4002l;
  word esp 0x7000l;
  word flags 0x202l;
  check (System_state.cpl system = 3) "Return to user CPL";
  (* A DPL-0 gate rejects user INT without changing CPL/stack. *)
  Memory.write_u32 memory ~address:0x3404l 0x00008e00l;
  (match
     System_state.interrupt
       system
       ~vector:128
       ~software:true
       ~error:None
       ~eip
       ~esp
       ~eflags:flags
   with
   | _ -> failwith "Expected gate DPL fault"
   | exception System_state.Guest_fault { vector; error } ->
     check (vector = 13 && error = Some 1026) "Gate #GP code");
  check (System_state.cpl system = 3) "Fault preserves privilege";
  print_endline "IDT, TSS, privilege-transition and IRET tests passed"
;;
