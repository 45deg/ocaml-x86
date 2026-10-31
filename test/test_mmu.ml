open Ocaml_os

let check b message = if not b then failwith message
let word a b = check (a = b) (Printf.sprintf "%lx <> %lx" a b)

let expect_fault expected f =
  match f () with
  | _ -> failwith "Expected page fault"
  | exception Mmu.Page_fault { error; _ } ->
    check (error = expected) "Page-fault error code"
;;

let () =
  let memory = Memory.create ~size:(8 * 1024 * 1024) in
  let bus = Physical_bus.create memory in
  let put address value = Memory.write_u32 memory ~address value in
  put 0x1000l 0x2007l;
  put 0x2004l 0x3007l;
  let walk ?(cr0 = 0x80010011l) ?(cr4 = 0l) ~user ~access address =
    Mmu.translate bus ~cr0 ~cr3:0x1000l ~cr4 ~user ~access address
  in
  word (walk ~user:true ~access:Read 0x1234l) 0x3234l;
  word (Memory.read_u32 memory ~address:0x1000l) 0x2027l;
  word (Memory.read_u32 memory ~address:0x2004l) 0x3027l;
  ignore (walk ~user:true ~access:Write 0x1234l);
  word (Memory.read_u32 memory ~address:0x2004l) 0x3067l;
  put 0x2004l 0x3005l;
  expect_fault 7 (fun () -> walk ~user:true ~access:Write 0x1000l);
  expect_fault 3 (fun () -> walk ~user:false ~access:Write 0x1000l);
  word (walk ~cr0:0x80000011l ~user:false ~access:Write 0x1000l) 0x3000l;
  put 0x2004l 0x3003l;
  expect_fault 5 (fun () -> walk ~user:true ~access:Read 0x1000l);
  put 0x2004l 0l;
  expect_fault 6 (fun () -> walk ~user:true ~access:Write 0x1000l);
  put 0x1000l 0x400087l;
  word (walk ~cr4:0x10l ~user:true ~access:Read 0x123456l) 0x523456l;
  expect_fault 13 (fun () -> walk ~user:true ~access:Read 0x123456l);
  (* PAE PDPT -> directory -> table. *)
  put 0x1000l 0x2001l;
  put 0x1004l 0l;
  put 0x2000l 0x3007l;
  put 0x2004l 0l;
  put 0x3008l 0x4007l;
  put 0x300cl 0l;
  word (walk ~cr4:0x20l ~user:true ~access:Write 0x1234l) 0x4234l;
  word (Memory.read_u32 memory ~address:0x3008l) 0x4067l;
  put 0x300cl 0x80000000l;
  expect_fault 13 (fun () -> walk ~cr4:0x20l ~user:true ~access:Read 0x1000l);
  put 0x300cl 0l;
  put 0x2000l 0x400087l;
  word (walk ~cr4:0x20l ~user:true ~access:Read 0x123456l) 0x523456l;
  (* Cross-page stores must preflight both pages. *)
  let system = System_state.create ~memory in
  put 0x1000l 0x2003l;
  put 0x2000l 0x3003l;
  put 0x2004l 0l;
  System_state.write_control system 3 0x1000l;
  System_state.write_control system 0 0x80010011l;
  Memory.write_u8 memory ~address:0x3fffl 0x55;
  (match System_state.write_linear system ~width:Dword 0xfffl 0x12345678l with
   | () -> failwith "Expected cross-page #PF"
   | exception System_state.Guest_fault { vector; error } ->
     check (vector = 14 && error = Some 2) "Cross-page #PF");
  check (Memory.read_u8 memory ~address:0x3fffl = 0x55) "No partial cross-page store";
  word (System_state.read_control system 2) 0x1000l;
  put 0x2004l 0x5003l;
  System_state.write_linear system ~width:Dword 0xfffl 0x12345678l;
  word (System_state.read_linear system ~access:Read ~width:Dword 0xfffl) 0x12345678l;
  Physical_bus.enable_pc_mode bus;
  word (Physical_bus.read bus ~width:Dword 0xc1dfc7dcl) (-1l);
  Physical_bus.write bus ~width:Dword 0xc1dfc7dcl 123l;
  word (Physical_bus.read bus ~width:Dword 0xc1dfc7dcl) (-1l);
  print_endline "Legacy/PAE paging, permissions and physical-bus tests passed"
;;

(* Cached translations must observe all physical writes to page tables. *)
let () =
  let memory = Memory.create ~size:0x10000 in
  let system = System_state.create ~memory in
  let put address value = Memory.write_u32 memory ~address value in
  put 0x1000l 0x2007l;
  put 0x2004l 0x3007l;
  put 0x3000l 0x12345678l;
  put 0x4000l 0xabcdef01l;
  System_state.write_control system 3 0x1000l;
  System_state.write_control system 0 0x80010011l;
  let read () = System_state.read_linear system ~access:Read ~width:Dword 0x1000l in
  word (read ()) 0x12345678l;
  word (read ()) 0x12345678l;
  let epoch = Memory.translation_epoch memory in
  put 0x3004l 42l;
  check (Memory.translation_epoch memory = epoch) "Ordinary RAM retains cache";
  Memory.write_u8 memory ~address:0x2005l 0x40;
  word (read ()) 0xabcdef01l;
  (* Clearing A/D bits must cause a walk that sets them again. *)
  put 0x2004l 0x4007l;
  ignore (read ());
  word (Memory.read_u32 memory ~address:0x2004l) 0x4027l;
  System_state.write_linear system ~width:Dword 0x1000l 17l;
  word (Memory.read_u32 memory ~address:0x2004l) 0x4067l;
  (* The read cache must not bypass write/user protection. *)
  put 0x2004l 0x4001l;
  ignore (read ());
  (match System_state.write_linear system ~width:Dword 0x1000l 99l with
   | () -> failwith "Cached read bypassed write protection"
   | exception System_state.Guest_fault { vector; error } ->
     check (vector = 14 && error = Some 3) "Cached write #PF");
  (match System_state.translate system ~user:true ~access:Read 0x1000l with
   | _ -> failwith "Cached supervisor read bypassed user protection"
   | exception System_state.Guest_fault { vector; error } ->
     check (vector = 14 && error = Some 5) "Cached user #PF");
  (* Host load, directory edits and CR3 switches invalidate too. *)
  Memory.load memory ~address:0x2004l "\x07\x30\x00\x00";
  word (read ()) 0x12345678l;
  put 0x5004l 0x4007l;
  put 0x1000l 0x5007l;
  word (read ()) 17l;
  put 0x6000l 0x2007l;
  System_state.write_control system 3 0x6000l;
  word (read ()) 0x12345678l;
  (* A guest store through a virtual alias of its own page table also invalidates. *)
  put 0x2000l 0x2003l;
  System_state.write_linear system ~width:Dword 4l 0x4007l;
  word (read ()) 17l;
  (* A PAE high-half edit must not leave a cached valid mapping. *)
  put 0x7000l 0x8001l;
  put 0x8000l 0x9007l;
  put 0x9008l 0x3007l;
  System_state.write_control system 3 0x7000l;
  System_state.write_control system 4 0x20l;
  word (read ()) 0x12345678l;
  put 0x900cl 0x80000000l;
  (match read () with
   | _ -> failwith "Cached PAE high half ignored"
   | exception System_state.Guest_fault { vector; error } ->
     check (vector = 14 && error = Some 9) "PAE reserved #PF");
  print_endline "Translation cache coherence, permissions and A/D tests passed"
;;
