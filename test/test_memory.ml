open Ocaml_os

let check condition message = if not condition then failwith message

let expect_fault f =
  match f () with
  | _ -> failwith "Expected physical memory fault"
  | exception Memory.Access_fault _ -> ()

let () =
  (* Exceeds the single-bytes limit of a 32-bit OCaml runtime. *)
  let size = 32 * 1024 * 1024 in
  let memory = Memory.create ~size in
  check (Memory.size memory = size) "RAM size";
  List.iter
    (fun offset ->
      check
        (Memory.read_u8 memory ~address:(Int32.of_int offset) = 0)
        "Zero-initialized RAM")
    [ 0; 4095; size / 2; size - 1 ];
  List.iter
    (fun offset ->
      let address = Int32.of_int offset in
      Memory.write_u32 memory ~address 0xfedcba98l;
      check (Memory.read_u32 memory ~address = 0xfedcba98l) "Dword round trip";
      check (Memory.read_u16 memory ~address = 0xba98) "Little-endian low word";
      check
        (Memory.read_u8 memory ~address:(Int32.add address 3l) = 0xfe)
        "Little-endian high byte";
      Memory.write_u16 memory ~address 0x7654;
      check
        (Memory.read_u32 memory ~address = 0xfedc7654l)
        "Word store preserves adjacent bytes")
    [ 0; 1; 4094; 4095; size - 4 ];
  let last = Int32.of_int (size - 1) in
  Memory.write_u8 memory ~address:last 0x5a;
  expect_fault (fun () -> Memory.write_u32 memory ~address:last 0l);
  expect_fault (fun () -> Memory.write_u16 memory ~address:last 0);
  expect_fault (fun () -> Memory.load memory ~address:last "ab");
  check (Memory.read_u8 memory ~address:last = 0x5a) "No partial invalid store";
  expect_fault (fun () -> Memory.read_u8 memory ~address:(-1l));
  expect_fault (fun () -> Memory.read_u32 memory ~address:last);
  Memory.watch_page_table memory ~address:4096l;
  let epoch = Memory.translation_epoch memory in
  Memory.write_u32 memory ~address:4093l 0xabcdef01l;
  check
    (Memory.translation_epoch memory = epoch + 1)
    "Cross-page store invalidates translations";
  Memory.load memory ~address:4095l "\x12\x34\x56";
  check (Memory.read_u16 memory ~address:4096l = 0x5634) "Cross-page load";
  check
    (Memory.translation_epoch memory = epoch + 2)
    "Image load invalidates translations";
  Memory.write_u8 memory ~address:0l 1;
  check
    (Memory.translation_epoch memory = epoch + 2)
    "Ordinary store retains translations";
  print_endline
    "Bigarray RAM, byte order, bounds and write tracking tests passed"
