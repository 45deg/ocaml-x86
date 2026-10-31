(* Loads modern (protocol >= 2.10) 32-bit bzImages into RAM.
    This prepares memory only; the caller must initialize protected-mode CPU state. *)

open Base
module W = Stdlib.Int32
module U = Stdlib.Int64

type t = { entry : int32; boot_params : int32; stack : int32; gdt : int32 }

exception Invalid_image of string

let invalid message = raise (Invalid_image message)

let load ~memory ~kernel ~initrd ~command_line =
  if String.length kernel < 0x268 then invalid "Truncated setup header";
  let u8 offset = Char.to_int kernel.[offset] in
  let u16 offset = u8 offset lor (u8 (offset + 1) lsl 8) in
  let u32 offset =
    let value = ref 0L in
    for byte = 0 to 3 do
      value :=
        U.logor !value (U.shift_left (U.of_int (u8 (offset + byte))) (8 * byte))
    done;
    U.to_int !value
  in
  if
    u16 0x1fe <> 0xaa55
    || not (String.equal (String.sub kernel ~pos:0x202 ~len:4) "HdrS")
  then invalid "Not a Linux bzImage";
  if u16 0x206 < 0x20a then
    invalid "Boot protocol older than 2.10 is unsupported";
  if u8 0x211 land 1 = 0 then invalid "Only high-loaded bzImages are supported";
  if u16 0x206 >= 0x20c && u16 0x236 land 1 <> 0 then
    invalid "64-bit kernels are unsupported";
  let setup_sectors = if u8 0x1f1 = 0 then 4 else u8 0x1f1 in
  let payload = (setup_sectors + 1) * 512 in
  let header_end = 0x202 + u8 0x201 in
  if
    header_end < 0x264 || header_end > payload
    || payload >= String.length kernel
  then invalid "Invalid setup size";
  let payload_size = String.length kernel - payload in
  if u32 0x1f4 * 16 > payload_size + 15 then
    invalid "Truncated protected-mode payload";
  let entry = 0x100000 in
  let init_size = u32 0x260 in
  if init_size < payload_size then invalid "init_size is smaller than payload";
  let alignment = u32 0x230 in
  if alignment = 0 || alignment land (alignment - 1) <> 0 then
    invalid "Invalid kernel alignment";
  let workspace_end = ((entry + alignment - 1) land -alignment) + init_size in
  let ram_size = Memory.size memory in
  if workspace_end > ram_size then invalid "Kernel workspace exceeds RAM";
  if String.contains command_line '\000' then
    invalid "NUL in kernel command line";
  if String.length command_line > Int.min (u32 0x238) 4095 then
    invalid "Kernel command line too long";
  let initrd_limit = Int.min ram_size (u32 0x22c + 1) in
  let initrd_address = (initrd_limit - String.length initrd) land -4096 in
  if initrd_address < workspace_end then
    invalid "Initramfs overlaps kernel workspace";
  let boot_params = 0x10000l in
  let command_address = 0x20000l in
  let gdt = 0x30000l in
  let at offset = W.add boot_params (W.of_int offset) in
  let write32 offset value =
    Memory.write_u32 memory ~address:(at offset) (W.of_int value)
  in
  Memory.load memory ~address:boot_params (String.make 4096 '\000');
  Memory.load memory ~address:(at 0x1f1)
    (String.sub kernel ~pos:0x1f1 ~len:(header_end - 0x1f1));
  Memory.load memory ~address:(W.of_int entry)
    (String.sub kernel ~pos:payload ~len:payload_size);
  Memory.load memory ~address:command_address (command_line ^ "\000");
  Memory.load memory ~address:(W.of_int initrd_address) initrd;
  Memory.write_u8 memory ~address:(at 0x210) 0xff;
  Memory.write_u8 memory ~address:(at 0x211) (u8 0x211 land 0x9f);
  write32 0x214 entry;
  write32 0x218 (if String.is_empty initrd then 0 else initrd_address);
  write32 0x21c (String.length initrd);
  Memory.write_u32 memory ~address:(at 0x228) command_address;
  (* E820 describes low RAM, the legacy reserved hole, and extended RAM. *)
  Memory.write_u8 memory ~address:(at 0x1e8) 3;
  List.iteri
    [
      (0, 0xa0000, 1); (0xa0000, 0x60000, 2); (0x100000, ram_size - 0x100000, 1);
    ]
    ~f:(fun index (base, length, kind) ->
      let offset = 0x2d0 + (index * 20) in
      write32 offset base;
      write32 (offset + 8) length;
      write32 (offset + 16) kind);
  Memory.load memory ~address:gdt
    (String.make 16 '\000'
   ^ "\xff\xff\x00\x00\x00\x9b\xcf\x00\xff\xff\x00\x00\x00\x93\xcf\x00");
  { entry = W.of_int entry; boot_params; stack = 0x40000l; gdt }
