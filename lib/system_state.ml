(* Protected-mode state, segment caches, linear memory and interrupt frames. *)

open Base
module W = Stdlib.Int32
module U = Stdlib.Int64

(* A cached segment descriptor and its selector. *)
type segment = {
  selector : int;
  base : int32;
  limit : int64;
  access : int;
  wide : bool;
  valid : bool;
}

(* A cached linear-to-physical page translation and its permissions. *)
type translation = {
  page : int32;
  physical_page : int32;
  user : bool;
  kind : Mmu.access;
  epoch : int;
}

(* Protected-mode control state, descriptor tables, segments, and memory bus. *)
type t = {
  bus : Physical_bus.t;
  mutable gdt_base : int32;
  mutable gdt_limit : int;
  mutable idt_base : int32;
  mutable idt_limit : int;
  mutable ldt_base : int32;
  mutable ldt_limit : int;
  mutable ldtr : int;
  mutable task_base : int32;
  mutable task_limit : int;
  mutable task_selector : int;
  segments : segment array;
  sysenter : int64 array;
  debug : int32 array;
  control : int32 array;
  translations : translation option array;
}

exception Unsupported of string
exception Guest_fault of { vector : int; error : int option }

(* Raise an error for an unsupported system operation. *)
let unsupported message = raise (Unsupported message)

(* Raise a guest exception with its vector and error code. *)
let fault vector error =
  let error =
    if List.mem [ 8; 10; 11; 12; 13; 14; 17; 21 ] vector ~equal:Int.equal then
      Some error
    else None
  in
  raise (Guest_fault { vector; error })

(* Test whether a bit mask is set in a 32-bit value. *)
let bit value mask = not (W.equal (W.logand value mask) 0l)
(* Interpret an int32 as an unsigned 32-bit integer. *)
let unsigned value = U.logand (U.of_int32 value) 0xffffffffL

(* Create an empty segment cache for the given selector. *)
let empty selector =
  { selector; base = 0l; limit = 0L; access = 0; wide = true; valid = false }

(* Create initial protected-mode system state backed by memory. *)
let create ~memory =
  let flat =
    {
      selector = 0;
      base = 0l;
      limit = 0xffffffffL;
      access = 0x93;
      wide = true;
      valid = true;
    }
  in
  let segments = Array.create ~len:6 flat in
  segments.(1) <- { flat with access = 0x9b };
  {
    bus = Physical_bus.create memory;
    gdt_base = 0l;
    gdt_limit = 0;
    idt_base = 0l;
    idt_limit = 0;
    ldt_base = 0l;
    ldt_limit = 0;
    ldtr = 0;
    task_base = 0l;
    task_limit = 0;
    task_selector = 0;
    segments;
    sysenter = Array.create ~len:3 0L;
    debug = [| 0l; 0l; 0l; 0l; 0l; 0l; 0xffff0ff0l; 0x400l |];
    control = [| 0x11l; 0l; 0l; 0l; 0l |];
    translations = Array.create ~len:1024 None;
  }

(* Enable PC-style handling of unclaimed physical addresses. *)
let enable_pc_mode t = Physical_bus.enable_pc_mode t.bus
(* Return the current privilege level from the code selector. *)
let cpl t = t.segments.(1).selector land 3
(* Return the default operand width of the current code segment. *)
let code_width t = if t.segments.(1).wide then Width.Dword else Width.Word
(* Return the address width of the current stack segment. *)
let stack_width t = if t.segments.(2).wide then Width.Dword else Width.Word
(* Return the selector loaded in a segment register. *)
let selector t index = t.segments.(index).selector

(* Set the global descriptor table base and limit. *)
let set_gdt t ~base ~limit =
  t.gdt_base <- base;
  t.gdt_limit <- limit

(* Set the interrupt descriptor table base and limit. *)
let set_idt t ~base ~limit =
  t.idt_base <- base;
  t.idt_limit <- limit

(* Return the global descriptor table base and limit. *)
let gdt t = (t.gdt_base, t.gdt_limit)
(* Return the interrupt descriptor table base and limit. *)
let idt t = (t.idt_base, t.idt_limit)
(* Raise a protection fault unless the current privilege level is zero. *)
let require_ring0 t = if cpl t <> 0 then fault 13 0

(* Read a supported control register. *)
let read_control t index =
  if index <> 0 && index <> 2 && index <> 3 && index <> 4 then fault 6 0;
  t.control.(index)

(* Discard cached linear-to-physical translations. *)
let invalidate_translations t = Array.fill t.translations ~pos:0 ~len:1024 None

(* Write a control register and invalidate translations when needed. *)
let write_control t index value =
  require_ring0 t;
  ignore (read_control t index : int32);
  if index = 0 && not (bit value 1l) then
    unsupported "Real mode is not implemented";
  if index = 4 && bit value (W.lognot 0x6bfl) then fault 13 0;
  t.control.(index) <- value;
  if index <> 2 then invalidate_translations t

(* Translate a linear address, using and updating the translation cache. *)
let translate t ~user ~access address =
  (* Convert an MMU page fault into a guest exception after recording CR2. *)
  let walk () =
    try
      Mmu.translate t.bus ~cr0:t.control.(0) ~cr3:t.control.(3)
        ~cr4:t.control.(4) ~user ~access address
    with Mmu.Page_fault { address; error } ->
      t.control.(2) <- address;
      fault 14 error
  in
  if not (bit t.control.(0) Int32.min_value) then address
  else
    let page = W.logand address 0xfffff000l in
    let offset = W.logand address 0xfffl in
    let slot = W.to_int (W.shift_right_logical page 12) land 1023 in
    let epoch = Physical_bus.translation_epoch t.bus in
    (* Permissions depend on privilege and access type; watched page-table
       writes invalidate cached entries by advancing the epoch. *)
    let physical_page =
      match t.translations.(slot) with
      | Some entry
        when W.equal entry.page page && Bool.equal entry.user user
             && Poly.equal entry.kind access
             && entry.epoch = epoch ->
          entry.physical_page
      | _ ->
          let physical_page = W.logand (walk ()) 0xfffff000l in
          t.translations.(slot) <-
            Some
              {
                page;
                physical_page;
                user;
                kind = access;
                epoch = Physical_bus.translation_epoch t.bus;
              };
          physical_page
    in
    W.logor physical_page offset

(* Read linear memory with paging and privilege checks. *)
let read_linear ?(supervisor = false) t ~access ~width address =
  let user = (not supervisor) && cpl t = 3 in
  let first = translate t ~user ~access address in
  if W.to_int (W.logand address 0xfffl) + Width.bytes width <= 4096 then
    Physical_bus.read t.bus ~width first
  else
    let value = ref 0l in
    for offset = 0 to Width.bytes width - 1 do
      let physical =
        translate t ~user ~access (W.add address (W.of_int offset))
      in
      value :=
        W.logor !value
          (W.shift_left
             (Physical_bus.read t.bus ~width:Byte physical)
             (8 * offset))
    done;
    !value

(* Validate a linear memory range before an access. *)
let check_linear ?(supervisor = false) t ~access ~address ~width =
  let user = (not supervisor) && cpl t = 3 in
  (* Check each page separately because adjacent linear pages need not map to
     adjacent physical addresses or have the same permissions. *)
  let rec loop address remaining =
    if remaining > 0 then (
      let amount =
        Int.min remaining (4096 - W.to_int (W.logand address 0xfffl))
      in
      let physical = translate t ~user ~access address in
      Physical_bus.check t.bus ~address:physical ~width:amount;
      loop (W.add address (W.of_int amount)) (remaining - amount))
  in
  loop address width

(* Write linear memory after checking the full access range. *)
let write_linear ?(supervisor = false) t ~width address value =
  let user = (not supervisor) && cpl t = 3 in
  let physical = translate t ~user ~access:Write address in
  if W.to_int (W.logand address 0xfffl) + Width.bytes width <= 4096 then
    Physical_bus.write t.bus ~width physical value
  else
    (* Translate and check every byte before writing any, so a fault on the
       second page cannot leave a partially written operand. *)
    let addresses =
      Array.init (Width.bytes width) ~f:(fun offset ->
          let address =
            translate t ~user ~access:Write (W.add address (W.of_int offset))
          in
          Physical_bus.check t.bus ~address ~width:1;
          address)
    in
    Array.iteri addresses ~f:(fun offset address ->
        Physical_bus.write t.bus ~width:Byte address
          (W.shift_right_logical value (offset * 8)))

(* Read and decode a segment descriptor selected from the GDT or LDT. *)
let descriptor t selector =
  let offset = selector land 0xfff8 in
  let base, limit =
    if selector land 4 = 0 then (t.gdt_base, t.gdt_limit)
    else (t.ldt_base, t.ldt_limit)
  in
  if offset + 7 > limit || (selector land 4 <> 0 && t.ldtr = 0) then
    fault 13 (selector land 0xfffc);
  let address = W.add base (W.of_int offset) in
  let low = read_linear ~supervisor:true t ~access:Read ~width:Dword address in
  let high =
    read_linear ~supervisor:true t ~access:Read ~width:Dword (W.add address 4l)
  in
  let base =
    W.logor
      (W.shift_right_logical low 16)
      (W.logor
         (W.shift_left (W.logand high 255l) 16)
         (W.logand high 0xff000000l))
  in
  let limit =
    unsigned (W.logor (W.logand low 0xffffl) (W.logand high 0xf0000l))
  in
  let limit =
    if bit high 0x800000l then U.logor (U.shift_left limit 12) 0xfffL else limit
  in
  let access = W.to_int (W.logand (W.shift_right_logical high 8) 255l) in
  ( { selector; base; limit; access; wide = bit high 0x400000l; valid = true },
    address,
    high )

(* Test whether a segment descriptor is present. *)
let present segment = segment.access land 0x80 <> 0
(* Test whether a segment descriptor describes code. *)
let code segment = segment.access land 8 <> 0
(* Return the descriptor privilege level of a segment. *)
let dpl segment = (segment.access lsr 5) land 3

(* Set the accessed bit in a descriptor in guest memory. *)
let mark_accessed t address high =
  if not (bit high 0x100l) then
    write_linear ~supervisor:true t ~width:Dword (W.add address 4l)
      (W.logor high 0x100l)

(* Check whether a data segment can be loaded at the given privilege level. *)
let validate_data ~level ~index segment =
  let selector = segment.selector in
  if segment.access land 0x10 = 0 then fault 13 (selector land 0xfffc);
  if index = 2 then (
    if
      code segment
      || segment.access land 2 = 0
      || dpl segment <> level
      || selector land 3 <> level
    then fault 13 (selector land 0xfffc))
  else (
    if code segment && segment.access land 2 = 0 then
      fault 13 (selector land 0xfffc);
    if
      ((not (code segment)) || segment.access land 4 = 0)
      && Int.max level (selector land 3) > dpl segment
    then fault 13 (selector land 0xfffc));
  if not (present segment) then
    fault (if index = 2 then 12 else 11) (selector land 0xfffc)

(* Validate and return a code segment descriptor for a transfer. *)
let code_descriptor t ~selector ~level ~gate =
  if selector land 0xfffc = 0 then fault 13 0;
  let segment, address, high = descriptor t selector in
  if (not (code segment)) || segment.access land 0x10 = 0 then
    fault 13 (selector land 0xfffc);
  let conforming = segment.access land 4 <> 0 in
  if gate then (if dpl segment > level then fault 13 (selector land 0xfffc))
  else if
    (conforming && dpl segment > level)
    || ((not conforming) && dpl segment <> level)
    || selector land 3 > level
  then fault 13 (selector land 0xfffc);
  if not (present segment) then fault 11 (selector land 0xfffc);
  mark_accessed t address high;
  segment

(* Validate and load a segment selector into its cache. *)
let load_segment t ~memory:_ ~index ~selector =
  if index < 0 || index >= 6 then fault 6 0;
  if selector land 0xfffc = 0 then (
    if index = 1 || index = 2 then fault 13 0;
    t.segments.(index) <- empty selector)
  else if index = 1 then
    let segment = code_descriptor t ~selector ~level:(cpl t) ~gate:false in
    t.segments.(1) <- { segment with selector = selector land 0xfffc lor cpl t }
  else
    let segment, address, high = descriptor t selector in
    validate_data ~level:(cpl t) ~index segment;
    mark_accessed t address high;
    t.segments.(index) <- segment

(* Check a segmented access and calculate its linear address. *)
let segment_address segment ~index ~access ~offset ~width =
  let error () = fault (if index = 2 then 12 else 13) 0 in
  if not segment.valid then error ();
  if Poly.equal access Mmu.Execute && not (code segment) then error ();
  if Poly.equal access Mmu.Read && code segment && segment.access land 2 = 0
  then error ();
  if Poly.equal access Mmu.Write && (code segment || segment.access land 2 = 0)
  then error ();
  let last = U.add (unsigned offset) (U.of_int (width - 1)) in
  if (not (code segment)) && segment.access land 4 <> 0 then (
    let maximum = if segment.wide then 0xffffffffL else 0xffffL in
    if
      U.compare (unsigned offset) segment.limit <= 0
      || U.compare last maximum > 0
    then error ())
  else if U.compare last segment.limit > 0 then error ();
  W.add segment.base offset

(* Translate a segment offset to a checked linear address. *)
let linear_address t ~segment ~access ~offset ~width =
  segment_address t.segments.(segment) ~index:segment ~access ~offset ~width

(* Read guest memory through a segment and the paging system. *)
let read t ~segment ~access ~width offset =
  read_linear t ~access ~width
    (linear_address t ~segment ~access ~offset ~width:(Width.bytes width))

(* Write guest memory through a segment and the paging system. *)
let write t ~segment ~width offset value =
  write_linear t ~width
    (linear_address t ~segment ~access:Write ~offset ~width:(Width.bytes width))
    value

(* Validate a segmented memory range without accessing its contents. *)
let check t ~segment ~access ~address ~width =
  check_linear t ~access
    ~address:(linear_address t ~segment ~access ~offset:address ~width)
    ~width

(* Validate and install the code segment for a far jump. *)
let far_jump t ~selector ~offset =
  let segment = code_descriptor t ~selector ~level:(cpl t) ~gate:false in
  if U.compare (unsigned offset) segment.limit > 0 then fault 13 0;
  t.segments.(1) <- { segment with selector = selector land 0xfffc lor cpl t }

(* Load the local descriptor table selector and cache. *)
let load_ldt t selector =
  require_ring0 t;
  if selector land 0xfffc = 0 then (
    t.ldtr <- 0;
    t.ldt_limit <- 0)
  else (
    if selector land 4 <> 0 then fault 13 (selector land 0xfffc);
    let segment, _, _ = descriptor t selector in
    if segment.access land 0x1f <> 2 then fault 13 (selector land 0xfffc);
    if not (present segment) then fault 11 (selector land 0xfffc);
    t.ldtr <- selector;
    t.ldt_base <- segment.base;
    t.ldt_limit <- U.to_int segment.limit)

(* Load and validate the task register and its descriptor. *)
let load_task t selector =
  require_ring0 t;
  if selector land 0xfffc = 0 || selector land 4 <> 0 then
    fault 13 (selector land 0xfffc);
  let segment, address, high = descriptor t selector in
  if segment.access land 0x1f <> 9 then fault 13 (selector land 0xfffc);
  if not (present segment) then fault 11 (selector land 0xfffc);
  write_linear ~supervisor:true t ~width:Dword (W.add address 4l)
    (W.logor high 0x200l);
  t.task_selector <- selector;
  t.task_base <- segment.base;
  t.task_limit <- U.to_int segment.limit

(* Return the current task register selector. *)
let task_selector t = t.task_selector
(* Return the current local descriptor table selector. *)
let ldt_selector t = t.ldtr

(* Interrupt frames are checked before their writes; descriptor A bits and page
   A/D bits may change while validating, as on hardware. *)
(* Enter an interrupt gate and construct the guest stack frame. *)
let interrupt t ~vector ~software ~error ~eip ~esp ~eflags =
  let gate_error = (vector * 8) + 2 in
  if vector < 0 || vector > 255 || (vector * 8) + 7 > t.idt_limit then
    fault 13 gate_error;
  let address = W.add t.idt_base (W.of_int (vector * 8)) in
  let low = read_linear ~supervisor:true t ~access:Read ~width:Dword address in
  let high =
    read_linear ~supervisor:true t ~access:Read ~width:Dword (W.add address 4l)
  in
  let attributes = W.to_int (W.logand (W.shift_right_logical high 8) 255l) in
  let kind = attributes land 31 in
  if kind <> 14 && kind <> 15 then fault 13 gate_error;
  if software && cpl t > (attributes lsr 5) land 3 then fault 13 gate_error;
  if attributes land 128 = 0 then fault 11 gate_error;
  let selector = W.to_int (W.shift_right_logical low 16) in
  let target = code_descriptor t ~selector ~level:(cpl t) ~gate:true in
  let offset = W.logor (W.logand low 65535l) (W.logand high 0xffff0000l) in
  if U.compare (unsigned offset) target.limit > 0 then fault 13 0;
  let level = if target.access land 4 <> 0 then cpl t else dpl target in
  let stack, stack_pointer, outer_frame =
    if level = cpl t then (t.segments.(2), esp, [])
    else (
      if t.task_selector = 0 || 4 + (level * 8) + 5 > t.task_limit then
        fault 10 (t.task_selector land 0xfffc);
      let address = W.add t.task_base (W.of_int (4 + (level * 8))) in
      let pointer =
        read_linear ~supervisor:true t ~access:Read ~width:Dword address
      in
      let selector =
        W.to_int
          (read_linear ~supervisor:true t ~access:Read ~width:Word
             (W.add address 4l))
      in
      if selector land 0xfffc = 0 then fault 10 0;
      let stack, descriptor_address, descriptor_high = descriptor t selector in
      validate_data ~level ~index:2 stack;
      mark_accessed t descriptor_address descriptor_high;
      (stack, pointer, [ W.of_int t.segments.(2).selector; esp ]))
  in
  if not stack.wide then unsupported "16-bit interrupt stack is not implemented";
  let values =
    outer_frame
    @ [ eflags; W.of_int t.segments.(1).selector; eip ]
    @ Option.to_list (Option.map error ~f:W.of_int)
  in
  let final_sp = W.sub stack_pointer (W.of_int (4 * List.length values)) in
  let addresses =
    List.mapi values ~f:(fun index _ ->
        let offset = W.sub stack_pointer (W.of_int (4 * (index + 1))) in
        let address =
          segment_address stack ~index:2 ~access:Write ~offset ~width:4
        in
        check_linear ~supervisor:(level < 3) t ~access:Write ~address ~width:4;
        address)
  in
  List.iter2_exn addresses values ~f:(fun address value ->
      write_linear ~supervisor:(level < 3) t ~width:Dword address value);
  t.segments.(1) <- { target with selector = selector land 0xfffc lor level };
  t.segments.(2) <- stack;
  let clear = 0x34100l in
  let flags =
    W.logand eflags
      (W.lognot (if kind = 14 then W.logor clear 0x200l else clear))
  in
  (offset, final_sp, flags)

(* Restore execution state from an interrupt return frame. *)
let iret t ~esp ~eflags =
  if bit eflags 0x4000l then unsupported "Task-return IRET is not implemented";
  let get offset =
    read t ~segment:2 ~access:Read ~width:Dword (W.add esp (W.of_int offset))
  in
  let offset = get 0 in
  let selector = W.to_int (W.logand (get 4) 65535l) in
  let flags = get 8 in
  if bit flags 0x20000l then unsupported "Virtual-8086 mode is not implemented";
  let level = selector land 3 in
  if level < cpl t then fault 13 (selector land 0xfffc);
  let target = code_descriptor t ~selector ~level ~gate:false in
  if U.compare (unsigned offset) target.limit > 0 then fault 13 0;
  let stack, pointer =
    if level = cpl t then (t.segments.(2), W.add esp 12l)
    else
      let pointer = get 12 in
      let ss = W.to_int (W.logand (get 16) 65535l) in
      if ss land 0xfffc = 0 then fault 13 0;
      let segment, address, high = descriptor t ss in
      validate_data ~level ~index:2 segment;
      mark_accessed t address high;
      (segment, pointer)
  in
  let mask =
    if cpl t = 0 then 0x257fd5l
    else if cpl t <= W.to_int (W.logand (W.shift_right_logical eflags 12) 3l)
    then 0x254fd5l
    else 0x254dd5l
  in
  let flags =
    W.logor 2l (W.logor (W.logand flags mask) (W.logand eflags (W.lognot mask)))
  in
  t.segments.(1) <- target;
  t.segments.(2) <- stack;
  List.iter [ 0; 3; 4; 5 ] ~f:(fun index ->
      let segment = t.segments.(index) in
      if
        segment.valid
        && ((not (code segment)) || segment.access land 4 = 0)
        && dpl segment < level
      then t.segments.(index) <- empty 0);
  (offset, pointer, flags)

(* Read a supported model-specific register. *)
let read_msr t index =
  require_ring0 t;
  match index with
  | 0x17 | 0x8b -> 0L
  | 0x174 | 0x175 | 0x176 -> t.sysenter.(index - 0x174)
  | _ -> fault 13 0

(* Write a supported model-specific register. *)
let write_msr t index value =
  require_ring0 t;
  match index with
  | 0x8b when U.equal value 0L -> ()
  | 0x174 | 0x175 | 0x176 ->
      if not (U.equal (U.shift_right_logical value 32) 0L) then fault 13 0;
      t.sysenter.(index - 0x174) <- value
  | _ -> fault 13 0

(* Check whether the current privilege level permits an I/O port access. *)
let check_io t ~port ~width ~eflags =
  let iopl = W.to_int (W.logand (W.shift_right_logical eflags 12) 3l) in
  if cpl t > iopl then (
    if t.task_selector = 0 || t.task_limit < 103 then fault 13 0;
    let bitmap =
      W.to_int
        (read_linear ~supervisor:true t ~access:Read ~width:Word
           (W.add t.task_base 102l))
    in
    for byte = 0 to Width.bytes width - 1 do
      let port = port + byte in
      let offset = bitmap + (port lsr 3) in
      if offset > t.task_limit then fault 13 0;
      let mask =
        read_linear ~supervisor:true t ~access:Read ~width:Byte
          (W.add t.task_base (W.of_int offset))
      in
      if bit mask (W.shift_left 1l (port land 7)) then fault 13 0
    done)

(* Validate and normalize a debug register index. *)
let debug_index t index =
  require_ring0 t;
  if index = 4 || index = 5 then (
    if bit t.control.(4) 8l then fault 6 0;
    index + 2)
  else index

(* Read a debug register. *)
let read_debug t index = t.debug.(debug_index t index)

(* Write a debug register. *)
let write_debug t index value =
  let index = debug_index t index in
  if index = 7 && bit value 0x20ffl then
    unsupported "Hardware debug breakpoints are not implemented";
  t.debug.(index) <-
    (match index with
    | 6 -> W.logor (W.logand value 0xe00fl) 0xffff0ff0l
    | 7 -> W.logor (W.logand value 0xffff2fffl) 0x400l
    | _ -> value)

(* Look up a descriptor if it is visible at the current privilege level. *)
let visible_descriptor t selector =
  if selector land 0xfffc = 0 then None
  else
    try
      let segment, _, high = descriptor t selector in
      let conforming = segment.access land 0x1c = 0x1c in
      if conforming || Int.max (cpl t) (selector land 3) <= dpl segment then
        Some (segment, high)
      else None
    with Guest_fault { vector = 13; _ } -> None

(* Test whether a segment is accessible for reading or writing. *)
let verify_segment t ~write selector =
  match visible_descriptor t selector with
  | None -> false
  | Some (segment, _) ->
      segment.access land 0x10 <> 0
      &&
      if write then (not (code segment)) && segment.access land 2 <> 0
      else (not (code segment)) || segment.access land 2 <> 0

(* Query the access rights or limit of a visible segment. *)
let query_segment t ~limit selector =
  match visible_descriptor t selector with
  | None -> None
  | Some (segment, high) ->
      let allowed =
        segment.access land 0x10 <> 0
        || List.mem
             (if limit then [ 1; 2; 3; 9; 11 ] else [ 1; 2; 3; 4; 5; 9; 11; 12 ])
             (segment.access land 15) ~equal:Int.equal
      in
      if not allowed then None
      else
        Some
          (if limit then U.to_int32 segment.limit else W.logand high 0x00f0ff00l)
