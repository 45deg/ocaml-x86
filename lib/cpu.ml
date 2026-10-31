open Base
module W = Stdlib.Int32

(* CPU registers, execution state, memory system, and attached devices. *)
type t = {
  memory : Memory.t;
  fpu : Fpu.t;
  system : System_state.t;
  registers : int32 array;
  mutable eip : int32;
  mutable eflags : int32;
  mutable halted : bool;
  mutable deliver_faults : bool;
  mutable interrupt_shadow : int;
  mutable pending_interrupt : int option;
  mutable cycles : int64;
  mutable device_cycles : int;
  mutable devices : Devices.t option;
  output_u8 : port:int -> int -> unit;
}

(* A guest execution failure reported by the interpreter. *)
type fault =
  | System_fault of { eip : int32; message : string }
  | Unsupported_opcode of { eip : int32; opcode : int }
  | Memory_fault of { eip : int32; address : int32; width : int }

(* The reason CPU execution stopped. *)
type stop = Halted | Budget_exhausted | Fault of fault

(* Create a CPU with the supplied memory and byte output callback. *)
let create ~memory ~output_u8 =
  {
    memory;
    fpu = Fpu.create ();
    system = System_state.create ~memory;
    registers = Array.create ~len:8 0l;
    eip = 0l;
    eflags = 2l;
    halted = false;
    deliver_faults = false;
    interrupt_shadow = 0;
    pending_interrupt = None;
    device_cycles = 0;
    devices = None;
    cycles = 0L;
    output_u8;
  }

(* Read a general-purpose register. *)
let register t r = t.registers.(Register.encoding r)
(* Write a general-purpose register. *)
let set_register t r value = t.registers.(Register.encoding r) <- value
(* Return the current instruction pointer. *)
let eip t = t.eip
(* Set the instruction pointer. *)
let set_eip t value = t.eip <- value
(* Return the current flags register. *)
let eflags t = t.eflags
(* Read a control register from the system state. *)
let control t index = System_state.read_control t.system index

(* Read a byte relative to the current code segment. *)
let read_code_byte t ~offset =
  W.to_int
    (System_state.read t.system ~segment:1 ~access:Execute ~width:Byte
       (W.add t.eip (W.of_int offset)))

(* Initialize CPU state for the loaded Linux boot image. *)
let initialize_linux t (image : Linux_boot.t) =
  System_state.enable_pc_mode t.system;
  t.deliver_faults <- true;
  System_state.set_gdt t.system ~base:image.gdt ~limit:31;
  List.iter [ 0; 1; 2; 3; 4; 5 ] ~f:(fun index ->
      System_state.load_segment t.system ~memory:t.memory ~index
        ~selector:(if index = 1 then 0x10 else 0x18));
  t.eip <- image.entry;
  t.eflags <- 2l;
  set_register t Esi image.boot_params;
  set_register t Esp image.stack;
  List.iter [ Register.Ebx; Ebp; Edi ] ~f:(fun r -> set_register t r 0l)

(* Decode and execute one guest instruction. *)
let step t =
  let start = t.eip in
  let cursor = ref start in
  let consumed = ref 0 in
  let default_size = System_state.code_width t.system in
  let size = ref default_size in
  let address_size = ref default_size in
  let segment_override = ref None in
  let operand_segment = ref 3 in
  let data_segment default = Option.value !segment_override ~default in
  let repeat = ref 0 in
  let locked = ref false in
  let instruction_code = ref 0 in
  let extended_code = ref 0 in
  (* Keep EIP at the instruction start while decoding, so a fault can restart it.
     The cursor also enforces the architectural 15-byte instruction limit. *)
  let byte () =
    if !consumed >= 15 then raise Stdlib.Exit;
    let value =
      W.to_int
        (System_state.read t.system ~segment:1 ~access:Execute ~width:Byte
           !cursor)
    in
    cursor := W.add !cursor 1l;
    Int.incr consumed;
    value
  in
  (* Assemble instruction bytes in x86 little-endian order. *)
  let immediate width =
    let value = ref 0l in
    for offset = 0 to Width.bytes width - 1 do
      let next = byte () in
      value := W.logor !value (W.shift_left (W.of_int next) (offset * 8))
    done;
    !value
  in
  let word () = immediate Width.Dword in
  let relative_byte () = Width.sign_extend Byte (W.of_int (byte ())) in
  let read_segment segment width address =
    System_state.read t.system ~segment ~access:Read ~width address
  in
  let write_segment segment width address value =
    System_state.write t.system ~segment ~width address value
  in
  let read_memory width address =
    read_segment (data_segment !operand_segment) width address
  in
  let write_memory width address value =
    write_segment (data_segment !operand_segment) width address value
  in
  (* Byte encodings 4-7 select AH, CH, DH, and BH, not ESP through EDI. *)
  let register_part width r =
    let encoding = Register.encoding r in
    if Poly.equal width Width.Byte && encoding >= 4 then
      (Register.of_encoding (encoding - 4), 8)
    else (r, 0)
  in
  (* Register accesses honor partial-register widths; memory accesses use the
     decoded default segment unless a prefix overrides it. *)
  let read width = function
    | Operand.Register r ->
        let r, shift = register_part width r in
        Width.truncate width (W.shift_right_logical (register t r) shift)
    | Operand.Memory address -> read_memory width address
  in
  let write width operand value =
    match operand with
    | Operand.Register r ->
        let r, shift = register_part width r in
        let mask = W.shift_left (Width.mask width) shift in
        set_register t r
          (W.logor
             (W.logand (register t r) (W.lognot mask))
             (W.shift_left (Width.truncate width value) shift))
    | Operand.Memory address -> write_memory width address value
  in
  let reg n = Operand.Register (Register.of_encoding n) in
  (* Decode the operand before validating LOCK, which requires both the opcode
     extension and knowledge of whether the destination is memory. *)
  let decode () =
    let operands =
      Operand.decode ~address_size:!address_size ~byte ~word
        ~register:(register t) ()
    in
    (* LOCK is valid only for supported read-modify-write memory operands. *)
    (if !locked then
       let memory =
         match operands.rm with Operand.Memory _ -> true | _ -> false
       in
       let permitted =
         match !instruction_code with
         | n when n < 0x38 && n land 7 <= 1 -> true
         | 0x80 | 0x81 | 0x83 -> operands.extension <> 7
         | 0x86 | 0x87 -> true
         | 0xfe | 0xff -> operands.extension <= 1
         | 0xf6 | 0xf7 -> operands.extension = 2 || operands.extension = 3
         | 0x0f ->
             List.mem
               [ 0xb0; 0xb1; 0xc0; 0xc1; 0xab; 0xb3; 0xbb ]
               !extended_code ~equal:Int.equal
             || (!extended_code = 0xba && operands.extension >= 5)
             || (!extended_code = 0xc7 && operands.extension = 1)
         | _ -> false
       in
       if (not memory) || not permitted then System_state.fault 6 0);
    operand_segment := operands.segment;
    operands
  in
  let push value =
    let address = W.sub (register t Esp) (W.of_int (Width.bytes !size)) in
    (* Commit ESP only after the stack write succeeds. *)
    write_segment 2 !size address value;
    set_register t Esp address
  in
  let pop () =
    let address = register t Esp in
    let value = read_segment 2 !size address in
    set_register t Esp (W.add address (W.of_int (Width.bytes !size)));
    value
  in
  (* Validate the new segment before committing ESP; loading SS also delays
     interrupt delivery until after the following instruction. *)
  let pop_segment index =
    let address = register t Esp in
    let selector = W.to_int (W.logand (read_segment 2 !size address) 0xffffl) in
    System_state.load_segment t.system ~memory:t.memory ~index ~selector;
    set_register t Esp (W.add address (W.of_int (Width.bytes !size)));
    if index = 2 then t.interrupt_shadow <- 2
  in
  let flag mask = not (W.equal (W.logand t.eflags mask) 0l) in
  let operation width = function
    | 0 -> Alu.add ~width ~carry:false
    | 1 -> Alu.logor ~width
    | 2 -> Alu.add ~width ~carry:(flag 1l)
    | 3 -> Alu.sub ~width ~carry:(flag 1l)
    | 4 -> Alu.logand ~width
    | 5 | 7 -> Alu.sub ~width ~carry:false
    | _ -> Alu.logxor ~width
  in
  let apply width operation ~destination ~source ~store =
    let result = operation (read width destination) source in
    if store then write width destination result.Alu.value;
    t.eflags <- W.logor (W.logand t.eflags (W.lognot 0x8d5l)) result.flags
  in
  (* Condition codes are paired: the low bit negates the base condition. *)
  let condition code =
    let carry = flag 1l in
    let zero = flag 64l in
    let sign = flag 128l in
    let overflow = flag 2048l in
    let positive =
      match code lsr 1 with
      | 0 -> overflow
      | 1 -> carry
      | 2 -> zero
      | 3 -> carry || zero
      | 4 -> sign
      | 5 -> flag 4l
      | 6 -> not (Bool.equal sign overflow)
      | _ -> zero || not (Bool.equal sign overflow)
    in
    if code land 1 = 0 then positive else not positive
  in
  let jump displacement = cursor := W.add !cursor displacement in
  let near_jump displacement =
    jump displacement;
    cursor := Width.truncate !size !cursor
  in
  let set_flag mask enabled =
    t.eflags <-
      W.logor (W.logand t.eflags (W.lognot mask)) (if enabled then mask else 0l)
  in
  let multiply_flags overflow =
    set_flag 1l overflow;
    set_flag 0x800l overflow
  in
  let address_register r = read !address_size (Operand.Register r) in
  let set_address_register r value =
    write !address_size (Operand.Register r) value
  in
  let opcode = ref 0 in
  try
    if Poly.equal (System_state.stack_width t.system) Width.Word then
      System_state.unsupported "16-bit stack addressing is not implemented";
    (* Consume all prefixes, leaving the first non-prefix byte as the opcode.
       Repeated size overrides select the opposite default, not a second toggle. *)
    let rec prefixes () =
      let next = byte () in
      opcode := next;
      if next = 0x66 then (
        size :=
          if Poly.equal default_size Width.Word then Width.Dword else Width.Word;
        prefixes ())
      else if next = 0x67 then (
        address_size :=
          if Poly.equal default_size Width.Word then Width.Dword else Width.Word;
        prefixes ())
      else if
        List.mem [ 0x26; 0x2e; 0x36; 0x3e; 0x64; 0x65 ] next ~equal:Int.equal
      then (
        segment_override :=
          Some
            (match next with
            | 0x26 -> 0
            | 0x2e -> 1
            | 0x36 -> 2
            | 0x3e -> 3
            | 0x64 -> 4
            | _ -> 5);
        prefixes ())
      else if next = 0xf0 then (
        locked := true;
        prefixes ())
      else if next = 0xf2 || next = 0xf3 then (
        repeat := next;
        prefixes ())
      else next
    in
    let instruction = prefixes () in
    instruction_code := instruction;
    if
      !locked
      && not
           ((instruction < 0x38 && instruction land 7 <= 1)
           || List.mem
                [ 0x80; 0x81; 0x83; 0x86; 0x87; 0xfe; 0xff; 0xf6; 0xf7; 0x0f ]
                instruction ~equal:Int.equal)
    then System_state.fault 6 0;
    let width = if instruction land 1 = 0 then Width.Byte else !size in
    let supported =
      match instruction with
      | 0xea ->
          let offset = immediate !size in
          let selector = W.to_int (immediate Width.Word) in
          System_state.far_jump t.system ~selector ~offset;
          cursor := offset;
          true
      | 0xcd | 0xcc | 0xce ->
          let vector =
            if instruction = 0xcd then byte ()
            else if instruction = 0xcc then 3
            else 4
          in
          if instruction <> 0xce || flag 0x800l then (
            let offset, esp, flags =
              System_state.interrupt t.system ~vector ~software:true ~error:None
                ~eip:!cursor ~esp:(register t Esp) ~eflags:t.eflags
            in
            cursor := offset;
            set_register t Esp esp;
            t.eflags <- flags);
          true
      | 0xcf ->
          if Poly.equal !size Width.Word then
            System_state.unsupported "16-bit IRET is not implemented";
          let offset, esp, flags =
            System_state.iret t.system ~esp:(register t Esp) ~eflags:t.eflags
          in
          cursor := offset;
          set_register t Esp esp;
          t.eflags <- flags;
          true
      | 0xfa ->
          if
            System_state.cpl t.system
            > W.to_int (W.logand (W.shift_right_logical t.eflags 12) 3l)
          then System_state.fault 13 0;
          t.eflags <- W.logand t.eflags (W.lognot 0x200l);
          true
      | 0xfb ->
          if
            System_state.cpl t.system
            > W.to_int (W.logand (W.shift_right_logical t.eflags 12) 3l)
          then System_state.fault 13 0;
          t.interrupt_shadow <- 2;
          t.eflags <- W.logor t.eflags 0x200l;
          true
      | 0x9b ->
          let cr0 = System_state.read_control t.system 0 in
          if W.equal (W.logand cr0 0xal) 0xal then System_state.fault 7 0;
          if Fpu.status t.fpu land 0x80 <> 0 then System_state.fault 16 0;
          true
      | 0xd8 | 0xd9 | 0xda | 0xdb | 0xdc | 0xdd | 0xde | 0xdf ->
          let cr0 = System_state.read_control t.system 0 in
          if not (W.equal (W.logand cr0 0xcl) 0l) then System_state.fault 7 0;
          let modrm =
            W.to_int
              (System_state.read t.system ~segment:1 ~access:Execute ~width:Byte
                 !cursor)
          in
          let operands = decode () in
          (* x87 state transfers can cross pages; preflight the entire write
             before storing any bytes or changing the FPU state. *)
          let store address bytes =
            System_state.check t.system
              ~segment:(data_segment !operand_segment)
              ~access:Write ~address ~width:(String.length bytes);
            String.iteri bytes ~f:(fun offset byte ->
                write_memory Byte
                  (W.add address (W.of_int offset))
                  (W.of_int (Char.to_int byte)))
          in
          let load address size =
            String.init size ~f:(fun offset ->
                Char.of_int_exn
                  (W.to_int
                     (read_memory Byte (W.add address (W.of_int offset)))))
          in
          let supported =
            match (instruction, operands.extension, operands.rm) with
            | 0xdd, 0, Operand.Register r ->
                Fpu.free t.fpu (Register.encoding r);
                true
            | 0xdd, ((4 | 5) as operation), Operand.Register r ->
                Fpu.compare t.fpu (Fpu.get t.fpu (Register.encoding r));
                if operation = 5 then Fpu.pop t.fpu;
                true
            | 0xda, 5, Operand.Register Ecx ->
                Fpu.compare t.fpu (Fpu.get t.fpu 1);
                Fpu.pop t.fpu;
                Fpu.pop t.fpu;
                true
            | (0xdb | 0xdf), (5 | 6), Operand.Register r ->
                let order =
                  Extended.compare (Fpu.get t.fpu 0)
                    (Fpu.get t.fpu (Register.encoding r))
                in
                t.eflags <- W.logand t.eflags (W.lognot 0x8d5l);
                set_flag 1l (order < 0);
                set_flag 0x40l (order = 0);
                if instruction = 0xdf then Fpu.pop t.fpu;
                true
            | (0xda | 0xdb), operation, Operand.Register r when operation < 4 ->
                let test =
                  match operation with 0 -> 2 | 1 -> 4 | 2 -> 6 | _ -> 10
                in
                let value = Fpu.get t.fpu (Register.encoding r) in
                if condition (if instruction = 0xdb then test lor 1 else test)
                then Fpu.set t.fpu 0 value;
                true
            | (0xd8 | 0xdc | 0xda | 0xde), operation, Operand.Memory address ->
                let value =
                  if instruction = 0xd8 || instruction = 0xdc then
                    Extended.of_ieee
                      (load address (if instruction = 0xd8 then 4 else 8))
                  else
                    let width =
                      if instruction = 0xda then Width.Dword else Width.Word
                    in
                    Extended.of_int64
                      (Stdlib.Int64.of_int32
                         (Width.sign_extend width (read_memory width address)))
                in
                if operation = 2 || operation = 3 then (
                  Fpu.compare t.fpu value;
                  if operation = 3 then Fpu.pop t.fpu)
                else
                  Fpu.set t.fpu 0
                    (Extended.binary ~control:(Fpu.control t.fpu) ~operation
                       (Fpu.get t.fpu 0) value);
                true
            | (0xdb | 0xdf), ((2 | 3) as operation), Operand.Memory address ->
                store address
                  (Extended.to_integer ~control:(Fpu.control t.fpu)
                     ~bits:(if instruction = 0xdb then 32 else 16)
                     (Fpu.get t.fpu 0));
                if operation = 3 then Fpu.pop t.fpu;
                true
            | 0xdf, 7, Operand.Memory address ->
                store address
                  (Extended.to_integer ~control:(Fpu.control t.fpu) ~bits:64
                     (Fpu.get t.fpu 0));
                Fpu.pop t.fpu;
                true
            | 0xdf, 5, Operand.Memory address ->
                let bytes = load address 8 in
                let value = ref 0L in
                String.iteri bytes ~f:(fun index byte ->
                    value :=
                      Stdlib.Int64.logor !value
                        (Stdlib.Int64.shift_left
                           (Stdlib.Int64.of_int (Char.to_int byte))
                           (index * 8)));
                Fpu.push t.fpu (Extended.of_int64 !value);
                true
            | 0xd9, 4, Operand.Register ((Eax | Ecx) as r) ->
                let value = Bytes.of_string (Fpu.get t.fpu 0) in
                let sign = Char.to_int (Bytes.get value 9) in
                Bytes.set value 9
                  (Char.of_int_exn
                     (if Poly.equal r Eax then sign lxor 128 else sign land 127));
                Fpu.set t.fpu 0 (Bytes.to_string value);
                true
            | 0xd9, 4, Operand.Register Esp ->
                Fpu.compare t.fpu (Extended.of_int64 0L);
                true
            | 0xd9, 2, Operand.Register Eax -> true
            | 0xd9, 5, Operand.Register Eax ->
                Fpu.push t.fpu (Extended.of_int64 1L);
                true
            | 0xd9, 5, Operand.Register Esi ->
                Fpu.push t.fpu (Extended.of_int64 0L);
                true
            | 0xd9, 0, Operand.Register r ->
                Fpu.push t.fpu (Fpu.get t.fpu (Register.encoding r));
                true
            | 0xd9, 1, Operand.Register r ->
                let index = Register.encoding r in
                let a = Fpu.get t.fpu 0 and b = Fpu.get t.fpu index in
                Fpu.set t.fpu 0 b;
                Fpu.set t.fpu index a;
                true
            | 0xdd, ((2 | 3) as operation), Operand.Register r ->
                Fpu.set t.fpu (Register.encoding r) (Fpu.get t.fpu 0);
                if operation = 3 then Fpu.pop t.fpu;
                true
            | (0xd8 | 0xdc | 0xde), operation, Operand.Register r ->
                let index = Register.encoding r in
                (if operation = 2 || operation = 3 then (
                   Fpu.compare t.fpu (Fpu.get t.fpu index);
                   if operation = 3 then Fpu.pop t.fpu;
                   if instruction = 0xde then Fpu.pop t.fpu)
                 else
                   let destination, source =
                     if instruction = 0xd8 then (0, index) else (index, 0)
                   in
                   let operation =
                     if instruction <> 0xd8 && operation >= 4 then
                       operation lxor 1
                     else operation
                   in
                   let value =
                     Extended.binary ~control:(Fpu.control t.fpu) ~operation
                       (Fpu.get t.fpu destination)
                       (Fpu.get t.fpu source)
                   in
                   Fpu.set t.fpu destination value;
                   if instruction = 0xde then Fpu.pop t.fpu);
                true
            | (0xd9 | 0xdd), 0, Operand.Memory address ->
                Fpu.push t.fpu
                  (Extended.of_ieee
                     (load address (if instruction = 0xd9 then 4 else 8)));
                true
            | (0xd9 | 0xdd), ((2 | 3) as operation), Operand.Memory address ->
                store address
                  (Extended.to_ieee ~control:(Fpu.control t.fpu)
                     ~size:(if instruction = 0xd9 then 4 else 8)
                     (Fpu.get t.fpu 0));
                if operation = 3 then Fpu.pop t.fpu;
                true
            | 0xdb, 5, Operand.Memory address ->
                Fpu.push t.fpu (load address 10);
                true
            | 0xdb, 7, Operand.Memory address ->
                store address (Fpu.get t.fpu 0);
                Fpu.pop t.fpu;
                true
            | (0xdb | 0xdf), 0, Operand.Memory address ->
                let width =
                  if instruction = 0xdb then Width.Dword else Width.Word
                in
                Fpu.push t.fpu
                  (Extended.of_int64
                     (Stdlib.Int64.of_int32
                        (Width.sign_extend width (read_memory width address))));
                true
            | 0xdb, 4, Operand.Register Ebx ->
                Fpu.reset t.fpu;
                true
            | 0xdb, 4, Operand.Register Edx ->
                Fpu.clear_exceptions t.fpu;
                true
            | 0xdf, 4, Operand.Register Eax ->
                write Word (reg 0) (W.of_int (Fpu.status t.fpu));
                true
            | 0xd9, 7, Operand.Memory address ->
                write_memory Word address (W.of_int (Fpu.control t.fpu));
                true
            | 0xdd, 7, Operand.Memory address ->
                write_memory Word address (W.of_int (Fpu.status t.fpu));
                true
            | 0xd9, 5, Operand.Memory address ->
                Fpu.set_control t.fpu (W.to_int (read_memory Word address));
                true
            | 0xdd, 6, Operand.Memory address ->
                store address (Fpu.save t.fpu ~operand_size:!size);
                Fpu.reset t.fpu;
                true
            | 0xdd, 4, Operand.Memory address ->
                Fpu.restore t.fpu ~operand_size:!size
                  (load address (if Poly.equal !size Word then 94 else 108));
                true
            | 0xd9, 6, Operand.Memory address ->
                store address (Fpu.environment t.fpu ~operand_size:!size);
                Fpu.set_control t.fpu (Fpu.control t.fpu lor 0x3f);
                true
            | 0xd9, 4, Operand.Memory address ->
                Fpu.load_environment t.fpu ~operand_size:!size
                  (load address (if Poly.equal !size Word then 14 else 28));
                true
            | _ -> false
          in
          let control_instruction =
            match (instruction, operands.extension, operands.rm) with
            | 0xd9, extension, Operand.Memory _ when extension >= 4 -> true
            | 0xdd, (4 | 6 | 7), Operand.Memory _ -> true
            | 0xdb, 4, Operand.Register _ | 0xdf, 4, Operand.Register Eax ->
                true
            | _ -> false
          in
          (if supported && not control_instruction then
             let data =
               match operands.rm with
               | Operand.Register _ -> None
               | Operand.Memory address ->
                   Some
                     ( address,
                       System_state.selector t.system
                         (data_segment !operand_segment) )
             in
             Fpu.record t.fpu ~instruction:start
               ~code_segment:(System_state.selector t.system 1)
               ~opcode:(((instruction land 7) lsl 8) lor modrm)
               ~data);
          supported
      | 0x8e ->
          let operands = decode () in
          if operands.extension = 1 || operands.extension > 5 then false
          else
            let selector = W.to_int (read Width.Word operands.rm) in
            System_state.load_segment t.system ~memory:t.memory
              ~index:operands.extension ~selector;
            if operands.extension = 2 then t.interrupt_shadow <- 2;
            true
      | 0x8c ->
          let operands = decode () in
          if operands.extension > 5 then false
          else
            let width =
              match operands.rm with
              | Operand.Register _ -> !size
              | Operand.Memory _ -> Width.Word
            in
            write width operands.rm
              (W.of_int (System_state.selector t.system operands.extension));
            true
      | 0xfc ->
          t.eflags <- W.logand t.eflags (W.lognot 0x400l);
          true
      | 0xfd ->
          t.eflags <- W.logor t.eflags 0x400l;
          true
      | 0xf8 ->
          t.eflags <- W.logand t.eflags (W.lognot 1l);
          true
      | 0xf9 ->
          t.eflags <- W.logor t.eflags 1l;
          true
      | 0xf5 ->
          t.eflags <- W.logxor t.eflags 1l;
          true
      | n when n >= 0x90 && n <= 0x97 ->
          let left = read !size (reg 0) in
          let right = read !size (reg (n land 7)) in
          write !size (reg 0) right;
          write !size (reg (n land 7)) left;
          true
      | 0x98 ->
          let source_width =
            if Poly.equal !size Width.Word then Width.Byte else Width.Word
          in
          write !size (reg 0)
            (Width.sign_extend source_width (read source_width (reg 0)));
          true
      | 0x99 ->
          let value =
            if W.compare (Width.sign_extend !size (read !size (reg 0))) 0l < 0
            then -1l
            else 0l
          in
          write !size (reg 2) value;
          true
      | 0x0f -> (
          let second = byte () in
          extended_code := second;
          if
            !locked
            && not
                 (List.mem
                    [ 0xb0; 0xb1; 0xc0; 0xc1; 0xab; 0xb3; 0xbb; 0xba; 0xc7 ]
                    second ~equal:Int.equal)
          then System_state.fault 6 0;
          match second with
          | 0xc7 -> (
              let operands = decode () in
              match (operands.extension, operands.rm) with
              | 1, Operand.Memory address ->
                  (* CMPXCHG8B requires write access even when comparison fails.
                 Preflight all eight bytes before changing guest state. *)
                  System_state.check t.system
                    ~segment:(data_segment !operand_segment)
                    ~access:Write ~address ~width:8;
                  let high_address = W.add address 4l in
                  let low = read_memory Dword address in
                  let high = read_memory Dword high_address in
                  let equal =
                    W.equal low (register t Eax)
                    && W.equal high (register t Edx)
                  in
                  let new_low, new_high =
                    if equal then (register t Ebx, register t Ecx)
                    else (low, high)
                  in
                  write_memory Dword address new_low;
                  write_memory Dword high_address new_high;
                  if not equal then (
                    set_register t Eax low;
                    set_register t Edx high);
                  set_flag 0x40l equal;
                  true
              | _ -> System_state.fault 6 0)
          | 0xa0 | 0xa8 ->
              push
                (W.of_int
                   (System_state.selector t.system
                      (if second = 0xa0 then 4 else 5)));
              true
          | 0xa1 | 0xa9 ->
              pop_segment (if second = 0xa1 then 4 else 5);
              true
          | n when n >= 0x40 && n <= 0x4f ->
              let operands = decode () in
              let value = read !size operands.rm in
              if condition (n land 15) then
                write !size (reg operands.extension) value;
              true
          | 0xaf ->
              let operands = decode () in
              let result =
                Multiply.multiply ~width:!size ~signed:true
                  (read !size (reg operands.extension))
                  (read !size operands.rm)
              in
              write !size (reg operands.extension) result.low;
              multiply_flags result.overflow;
              true
          | 0xa4 | 0xa5 | 0xac | 0xad ->
              let operands = decode () in
              let count =
                if second land 1 = 0 then byte ()
                else W.to_int (read Width.Byte (reg 1))
              in
              let result =
                Bit_operation.double_shift ~width:!size
                  ~right:(second land 8 <> 0)
                  ~count ~flags:t.eflags ~destination:(read !size operands.rm)
                  ~source:(read !size (reg operands.extension))
              in
              if count land 31 <> 0 then write !size operands.rm result.value;
              t.eflags <- result.flags;
              true
          | 0xbc | 0xbd ->
              let operands = decode () in
              let index =
                Bit_operation.scan ~reverse:(second = 0xbd) ~width:!size
                  (read !size operands.rm)
              in
              (match index with
              | None -> set_flag 64l true
              | Some index ->
                  write !size (reg operands.extension) (W.of_int index);
                  set_flag 64l false);
              true
          | n when n >= 0xc8 && n <= 0xcf ->
              if Poly.equal !size Width.Word then false
              else
                let r = Register.of_encoding (n land 7) in
                set_register t r (Bit_operation.byte_swap (register t r));
                true
          | 0xa3 | 0xab | 0xb3 | 0xbb | 0xba ->
              let operands = decode () in
              let kind =
                if second = 0xba then operands.extension
                else ((second lsr 3) land 3) + 4
              in
              if second = 0xba && kind < 4 then false
              else
                let immediate_index = second = 0xba in
                let index =
                  if immediate_index then W.of_int (byte ())
                  else
                    Width.sign_extend !size
                      (read !size (reg operands.extension))
                in
                let bits = Width.bits !size in
                let bit = W.to_int (W.logand index (W.of_int (bits - 1))) in
                let destination =
                  match operands.rm with
                  | Operand.Memory address when not immediate_index ->
                      let units =
                        W.shift_right index (if bits = 16 then 4 else 5)
                      in
                      Operand.Memory
                        (W.add address
                           (W.mul units (W.of_int (Width.bytes !size))))
                  | other -> other
                in
                let value = read !size destination in
                let mask = W.shift_left 1l bit in
                let updated =
                  match kind land 3 with
                  | 1 -> W.logor value mask
                  | 2 -> W.logand value (W.lognot mask)
                  | 3 -> W.logxor value mask
                  | _ -> value
                in
                if kind land 3 <> 0 then write !size destination updated;
                set_flag 1l (not (W.equal (W.logand value mask) 0l));
                true
          | 0xb0 | 0xb1 | 0xc0 | 0xc1 ->
              let width = if second land 1 = 0 then Width.Byte else !size in
              let operands = decode () in
              let destination = read width operands.rm in
              let source = read width (reg operands.extension) in
              (if second < 0xc0 then (
                 let result = Alu.sub ~width (read width (reg 0)) destination in
                 if W.equal result.value 0l then write width operands.rm source
                 else write width (reg 0) destination;
                 t.eflags <-
                   W.logor (W.logand t.eflags (W.lognot 0x8d5l)) result.flags)
               else
                 let result = Alu.add ~width destination source in
                 (* Memory write can fault, so perform it before the register write. *)
                 (match operands.rm with
                 | Operand.Memory _ ->
                     write width operands.rm result.value;
                     write width (reg operands.extension) destination
                 | Operand.Register _ ->
                     write width (reg operands.extension) destination;
                     write width operands.rm result.value);
                 t.eflags <-
                   W.logor (W.logand t.eflags (W.lognot 0x8d5l)) result.flags);
              true
          | 0xa2 ->
              let leaf = register t Eax in
              let eax, ebx, ecx, edx =
                if W.equal leaf 0l then
                  (1l, 0x756e6547l, 0x6c65746el, 0x49656e69l)
                else if W.equal leaf 1l then (0x633l, 0l, 0l, 0xa179l)
                else if W.equal leaf 0x80000000l then (0x80000001l, 0l, 0l, 0l)
                else (0l, 0l, 0l, 0l)
              in
              set_register t Eax eax;
              set_register t Ebx ebx;
              set_register t Ecx ecx;
              set_register t Edx edx;
              true
          | 0x30 | 0x32 ->
              System_state.require_ring0 t.system;
              let index = W.to_int (register t Ecx) in
              (if second = 0x32 then (
                 let value =
                   if index = 0x10 then t.cycles
                   else System_state.read_msr t.system index
                 in
                 set_register t Eax (Stdlib.Int64.to_int32 value);
                 set_register t Edx
                   (Stdlib.Int64.to_int32
                      (Stdlib.Int64.shift_right_logical value 32)))
               else
                 let value =
                   Stdlib.Int64.logor
                     (Stdlib.Int64.logand
                        (Stdlib.Int64.of_int32 (register t Eax))
                        0xffffffffL)
                     (Stdlib.Int64.shift_left
                        (Stdlib.Int64.of_int32 (register t Edx))
                        32)
                 in
                 if index = 0x10 then t.cycles <- value
                 else System_state.write_msr t.system index value);
              true
          | 0x06 ->
              System_state.require_ring0 t.system;
              System_state.write_control t.system 0
                (W.logand (System_state.read_control t.system 0) (W.lognot 8l));
              true
          | 0x08 | 0x09 ->
              System_state.require_ring0 t.system;
              true
          | 0x31 ->
              if
                System_state.cpl t.system <> 0
                && not
                     (W.equal
                        (W.logand (System_state.read_control t.system 4) 4l)
                        0l)
              then System_state.fault 13 0;
              set_register t Eax (Stdlib.Int64.to_int32 t.cycles);
              set_register t Edx
                (Stdlib.Int64.to_int32
                   (Stdlib.Int64.shift_right_logical t.cycles 32));
              true
          | 0x00 -> (
              let operands = decode () in
              match operands.extension with
              | 0 ->
                  write Width.Word operands.rm
                    (W.of_int (System_state.ldt_selector t.system));
                  true
              | 1 ->
                  write Width.Word operands.rm
                    (W.of_int (System_state.task_selector t.system));
                  true
              | 2 ->
                  System_state.load_ldt t.system
                    (W.to_int (read Width.Word operands.rm));
                  true
              | 3 ->
                  System_state.load_task t.system
                    (W.to_int (read Width.Word operands.rm));
                  true
              | 4 | 5 ->
                  let selector = W.to_int (read Width.Word operands.rm) in
                  set_flag 0x40l
                    (System_state.verify_segment t.system
                       ~write:(operands.extension = 5) selector);
                  true
              | _ -> false)
          | 0x02 | 0x03 ->
              let operands = decode () in
              let selector = W.to_int (read Width.Word operands.rm) in
              let value =
                System_state.query_segment t.system ~limit:(second = 0x03)
                  selector
              in
              set_flag 0x40l (Option.is_some value);
              Option.iter value ~f:(write !size (reg operands.extension));
              true
          | 0x20 | 0x21 | 0x22 | 0x23 -> (
              System_state.require_ring0 t.system;
              let operands = decode () in
              match operands.rm with
              | Operand.Memory _ -> false
              | Operand.Register r ->
                  (match second with
                  | 0x20 ->
                      set_register t r
                        (System_state.read_control t.system operands.extension)
                  | 0x21 ->
                      set_register t r
                        (System_state.read_debug t.system operands.extension)
                  | 0x22 ->
                      System_state.write_control t.system operands.extension
                        (register t r)
                  | _ ->
                      System_state.write_debug t.system operands.extension
                        (register t r));
                  true)
          | 0x01 -> (
              let operands = decode () in
              match operands.extension with
              | 4 ->
                  write Width.Word operands.rm
                    (W.logand (System_state.read_control t.system 0) 0xffffl);
                  true
              | 6 ->
                  System_state.require_ring0 t.system;
                  let value = W.logand (read Width.Word operands.rm) 15l in
                  let old = System_state.read_control t.system 0 in
                  System_state.write_control t.system 0
                    (W.logor (W.logand old (W.lognot 14l)) value);
                  true
              | _ -> (
                  match operands.rm with
                  | Operand.Register _ -> false
                  | Operand.Memory address ->
                      let linear access =
                        System_state.linear_address t.system
                          ~segment:(data_segment operands.segment)
                          ~access ~offset:address ~width:6
                      in
                      if operands.extension = 7 then (
                        System_state.require_ring0 t.system;
                        System_state.invalidate_translations t.system;
                        true)
                      else if operands.extension = 2 || operands.extension = 3
                      then (
                        System_state.require_ring0 t.system;
                        let address = linear Read in
                        let limit =
                          W.to_int
                            (System_state.read_linear t.system ~access:Read
                               ~width:Word address)
                        in
                        let base =
                          System_state.read_linear t.system ~access:Read
                            ~width:Dword (W.add address 2l)
                        in
                        let base =
                          if Poly.equal !size Width.Word then
                            W.logand base 0xffffffl
                          else base
                        in
                        (if operands.extension = 2 then System_state.set_gdt
                         else System_state.set_idt)
                          t.system ~base ~limit;
                        true)
                      else if operands.extension = 0 || operands.extension = 1
                      then (
                        let address = linear Write in
                        let base, limit =
                          (if operands.extension = 0 then System_state.gdt
                           else System_state.idt)
                            t.system
                        in
                        System_state.check_linear t.system ~access:Write
                          ~address ~width:6;
                        System_state.write_linear t.system ~width:Word address
                          (W.of_int limit);
                        System_state.write_linear t.system ~width:Dword
                          (W.add address 2l) base;
                        true)
                      else false))
          | n when n >= 0x80 && n <= 0x8f ->
              let displacement = immediate !size in
              if condition (n land 15) then near_jump displacement;
              true
          | n when n >= 0x90 && n <= 0x9f ->
              let operands = decode () in
              write Width.Byte operands.rm
                (if condition (n land 15) then 1l else 0l);
              true
          | 0xb6 | 0xb7 | 0xbe | 0xbf ->
              let operands = decode () in
              let source_width =
                if second land 1 = 0 then Width.Byte else Width.Word
              in
              let value = read source_width operands.rm in
              let value =
                if second land 8 <> 0 then Width.sign_extend source_width value
                else value
              in
              write !size (reg operands.extension) value;
              true
          | _ -> false)
      | 0xa4 | 0xa5 | 0xa6 | 0xa7 | 0xaa | 0xab | 0xac | 0xad | 0xae | 0xaf ->
          if !repeat = 0 || not (W.equal (address_register Ecx) 0l) then (
            let source = address_register Esi in
            let destination = address_register Edi in
            let kind = instruction land 0xfe in
            let comparing = kind = 0xa6 || kind = 0xae in
            (match kind with
            | 0xa4 ->
                let value = read_segment (data_segment 3) width source in
                write_segment 0 width destination value
            | 0xaa -> write_segment 0 width destination (read width (reg 0))
            | 0xac ->
                write width (reg 0) (read_segment (data_segment 3) width source)
            | _ ->
                let left =
                  if kind = 0xa6 then read_segment (data_segment 3) width source
                  else read width (reg 0)
                in
                let right = read_segment 0 width destination in
                let result = Alu.sub ~width left right in
                t.eflags <-
                  W.logor (W.logand t.eflags (W.lognot 0x8d5l)) result.flags);
            let delta =
              W.of_int ((if flag 0x400l then -1 else 1) * Width.bytes width)
            in
            if kind = 0xa4 || kind = 0xa6 || kind = 0xac then
              set_address_register Esi (W.add source delta);
            if kind <> 0xac then
              set_address_register Edi (W.add destination delta);
            if !repeat <> 0 then (
              let remaining =
                Width.truncate !address_size (W.sub (address_register Ecx) 1l)
              in
              set_address_register Ecx remaining;
              let again =
                (not comparing) || Bool.equal (flag 64l) (!repeat = 0xf3)
              in
              if (not (W.equal remaining 0l)) && again then cursor := start));
          true
      | 0x9c ->
          push (W.logand t.eflags 0x00fcffffl);
          true
      | 0x9d ->
          let value = pop () in
          let privilege_mask =
            if System_state.cpl t.system = 0 then 0x247fd5l
            else if
              System_state.cpl t.system
              <= W.to_int (W.logand (W.shift_right_logical t.eflags 12) 3l)
            then 0x244fd5l
            else 0x244dd5l
          in
          let mask =
            if Poly.equal !size Width.Word then W.logand privilege_mask 65535l
            else privilege_mask
          in
          t.eflags <-
            W.logor 2l
              (W.logor
                 (W.logand t.eflags (W.lognot mask))
                 (W.logand value mask));
          true
      | 0xc0 | 0xc1 | 0xd0 | 0xd1 | 0xd2 | 0xd3 ->
          let operands = decode () in
          let count =
            if instruction < 0xd0 then byte ()
            else if instruction < 0xd2 then 1
            else W.to_int (read Width.Byte (reg 1))
          in
          let value = read width operands.rm in
          let result =
            Shift.execute ~width ~kind:operands.extension ~count ~flags:t.eflags
              value
          in
          if count land 31 <> 0 then write width operands.rm result.value;
          t.eflags <- result.flags;
          true
      | 0xf6 | 0xf7 -> (
          let operands = decode () in
          match operands.extension with
          | 0 ->
              let source = immediate width in
              apply width (Alu.logand ~width) ~destination:operands.rm ~source
                ~store:false;
              true
          | 2 ->
              let value = read width operands.rm in
              write width operands.rm (W.lognot value);
              true
          | 3 ->
              let value = read width operands.rm in
              let result = Alu.sub ~width 0l value in
              write width operands.rm result.value;
              t.eflags <-
                W.logor (W.logand t.eflags (W.lognot 0x8d5l)) result.flags;
              true
          | 4 | 5 ->
              let result =
                Multiply.multiply ~width ~signed:(operands.extension = 5)
                  (read width (reg 0))
                  (read width operands.rm)
              in
              if Poly.equal width Width.Byte then
                write Width.Word (reg 0)
                  (W.logor result.low (W.shift_left result.high 8))
              else (
                write width (reg 0) result.low;
                write width (reg 2) result.high);
              multiply_flags result.overflow;
              true
          | 6 | 7 ->
              let high =
                if Poly.equal width Width.Byte then read Width.Byte (reg 4)
                else read width (reg 2)
              in
              let quotient, remainder =
                Multiply.divide ~width ~signed:(operands.extension = 7) ~high
                  ~low:(read width (reg 0))
                  (read width operands.rm)
              in
              write width (reg 0) quotient;
              write width
                (reg (if Poly.equal width Width.Byte then 4 else 2))
                remainder;
              true
          | _ -> false)
      | 0xfe | 0xff ->
          let operands = decode () in
          let group = operands.extension in
          if group <= 1 then (
            let carry = W.logand t.eflags 1l in
            apply width
              (operation width (if group = 0 then 0 else 5))
              ~destination:operands.rm ~source:1l ~store:true;
            t.eflags <- W.logor (W.logand t.eflags (W.lognot 1l)) carry;
            true)
          else if instruction = 0xff && (group = 2 || group = 4 || group = 6)
          then (
            let value = read !size operands.rm in
            if group = 6 then push value
            else (
              if group = 2 then push !cursor;
              cursor := value);
            true)
          else false
      | 0xf4 ->
          System_state.require_ring0 t.system;
          t.halted <- true;
          true
      | n when n >= 0xb0 && n <= 0xbf ->
          let width = if n < 0xb8 then Width.Byte else !size in
          let value = immediate width in
          write width (reg (n land 7)) value;
          true
      | 0x86 | 0x87 ->
          let operands = decode () in
          let left = read width operands.rm in
          let right = read width (reg operands.extension) in
          write width operands.rm right;
          write width (reg operands.extension) left;
          true
      | 0x69 | 0x6b ->
          let operands = decode () in
          let source =
            if instruction = 0x6b then relative_byte () else immediate !size
          in
          let result =
            Multiply.multiply ~width:!size ~signed:true (read !size operands.rm)
              source
          in
          write !size (reg operands.extension) result.low;
          multiply_flags result.overflow;
          true
      | 0x9e ->
          let value = read Width.Byte (reg 4) in
          t.eflags <-
            W.logor (W.logand t.eflags (W.lognot 0xd5l)) (W.logand value 0xd5l);
          true
      | 0x9f ->
          write Width.Byte (reg 4) (W.logand t.eflags 0xd7l);
          true
      | 0x88 | 0x89 | 0x8a | 0x8b ->
          let operands = decode () in
          let r = reg operands.extension in
          let destination, source =
            if instruction land 2 = 0 then (operands.rm, r) else (r, operands.rm)
          in
          let value = read width source in
          write width destination value;
          true
      | 0x8d -> (
          let operands = decode () in
          match operands.rm with
          | Operand.Register _ -> false
          | Operand.Memory address ->
              write !size (reg operands.extension) address;
              true)
      | 0xc6 | 0xc7 ->
          let operands = decode () in
          if operands.extension <> 0 then false
          else
            let value = immediate width in
            write width operands.rm value;
            true
      | n when n < 0x40 && n land 7 <= 5 ->
          let group = n lsr 3 in
          let operation = operation width group in
          (if n land 7 < 4 then
             let operands = decode () in
             let r = reg operands.extension in
             let destination, source =
               if n land 2 = 0 then (operands.rm, r) else (r, operands.rm)
             in
             apply width operation ~destination ~source:(read width source)
               ~store:(group <> 7)
           else
             let source = immediate width in
             apply width operation ~destination:(reg 0) ~source
               ~store:(group <> 7));
          true
      | 0x80 | 0x81 | 0x83 ->
          let width = if instruction = 0x80 then Width.Byte else !size in
          let operands = decode () in
          let source =
            if instruction = 0x83 then relative_byte () else immediate width
          in
          apply width
            (operation width operands.extension)
            ~destination:operands.rm ~source ~store:(operands.extension <> 7);
          true
      | 0x84 | 0x85 ->
          let operands = decode () in
          apply width (Alu.logand ~width) ~destination:operands.rm
            ~source:(read width (reg operands.extension))
            ~store:false;
          true
      | 0xa8 | 0xa9 ->
          let source = immediate width in
          apply width (Alu.logand ~width) ~destination:(reg 0) ~source
            ~store:false;
          true
      | n when n >= 0x40 && n <= 0x4f ->
          let carry = W.logand t.eflags 1l in
          apply !size
            (operation !size (if n < 0x48 then 0 else 5))
            ~destination:(reg (n land 7))
            ~source:1l ~store:true;
          t.eflags <- W.logor (W.logand t.eflags (W.lognot 1l)) carry;
          true
      | 0xa0 | 0xa1 | 0xa2 | 0xa3 ->
          let address = immediate !address_size in
          if instruction land 2 = 0 then
            write width (reg 0) (read_memory width address)
          else write_memory width address (read width (reg 0));
          true
      | 0x06 | 0x0e | 0x16 | 0x1e ->
          push (W.of_int (System_state.selector t.system (instruction lsr 3)));
          true
      | 0x07 | 0x17 | 0x1f ->
          pop_segment (instruction lsr 3);
          true
      | n when n >= 0x50 && n <= 0x57 ->
          push (read !size (reg (n land 7)));
          true
      | n when n >= 0x58 && n <= 0x5f ->
          let value = pop () in
          write !size (reg (n land 7)) value;
          true
      | 0x68 | 0x6a ->
          let value =
            if instruction = 0x6a then relative_byte () else immediate !size
          in
          push value;
          true
      | 0xe8 ->
          let displacement = immediate !size in
          push !cursor;
          near_jump displacement;
          true
      | 0xc2 | 0xc3 ->
          let cleanup =
            if instruction = 0xc2 then immediate Width.Word else 0l
          in
          cursor := pop ();
          set_register t Esp (W.add (register t Esp) cleanup);
          true
      | 0xc8 ->
          let allocation = immediate Width.Word in
          let nesting = byte () land 31 in
          let bytes = Width.bytes !size in
          let original_sp = register t Esp in
          let frame = W.sub original_sp (W.of_int bytes) in
          let frame_values =
            List.init
              (Int.max 0 (nesting - 1))
              ~f:(fun index ->
                let address =
                  W.sub (read !size (reg 5)) (W.of_int ((index + 1) * bytes))
                in
                read_segment 2 !size
                  (if Poly.equal !size Width.Word then
                     Width.truncate Width.Word address
                   else address))
          in
          let values =
            read !size (reg 5)
            :: (frame_values @ if nesting = 0 then [] else [ frame ])
          in
          let final_sp =
            W.sub original_sp (W.of_int (List.length values * bytes))
          in
          System_state.check t.system ~segment:2 ~access:Write ~address:final_sp
            ~width:(List.length values * bytes);
          let allocated_sp = W.sub final_sp allocation in
          System_state.check t.system ~segment:2 ~access:Write
            ~address:allocated_sp ~width:bytes;
          List.iter values ~f:push;
          write !size (reg 5) frame;
          set_register t Esp allocated_sp;
          true
      | 0xd7 ->
          let address =
            Width.truncate !address_size
              (W.add (address_register Ebx) (read Width.Byte (reg 0)))
          in
          write Width.Byte (reg 0) (read_memory Width.Byte address);
          true
      | 0xc9 ->
          let address = register t Ebp in
          let value = read_segment 2 !size address in
          set_register t Esp (W.add address (W.of_int (Width.bytes !size)));
          write !size (reg 5) value;
          true
      | 0x60 ->
          let original_sp = register t Esp in
          let bytes = Width.bytes !size in
          let final_sp = W.sub original_sp (W.of_int (8 * bytes)) in
          System_state.check t.system ~segment:2 ~access:Write ~address:final_sp
            ~width:(8 * bytes);
          let values = Array.init 8 ~f:(fun index -> read !size (reg index)) in
          Array.iter values ~f:push;
          true
      | 0x61 ->
          let original_sp = register t Esp in
          let bytes = Width.bytes !size in
          System_state.check t.system ~segment:2 ~access:Read
            ~address:original_sp ~width:(8 * bytes);
          let values =
            Array.init 8 ~f:(fun index ->
                read_segment 2 !size
                  (W.add original_sp (W.of_int (index * bytes))))
          in
          Array.iteri values ~f:(fun index value ->
              if index <> 3 then write !size (reg (7 - index)) value);
          set_register t Esp (W.add original_sp (W.of_int (8 * bytes)));
          true
      | 0x8f ->
          (* ESP-based destinations use the post-increment stack pointer. *)
          let original_sp = register t Esp in
          let next_sp = W.add original_sp (W.of_int (Width.bytes !size)) in
          let operands =
            Operand.decode ~address_size:!address_size ~byte ~word
              ~register:(fun r ->
                if Poly.equal r Register.Esp then next_sp else register t r)
              ()
          in
          operand_segment := operands.segment;
          if operands.extension <> 0 then false
          else
            let value = read_segment 2 !size original_sp in
            (match operands.rm with
            | Operand.Register Register.Esp ->
                set_register t Esp next_sp;
                write !size operands.rm value
            | _ ->
                write !size operands.rm value;
                set_register t Esp next_sp);
            true
      | 0xe0 | 0xe1 | 0xe2 | 0xe3 ->
          let displacement = relative_byte () in
          let count = address_register Ecx in
          let take =
            if instruction = 0xe3 then W.equal count 0l
            else
              let count = Width.truncate !address_size (W.sub count 1l) in
              set_address_register Ecx count;
              (not (W.equal count 0l))
              && (instruction = 0xe2
                 || Bool.equal (flag 64l) (instruction = 0xe1))
          in
          if take then jump displacement;
          true
      | 0xe9 ->
          let displacement = immediate !size in
          near_jump displacement;
          true
      | 0xeb ->
          let displacement = relative_byte () in
          jump displacement;
          true
      | n when n >= 0x70 && n <= 0x7f ->
          let displacement = relative_byte () in
          if condition (n land 15) then jump displacement;
          true
      | (0xe4 | 0xe5 | 0xe6 | 0xe7 | 0xec | 0xed | 0xee | 0xef) as opcode ->
          let width = if opcode land 1 = 0 then Width.Byte else !size in
          let port =
            if opcode < 0xec then byte ()
            else W.to_int (W.logand (register t Edx) 0xffffl)
          in
          System_state.check_io t.system ~port ~width ~eflags:t.eflags;
          (if opcode land 2 = 0 then
             let value =
               match t.devices with
               | Some devices -> Devices.read devices ~port ~width
               | None -> Width.mask width
             in
             write width (reg 0) value
           else
             let value = read width (reg 0) in
             match t.devices with
             | Some devices -> Devices.write devices ~port ~width value
             | None -> t.output_u8 ~port (W.to_int (W.logand value 255l)));
          true
      | _ -> false
    in
    if supported then (
      t.eip <- !cursor;
      None)
    else Some (Unsupported_opcode { eip = start; opcode = !opcode })
  with
  | Multiply.Divide_error ->
      raise (System_state.Guest_fault { vector = 0; error = None })
  | Extended.Unsupported message | System_state.Unsupported message ->
      Some (System_fault { eip = start; message })
  | Stdlib.Exit -> Some (Unsupported_opcode { eip = start; opcode = !opcode })
  | Memory.Access_fault { address; width } ->
      Some (Memory_fault { eip = start; address; width })

(* Queue an interrupt vector for delivery. *)
let request_interrupt t vector =
  if vector < 0 || vector > 255 then invalid_arg "Interrupt vector out of range";
  t.pending_interrupt <- Some vector

(* Deliver an interrupt or exception, handling faults during delivery. *)
let deliver t ~vector ~error =
  let rec attempt vector error depth =
    try
      let eip, esp, flags =
        System_state.interrupt t.system ~vector ~software:false ~error
          ~eip:t.eip ~esp:(register t Esp) ~eflags:t.eflags
      in
      t.eip <- eip;
      set_register t Esp esp;
      t.eflags <- flags;
      t.halted <- false;
      None
    with
    | System_state.Guest_fault { vector = next; error = next_error } ->
        if vector = 8 || depth >= 3 then
          Some
            (System_fault
               {
                 eip = t.eip;
                 message = "Triple fault during exception delivery";
               })
        else
          (* Certain pairs of faults during delivery become a double fault. *)
          let contributory v =
            List.mem [ 0; 10; 11; 12; 13 ] v ~equal:Int.equal
          in
          if
            (contributory vector && contributory next)
            || (vector = 14 && (next = 14 || contributory next))
          then attempt 8 (Some 0) (depth + 1)
          else attempt next next_error (depth + 1)
    | Memory.Access_fault { address; width } ->
        Some (Memory_fault { eip = t.eip; address; width })
    | Extended.Unsupported message | System_state.Unsupported message ->
        Some (System_fault { eip = t.eip; message })
  in
  attempt vector error 0

(* Attach the PC devices used for port I/O and interrupts. *)
let attach_devices t devices = t.devices <- Some devices

(* Execute instructions until the CPU stops or the step budget is exhausted. *)
let run t ~max_steps =
  if max_steps < 0 then invalid_arg "Cpu.run: negative budget";
  (* Advance devices in batches, including while HLT waits for an interrupt. *)
  let rec loop remaining =
    if remaining = 0 then
      if t.halted && Option.is_none t.devices then Halted else Budget_exhausted
    else (
      Option.iter t.devices ~f:(fun devices ->
          t.device_cycles <- (t.device_cycles + if t.halted then 1024 else 1);
          if t.halted then t.cycles <- Stdlib.Int64.add t.cycles 1024L;
          if t.device_cycles >= 1024 then (
            Devices.advance devices ~cycles:t.device_cycles;
            t.device_cycles <- 0);
          if
            t.interrupt_shadow = 0
            && (not (W.equal (W.logand t.eflags 0x200l) 0l))
            && Option.is_none t.pending_interrupt
          then t.pending_interrupt <- Devices.interrupt devices);
      let interrupt =
        t.interrupt_shadow = 0
        && (not (W.equal (W.logand t.eflags 0x200l) 0l))
        && Option.is_some t.pending_interrupt
      in
      if t.halted && not interrupt then
        if Option.is_none t.devices then Halted else loop (remaining - 1)
      else (
        t.cycles <- Stdlib.Int64.succ t.cycles;
        let result =
          if interrupt then (
            let vector = Option.value_exn t.pending_interrupt in
            t.pending_interrupt <- None;
            deliver t ~vector ~error:None)
          else
            try step t
            with System_state.Guest_fault { vector; error } ->
              if t.deliver_faults then deliver t ~vector ~error
              else
                Some
                  (System_fault
                     {
                       eip = t.eip;
                       message = Printf.sprintf "Guest exception #%d" vector;
                     })
        in
        t.interrupt_shadow <- Int.max 0 (t.interrupt_shadow - 1);
        match result with
        | Some fault -> Fault fault
        | None -> loop (remaining - 1)))
  in
  loop max_steps
