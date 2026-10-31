open Ocaml_os

let check condition message = if not condition then failwith message

let word actual expected =
  check (actual = expected) (Printf.sprintf "%lx <> %lx" actual expected)
;;

let signed n = if n >= 128 then n - 256 else n

let parity n =
  let count = ref 0 in
  for bit = 0 to 7 do
    count := !count + ((n lsr bit) land 1)
  done;
  !count mod 2 = 0
;;

let () =
  for left = 0 to 255 do
    for right = 0 to 255 do
      for carry = 0 to 1 do
        List.iter
          (fun subtract ->
             let result =
               (if subtract then Alu.sub else Alu.add)
                 ~width:Byte
                 ~carry:(carry = 1)
                 (Int32.of_int left)
                 (Int32.of_int right)
             in
             let full = if subtract then left - right - carry else left + right + carry in
             let signed_full =
               if subtract
               then signed left - signed right - carry
               else signed left + signed right + carry
             in
             let low = full land 255 in
             let auxiliary =
               if subtract
               then left land 15 < (right land 15) + carry
               else (left land 15) + (right land 15) + carry > 15
             in
             let flag mask = Int32.logand result.flags mask <> 0l in
             word result.value (Int32.of_int low);
             check (flag 1l = (full < 0 || full > 255)) "CF";
             check (flag 4l = parity low) "PF";
             check (flag 16l = auxiliary) "AF";
             check (flag 64l = (low = 0)) "ZF";
             check (flag 128l = (low >= 128)) "SF";
             check (flag 2048l = (signed_full < -128 || signed_full > 127)) "OF")
          [ false; true ]
      done
    done
  done;
  let memory = Memory.create ~size:4096 in
  (* MOV AX; ADD AX; MOV AH; ADD AL; MOV [EBX], AX; PUSH AX; POP CX. *)
  Memory.load
    memory
    ~address:0l
    "\x66\xb8\xff\xff\x66\x05\x01\x00\xb4\x12\x04\x34\x66\x89\x03\x66\x50\x66\x59\xf4";
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  Cpu.set_register cpu Eax 0xabcd_0000l;
  Cpu.set_register cpu Ecx 0xdead_0000l;
  Cpu.set_register cpu Ebx 256l;
  Cpu.set_register cpu Esp 4096l;
  check (Cpu.run cpu ~max_steps:100 = Halted) "16-bit guest";
  word (Cpu.register cpu Eax) 0xabcd_1234l;
  word (Cpu.register cpu Ecx) 0xdead_1234l;
  word (Cpu.register cpu Esp) 4096l;
  check (Memory.read_u16 memory ~address:256l = 0x1234) "16-bit store";
  Memory.load memory ~address:512l (String.make 16 '\x66');
  let cpu = Cpu.create ~memory ~output_u8:(fun ~port:_ _ -> ()) in
  Cpu.set_eip cpu 512l;
  check
    (match Cpu.run cpu ~max_steps:1 with
     | Fault (Unsupported_opcode _) -> true
     | _ -> false)
    "Instruction length limit";
  word (Cpu.eip cpu) 512l;
  print_endline "Operand widths and exhaustive 8-bit ADC/SBB tests passed"
;;
