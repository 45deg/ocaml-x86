(* The eight 32-bit x86 general-purpose registers. *)
type t = Eax | Ecx | Edx | Ebx | Esp | Ebp | Esi | Edi

(* Return the instruction encoding of a general-purpose register. *)
let encoding = function
  | Eax -> 0
  | Ecx -> 1
  | Edx -> 2
  | Ebx -> 3
  | Esp -> 4
  | Ebp -> 5
  | Esi -> 6
  | Edi -> 7

(* Decode a general-purpose register from its instruction encoding. *)
let of_encoding = function
  | 0 -> Eax
  | 1 -> Ecx
  | 2 -> Edx
  | 3 -> Ebx
  | 4 -> Esp
  | 5 -> Ebp
  | 6 -> Esi
  | 7 -> Edi
  | _ -> invalid_arg "Register.of_encoding"
