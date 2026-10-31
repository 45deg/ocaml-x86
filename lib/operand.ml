(* Operand decoded in 32-bit address mode. Displacements and effective
    addresses wrap modulo 2^32. Decoding does not access operand memory. *)

open Base
module W = Stdlib.Int32

type t = Register of Register.t | Memory of int32
type modrm = { segment : int; extension : int; rm : t }

let decode ?(address_size = Width.Dword) ~byte ~word ~register () =
  let encoding = byte () in
  let mode = encoding lsr 6 in
  let extension = (encoding lsr 3) land 7 in
  let rm = encoding land 7 in
  let reg n = register (Register.of_encoding n) in
  let segment = ref 3 in
  let rm =
    if mode = 3 then Register (Register.of_encoding rm)
    else if Poly.equal address_size Width.Word then (
      let short () =
        let low = byte () in
        let high = byte () in
        W.of_int (low lor (high lsl 8))
      in
      if rm = 2 || rm = 3 || (rm = 6 && mode <> 0) then segment := 2;
      let base =
        match rm with
        | 0 -> W.add (reg 3) (reg 6)
        | 1 -> W.add (reg 3) (reg 7)
        | 2 -> W.add (reg 5) (reg 6)
        | 3 -> W.add (reg 5) (reg 7)
        | 4 -> reg 6
        | 5 -> reg 7
        | 6 -> if mode = 0 then short () else reg 5
        | _ -> reg 3
      in
      let displacement =
        match mode with
        | 0 -> 0l
        | 1 -> Width.sign_extend Width.Byte (W.of_int (byte ()))
        | _ -> short ()
      in
      Memory (Width.truncate Width.Word (W.add base displacement)))
    else
      let base =
        if rm = 4 then (
          let sib = byte () in
          let scale = sib lsr 6 in
          let index = (sib lsr 3) land 7 in
          let base = sib land 7 in
          if base = 4 || (base = 5 && mode <> 0) then segment := 2;
          let base = if base = 5 && mode = 0 then word () else reg base in
          let index =
            if index = 4 then 0l else W.shift_left (reg index) scale
          in
          W.add base index)
        else if rm = 5 && mode = 0 then word ()
        else (
          if rm = 5 then segment := 2;
          reg rm)
      in
      let displacement =
        match mode with
        | 0 -> 0l
        | 1 ->
            let value = byte () in
            W.of_int (if value >= 128 then value - 256 else value)
        | _ -> word ()
      in
      Memory (W.add base displacement)
  in
  { segment = !segment; extension; rm }
