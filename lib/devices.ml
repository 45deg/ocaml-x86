(* Minimal single-CPU PC: cascaded 8259 PIC, 8254 PIT, COM1 UART,
    deterministic CMOS clock, and an empty PCI configuration bus.
    The virtual TSC/CPU clock is 100 MHz. *)

open Base
module W = Stdlib.Int32

type pic =
  { mutable vector : int
  ; mutable mask : int
  ; mutable requests : int
  ; mutable service : int
  ; mutable init : int
  ; mutable icw4 : bool
  ; mutable auto_eoi : bool
  ; mutable read_service : bool
  }

type timer =
  { mutable reload : int
  ; mutable remaining : int
  ; mutable mode : int
  ; mutable access : int
  ; mutable low : int option
  ; mutable read_high : bool
  ; mutable latch : int option
  ; mutable status : int option
  ; mutable enabled : bool
  ; mutable output : bool
  }

type t =
  { master : pic
  ; slave : pic
  ; timers : timer array
  ; mutable phase : int
  ; mutable elapsed_cycles : int64
  ; uart : int array
  ; rx : char Queue.t
  ; mutable fifo : bool
  ; mutable tx_irq : bool
  ; mutable uart_irq : bool
  ; output : char -> unit
  ; mutable cmos_index : int
  ; cmos : int array
  ; mutable speaker : int
  ; mutable pci_address : int32
  }

let pic vector =
  { vector
  ; mask = 255
  ; requests = 0
  ; service = 0
  ; init = 0
  ; icw4 = false
  ; auto_eoi = false
  ; read_service = false
  }
;;

let timer () =
  { reload = 65536
  ; remaining = 65536
  ; mode = 3
  ; access = 3
  ; low = None
  ; read_high = false
  ; latch = None
  ; status = None
  ; enabled = true
  ; output = true
  }
;;

let create ~output =
  let cmos = Array.create ~len:128 0 in
  cmos.(0xa) <- 0x26;
  cmos.(0xb) <- 2;
  cmos.(0xd) <- 128;
  cmos.(0x15) <- 0x80;
  cmos.(0x16) <- 2;
  cmos.(0x17) <- 255;
  cmos.(0x18) <- 255;
  cmos.(0x30) <- 255;
  cmos.(0x31) <- 255;
  cmos.(0x35) <- 7;
  let timers = Array.init 3 ~f:(fun _ -> timer ()) in
  timers.(2).enabled <- false;
  { master = pic 8
  ; slave = pic 0x70
  ; timers
  ; phase = 0
  ; elapsed_cycles = 0L
  ; uart = Array.create ~len:8 0
  ; rx = Queue.create ()
  ; fifo = false
  ; tx_irq = true
  ; uart_irq = false
  ; output
  ; cmos_index = 0
  ; cmos
  ; speaker = 0
  ; pci_address = 0l
  }
;;

let first bits =
  let rec loop index =
    if index = 8
    then None
    else if bits land (1 lsl index) <> 0
    then Some index
    else loop (index + 1)
  in
  if bits = 0 then None else loop 0
;;

let eligible pic =
  let pending = pic.requests land lnot pic.mask in
  if pending = 0
  then None
  else (
    let priority = Option.value (first pic.service) ~default:8 in
    first (pending land ((1 lsl priority) - 1)))
;;

let request t irq =
  if irq < 8
  then t.master.requests <- t.master.requests lor (1 lsl irq)
  else (
    t.slave.requests <- t.slave.requests lor (1 lsl (irq - 8));
    t.master.requests <- t.master.requests lor 4)
;;

let update_uart t =
  let pending =
    ((t.uart.(1) land 1 <> 0 && not (Queue.is_empty t.rx))
     || (t.uart.(1) land 2 <> 0 && t.tx_irq))
    && t.uart.(4) land 8 <> 0
  in
  if pending && not t.uart_irq then request t 4;
  t.uart_irq <- pending
;;

let input t text =
  String.iter text ~f:(fun byte -> Queue.enqueue t.rx byte);
  update_uart t
;;

let acknowledge pic irq =
  pic.requests <- pic.requests land lnot (1 lsl irq);
  if not pic.auto_eoi then pic.service <- pic.service lor (1 lsl irq);
  pic.vector + irq
;;

let interrupt t =
  if Option.is_some (eligible t.slave) then t.master.requests <- t.master.requests lor 4;
  match eligible t.master with
  | None -> None
  | Some 2 ->
    ignore (acknowledge t.master 2 : int);
    Some
      (match eligible t.slave with
       | None -> t.slave.vector + 7
       | Some irq -> acknowledge t.slave irq)
  | Some irq -> Some (acknowledge t.master irq)
;;

let write_pic pic ~command value =
  if command
  then (
    if value land 16 <> 0
    then (
      pic.init <- 1;
      pic.icw4 <- value land 1 <> 0;
      pic.mask <- 0;
      pic.service <- 0;
      pic.requests <- 0;
      pic.auto_eoi <- false)
    else if value land 8 <> 0
    then (if value land 2 <> 0 then pic.read_service <- value land 1 <> 0)
    else if value land 0x20 <> 0
    then (
      let irq = if value land 0x40 <> 0 then Some (value land 7) else first pic.service in
      Option.iter irq ~f:(fun irq -> pic.service <- pic.service land lnot (1 lsl irq))))
  else (
    match pic.init with
    | 1 ->
      pic.vector <- value land 0xf8;
      pic.init <- 2
    | 2 -> pic.init <- (if pic.icw4 then 3 else 0)
    | 3 ->
      pic.auto_eoi <- value land 2 <> 0;
      pic.init <- 0
    | _ -> pic.mask <- value)
;;

let timer_count timer = timer.remaining land 65535

let timer_status (timer : timer) =
  (if timer.output then 128 else 0) lor (timer.access lsl 4) lor (timer.mode lsl 1)
;;

let read_timer timer =
  match timer.status with
  | Some status ->
    timer.status <- None;
    status
  | None ->
    let count = Option.value timer.latch ~default:(timer_count timer) in
    let high = timer.access = 2 || (timer.access = 3 && timer.read_high) in
    let value = if high then count lsr 8 else count land 255 in
    if timer.access = 3 then timer.read_high <- not timer.read_high;
    if timer.access <> 3 || not timer.read_high then timer.latch <- None;
    value
;;

let load_timer timer count =
  timer.reload <- (if count = 0 then 65536 else count);
  timer.remaining <- timer.reload;
  timer.enabled <- true;
  timer.output <- timer.mode <> 0
;;

let write_timer timer value =
  match timer.access with
  | 1 -> load_timer timer value
  | 2 -> load_timer timer (value lsl 8)
  | _ ->
    (match timer.low with
     | None -> timer.low <- Some value
     | Some low ->
       timer.low <- None;
       load_timer timer (low lor (value lsl 8)))
;;

let timer_command t value =
  let channel = value lsr 6 in
  if channel = 3
  then
    Array.iteri t.timers ~f:(fun index timer ->
      if value land (2 lsl index) = 0
      then (
        if value land 0x20 = 0 && Option.is_none timer.latch
        then timer.latch <- Some (timer_count timer);
        if value land 0x10 = 0 then timer.status <- Some (timer_status timer)))
  else (
    let timer = t.timers.(channel) in
    let access = (value lsr 4) land 3 in
    if access = 0
    then (if Option.is_none timer.latch then timer.latch <- Some (timer_count timer))
    else (
      timer.access <- access;
      timer.mode <- (value lsr 1) land 7;
      if timer.mode >= 6 then timer.mode <- timer.mode - 4;
      timer.low <- None;
      timer.read_high <- false;
      timer.latch <- None))
;;

let advance t ~cycles =
  t.elapsed_cycles <- Stdlib.Int64.add t.elapsed_cycles (Stdlib.Int64.of_int cycles);
  t.phase <- t.phase + (cycles * 1193182);
  let ticks = t.phase / 100000000 in
  t.phase <- t.phase % 100000000;
  Array.iteri t.timers ~f:(fun channel timer ->
    if timer.enabled && ticks > 0
    then (
      let remaining = timer.remaining - ticks in
      if remaining <= 0
      then (
        if channel = 0 then request t 0;
        if timer.mode = 2 || timer.mode = 3
        then timer.remaining <- timer.reload - (-remaining % timer.reload)
        else (
          timer.remaining <- 0;
          timer.enabled <- false;
          timer.output <- true))
      else timer.remaining <- remaining;
      if timer.mode = 3 then timer.output <- timer.remaining > timer.reload / 2));
  update_uart t
;;

let uart_read t index =
  let dlab = t.uart.(3) land 128 <> 0 in
  match index with
  | 0 when not dlab ->
    (match Queue.dequeue t.rx with
     | None -> 0
     | Some byte -> Char.to_int byte)
  | 0 -> t.uart.(0)
  | 1 when dlab -> t.uart.(2)
  | 1 -> t.uart.(1)
  | 2 ->
    let reason =
      if t.uart.(1) land 1 <> 0 && not (Queue.is_empty t.rx)
      then 4
      else if t.uart.(1) land 2 <> 0 && t.tx_irq
      then (
        t.tx_irq <- false;
        2)
      else 1
    in
    reason lor if t.fifo then 0xc0 else 0
  | 5 -> 0x60 lor if Queue.is_empty t.rx then 0 else 1
  | 6 ->
    if t.uart.(4) land 16 = 0
    then 0xb0
    else
      ((t.uart.(4) land 2) lsl 3)
      lor ((t.uart.(4) land 1) lsl 5)
      lor ((t.uart.(4) land 12) lsl 4)
  | _ -> t.uart.(index)
;;

let uart_write t index value =
  let dlab = t.uart.(3) land 128 <> 0 in
  match index with
  | 0 when not dlab ->
    if t.uart.(4) land 16 <> 0
    then Queue.enqueue t.rx (Char.of_int_exn value)
    else t.output (Char.of_int_exn value);
    t.tx_irq <- true
  | 0 -> t.uart.(0) <- value
  | 1 when dlab -> t.uart.(2) <- value
  | 1 ->
    if value land 2 <> 0 && t.uart.(1) land 2 = 0 then t.tx_irq <- true;
    t.uart.(1) <- value land 15
  | 2 ->
    t.fifo <- value land 1 <> 0;
    if value land 2 <> 0 then Queue.clear t.rx
  | 3 | 4 | 7 -> t.uart.(index) <- value
  | _ -> ()
;;

let read_cmos t =
  let seconds = Stdlib.Int64.to_int (Stdlib.Int64.div t.elapsed_cycles 100000000L) in
  let binary = t.cmos.(0xb) land 4 <> 0 in
  let encode value = if binary then value else (value / 10 * 16) + (value % 10) in
  match t.cmos_index with
  | 0 -> encode (seconds % 60)
  | 2 -> encode (seconds / 60 % 60)
  | 4 -> encode (seconds / 3600 % 24)
  | 6 -> 6
  | 7 -> encode 30
  | 8 -> encode 10
  | 9 -> encode 26
  | 0x32 -> encode 20
  | 0xc ->
    let value = t.cmos.(0xc) in
    t.cmos.(0xc) <- 0;
    value
  | index -> t.cmos.(index)
;;

let read_byte t port =
  match port with
  | 0x20 -> if t.master.read_service then t.master.service else t.master.requests
  | 0xa0 -> if t.slave.read_service then t.slave.service else t.slave.requests
  | 0x21 -> t.master.mask
  | 0xa1 -> t.slave.mask
  | 0x40 | 0x41 | 0x42 -> read_timer t.timers.(port - 0x40)
  | 0x61 -> t.speaker land 0xdf lor if t.timers.(2).output then 0x20 else 0
  | 0x64 -> 0
  | 0x71 -> read_cmos t
  | 0x92 -> 2
  | 0x4d0 | 0x4d1 -> 0
  | n when n >= 0x3f8 && n <= 0x3ff -> uart_read t (n - 0x3f8)
  | _ -> 255
;;

let write_byte t port value =
  match port with
  | 0x20 | 0x21 -> write_pic t.master ~command:(port = 0x20) value
  | 0xa0 | 0xa1 -> write_pic t.slave ~command:(port = 0xa0) value
  | 0x40 | 0x41 | 0x42 ->
    write_timer t.timers.(port - 0x40) value;
    if port = 0x42 then t.timers.(2).enabled <- t.speaker land 1 <> 0
  | 0x43 -> timer_command t value
  | 0x61 ->
    if value land 1 <> 0 && t.speaker land 1 = 0
    then load_timer t.timers.(2) t.timers.(2).reload;
    t.timers.(2).enabled <- value land 1 <> 0;
    t.speaker <- value
  | 0x70 -> t.cmos_index <- value land 127
  | 0x71 -> t.cmos.(t.cmos_index) <- value
  | n when n >= 0x3f8 && n <= 0x3ff -> uart_write t (n - 0x3f8) value
  | _ -> ()
;;

let read t ~port ~width =
  let value =
    if port = 0xcf8 && Poly.equal width Width.Dword
    then t.pci_address
    else (
      let value = ref 0l in
      for offset = 0 to Width.bytes width - 1 do
        value
        := W.logor
             !value
             (W.shift_left
                (W.of_int (read_byte t ((port + offset) land 65535)))
                (offset * 8))
      done;
      !value)
  in
  update_uart t;
  value
;;

let write t ~port ~width value =
  if port = 0xcf8 && Poly.equal width Width.Dword
  then t.pci_address <- value
  else
    for offset = 0 to Width.bytes width - 1 do
      write_byte
        t
        ((port + offset) land 65535)
        (W.to_int (W.logand (W.shift_right_logical value (offset * 8)) 255l))
    done;
  update_uart t
;;
