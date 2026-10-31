(** Physical RAM. Every access checks its complete unsigned address range. *)
type t

exception
  Access_fault of
    { address : int32
    ; width : int
    }

val create : size:int -> t
val size : t -> int
val check_access : t -> address:int32 -> width:int -> unit
val read_u8 : t -> address:int32 -> int
val read_u16 : t -> address:int32 -> int
val read_u32 : t -> address:int32 -> int32
val write_u8 : t -> address:int32 -> int -> unit
val write_u16 : t -> address:int32 -> int -> unit
val write_u32 : t -> address:int32 -> int32 -> unit
val load : t -> address:int32 -> string -> unit

(** Page walks register their physical table pages. Writes to any registered
    page advance the epoch, including writes through aliases and [load]. *)
val watch_page_table : t -> address:int32 -> unit

val translation_epoch : t -> int
