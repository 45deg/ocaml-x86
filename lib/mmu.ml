(* Legacy 4 KiB/4 MiB and PAE 4 KiB/2 MiB walks. Physical address width is
    32 bits; NX and PSE-36 are not exposed. System_state caches successful
    walks and invalidates them when watched physical table pages change. *)

open Base
module W = Stdlib.Int32

type access = Read | Write | Execute

exception Page_fault of { address : int32; error : int }

let bit value mask = not (W.equal (W.logand value mask) 0l)

let translate bus ~cr0 ~cr3 ~cr4 ~user ~access address =
  if not (bit cr0 Int32.min_value) then address
  else
    let writing = Poly.equal access Write in
    let error = (if writing then 2 else 0) lor if user then 4 else 0 in
    let fault ~present ~reserved =
      raise
        (Page_fault
           {
             address;
             error =
               (error
               lor (if present then 1 else 0)
               lor if reserved then 8 else 0);
           })
    in
    let read entry_address =
      Physical_bus.watch_page_table bus entry_address;
      let entry = Physical_bus.read bus ~width:Dword entry_address in
      if not (bit entry 1l) then fault ~present:false ~reserved:false;
      entry
    in
    let mark entry_address entry mask =
      if not (W.equal (W.logand entry mask) mask) then
        Physical_bus.write bus ~width:Dword entry_address (W.logor entry mask)
    in
    let permission writable accessible =
      if
        (user && not accessible)
        || (writing && (not writable) && (user || bit cr0 0x10000l))
      then fault ~present:true ~reserved:false
    in
    let leaf entry_address entry ~writable ~accessible ~mask =
      permission writable accessible;
      mark entry_address entry (if writing then 0x60l else 0x20l);
      W.logor (W.logand entry (W.lognot mask)) (W.logand address mask)
    in
    if not (bit cr4 0x20l) then
      let directory_address =
        W.add (W.logand cr3 0xfffff000l)
          (W.shift_left (W.shift_right_logical address 22) 2)
      in
      let directory = read directory_address in
      if bit directory 0x80l then (
        if (not (bit cr4 0x10l)) || bit directory 0x003fe000l then
          fault ~present:true ~reserved:true;
        leaf directory_address directory ~writable:(bit directory 2l)
          ~accessible:(bit directory 4l) ~mask:0x003fffffl)
      else (
        mark directory_address directory 0x20l;
        let table_address =
          W.add
            (W.logand directory 0xfffff000l)
            (W.logand (W.shift_right_logical address 10) 0xffcl)
        in
        let table = read table_address in
        leaf table_address table
          ~writable:(bit directory 2l && bit table 2l)
          ~accessible:(bit directory 4l && bit table 4l)
          ~mask:0xfffl)
    else
      let read64 entry_address =
        let low = read entry_address in
        let high =
          Physical_bus.read bus ~width:Dword (W.add entry_address 4l)
        in
        if not (W.equal (W.logand high 0x800fffffl) 0l) then
          fault ~present:true ~reserved:true;
        low
      in
      let pdpt_address =
        W.add (W.logand cr3 0xffffffe0l)
          (W.shift_left (W.shift_right_logical address 30) 3)
      in
      let pdpt = read64 pdpt_address in
      if bit pdpt 0x1e6l then fault ~present:true ~reserved:true;
      let directory_address =
        W.add
          (W.logand pdpt 0xfffff000l)
          (W.logand (W.shift_right_logical address 18) 0xff8l)
      in
      let directory = read64 directory_address in
      if bit directory 0x80l then (
        if bit directory 0x001fe000l then fault ~present:true ~reserved:true;
        leaf directory_address directory ~writable:(bit directory 2l)
          ~accessible:(bit directory 4l) ~mask:0x001fffffl)
      else (
        mark directory_address directory 0x20l;
        let table_address =
          W.add
            (W.logand directory 0xfffff000l)
            (W.logand (W.shift_right_logical address 9) 0xff8l)
        in
        let table = read64 table_address in
        leaf table_address table
          ~writable:(bit directory 2l && bit table 2l)
          ~accessible:(bit directory 4l && bit table 4l)
          ~mask:0xfffl)
