
open Lib.Mbr;;

(* take the first n elements from the list*)
let rec take_n lst n =
  match (n, lst) with
  | (0, _) -> []
  | (_, []) -> []
  | (n, _) when n < 0 -> failwith "length must not be negative"
  | (n, x :: xs) -> x :: (take_n xs (n - 1))
;;

let parse_entry bits = match%bitstring bits with
  | {| _ : 8 * 1 (* attributes *)
      ; _ : 8 * 3 (* chs_start *)
      ;  part_type : 8 * 1 : int
      ;  _ : 8 * 3 (* chs_last *)
      ;  lba_start : 8 * 4 : unsigned
      ;  lba_count : 8 * 4 : unsigned |} ->
    if lba_count = 0l then None
    else if part_type = 0x5 || part_type = 0xf then Some (Mbr.Secondary {start=lba_start; size=lba_count})
    else Some (Mbr.Primary {start=lba_start; size=lba_count})
  | {| _ |} -> None;;

(* *)
let parse_mbr bits = 
  match%bitstring bits with
  | {| _ : 8 * 440 : string
      ; signature : 8 * 4
      ; _  : 8 * 2 (* reserved *)
      ; fst_entry : 8 * 16 : bitstring
      ; sec_entry : 8 * 16 : bitstring
      ; trd_entry : 8 * 16 : bitstring
      ; frt_entry : 8 * 16 : bitstring
      |} -> 
    let entries = List.map Option.get (List.filter Option.is_some (List.map parse_entry [fst_entry; sec_entry; trd_entry; frt_entry])) in
    List.iter Mbr.print_entry entries;
    Some ({
        Mbr.signature = signature;
        Mbr.entries = entries
      })
  | {| _ |} -> None;;

let read_mbr file =
  let ic = open_in_bin file in
  try
    match parse_mbr (Bitstring.bitstring_of_chan_max ic 512) with
    | Some _ -> ()
    | None -> print_endline "parsed nothing!"
  with e ->
    print_endline "errored!";
    close_in_noerr ic;
    raise e;;

print_endline "sda:";
read_mbr "disk.img";
