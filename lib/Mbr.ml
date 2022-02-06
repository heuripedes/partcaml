module Mbr =  struct
  type partition =
    { start: int32
    ; size: int32
    };;

  type partition_entry = 
    | Primary of partition
    | Secondary of partition
  ;;

  type partition_list = partition_entry list;;

  type table = { signature: int32; entries: partition_list }

  (* appends p to the table if allowed by the specification *)
  let append tbl p =
    let ps = tbl.entries in
    let (pri, sec) = List.partition (function | Primary _ -> true | Secondary _ -> false) ps in
    match p with
    | Primary _ when (List.length pri) > 3 -> failwith "too many primary partitions"
    | Secondary _ when (List.length sec) > 0 -> failwith "too many secondary partitions"
    | _ when (List.length ps) > 3 -> failwith "too many partitions"
    | _ -> { tbl with entries = p :: ps }
  ;;

  let part_to_string p = Printf.sprintf "{start: %lu size: %lu}" p.start p.size;;

  let print_entry = function
    | Primary pri -> print_endline ("primary" ^ (part_to_string pri))
    | Secondary sec ->  print_endline ("secondary" ^ (part_to_string sec));
  ;;

  let print {entries; _} = 
    List.map print_entry entries
  ;;
end;;
