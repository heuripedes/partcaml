module Mbr :
sig
  type partition = { start : int32; size : int32; }
  type partition_entry = Primary of partition | Secondary of partition
  type partition_list = partition_entry list
  type table = { signature : int32; entries : partition_list; }
  val append : table -> partition_entry -> table
  val print_entry : partition_entry -> unit
  val print : table -> unit list
end
