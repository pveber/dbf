open Rresult
open Rresult.R.Infix
open Core_kernel

type file_type =
  | FoxBASE
  | FoxBASE_plus_Dbase_III_plus_no_memo
  | Visual_FoxPro
  | Visual_FoxPro_autoincrement_enabled
  | Visual_FoxPro_with_field_type_Varchar_or_Varbinary
  | DBASE_IV_SQL_table_files_no_memo
  | DBASE_IV_SQL_system_files_no_memo
  | FoxBASE_plus_dBASE_III_PLUS_with_memo
  | DBASE_IV_with_memo
  | DBASE_IV_SQL_table_files_with_memo
  | FoxPro_2_x_or_earlier_with_memo
  | HiPer_Six_format_with_SMT_memo_file

type date = int * int * int

type parser_error = [
  | `Unexpected_end_of_file
  | `Unknown_file_type
]

type t = {
  file_type : file_type ;
}

let file_type_of_byte = function
  | 0x2 | 0xFB -> Ok FoxBASE
  | 0x3 -> Ok FoxBASE_plus_Dbase_III_plus_no_memo
  | 0x30 -> Ok Visual_FoxPro
  | 0x31 -> Ok Visual_FoxPro_autoincrement_enabled
  | 0x32 -> Ok Visual_FoxPro_with_field_type_Varchar_or_Varbinary
  | 0x43 -> Ok DBASE_IV_SQL_table_files_no_memo
  | 0x63 -> Ok DBASE_IV_SQL_system_files_no_memo
  | 0x83 -> Ok FoxBASE_plus_dBASE_III_PLUS_with_memo
  | 0x8B -> Ok DBASE_IV_with_memo
  | 0xCB -> Ok DBASE_IV_SQL_table_files_with_memo
  | 0xF5 -> Ok FoxPro_2_x_or_earlier_with_memo
  | 0xE5 -> Ok HiPer_Six_format_with_SMT_memo_file
  | _ -> Error `Unknown_file_type

let read_byte ic =
  match In_channel.input_byte ic with
  | Some b -> Ok b
  | None -> Error `Unexpected_end_of_file

let read ic =
  read_byte ic >>= file_type_of_byte >>= fun file_type ->
  Ok { file_type }
