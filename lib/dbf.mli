(** 
   https://www.dbf2002.com/dbf-file-format.html 
   https://formats.kaitai.io/dbf/index.html
*)

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

type parser_error = [
  | `Unexpected_end_of_file
  | `Unknown_file_type
]

type t

val read : in_channel -> (t, parser_error) result

val of_file : string -> int32 * string list




(** Low-level access function *)
