(** 
   Sources:
   - https://www.dbf2002.com/dbf-file-format.html 
   - https://formats.kaitai.io/dbf/index.html
   - https://en.wikipedia.org/wiki/.dbf
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

type date = int * int * int

type field_type =
  | Character
  | Currency
  | Numeric
  | Float
  | Date
  | DateTime
  | Double
  | Integer
  | Logical
  | Memo
  | General
  | Picture
  | Autoincrement
  | Double_level7
  | Timestamp
  | Varchar

type field = {
  field_name : string ;
  field_type : field_type ;
  field_length : int ;
  field_decimal_count : int ;
  field_system_column : bool ;
  field_column_can_store_null : bool ;
  field_binary_column : bool ;
  field_column_autoincrementing : bool ;
}

type header = {
  file_type : file_type ;
  last_update : date ;
  fields : field list ;
  nrecords : int ;
  len_header : int ;
  len_record : int ;
}

type parser_error = [
  | `Unexpected_end_of_file
  | `Unknown_file_type
  | `Unknown_field_type
]


type column =
  | String_data of string array
  | Float_data of float array

type t = {
  header : header ;
  columns : (string * column) list ;
}

val of_file : string -> (t, parser_error) result
