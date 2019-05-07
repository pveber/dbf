open Rresult.R.Infix
open Core_kernel


let string_of_cstring s =
  match String.index s '\x00' with
  | Some len -> String.sub s ~pos:0 ~len
  | None -> s

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

module Header1 = struct
  [%%cstruct
    type t = {
      file_type : uint8_t ;
      yy : uint8_t ;
      mm : uint8_t ;
      dd : uint8_t ;
      nrecords : uint32_t ;
      len_header : uint16_t ;
      len_record : uint16_t ;
    } [@@little_endian] ]
end

module Header2 = struct
  [%%cstruct
    type t = {
      reserved1 : uint8_t [@len 16] ;
      table_flags : uint8_t ;
      code_page_mark : uint8_t ;
      reserved2 : uint16_t ;
    } [@@little_endian] ]
end

module Field = struct
  [%%cstruct
    type t = {
      name : uint8_t [@len 11] ;
      datatype : uint8_t ;
      data_address : uint32_t ;
      length : uint8_t ;
      decimal_count : uint8_t ;
      reserved1 : uint16_t ;
      work_area_id : uint8_t ;
      reserved2 : uint16_t ;
      set_fields_flag : uint8_t ;
      reserved3 : uint8_t [@len 8] ;
    } [@@little_endian] ]
end

type t = {
  file_type : file_type ;
  last_update : date ;
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
  read_byte ic >>= fun yy ->
  read_byte ic >>= fun mm ->
  read_byte ic >>= fun dd ->
  Ok {
    file_type ;
    last_update = (yy, mm, dd) ;
  }

let read_fields t =
  let rec loop acc pos =
    match Cstruct.get_uint8 t pos with
    | 0x0D -> List.rev acc
    | _ ->
      let field = Cstruct.sub t pos Field.sizeof_t in
      let name =
        Field.get_t_name field
        |> Cstruct.to_string
        |> string_of_cstring
      in
      print_endline name ;
      loop (name :: acc) (pos + Field.sizeof_t)
  in
  loop [] (Header1.sizeof_t + Header2.sizeof_t)

let of_cstruct t =
  let header1 = Cstruct.sub t 0 Header1.sizeof_t in
  let fields = read_fields t in
  Header1.get_t_nrecords header1, fields

let of_file fn =
  let fd = Unix.openfile fn [O_RDONLY] 0 in
  let t = Unix_cstruct.of_fd fd in
  let res = of_cstruct t in
  Unix.close fd ;
  res
