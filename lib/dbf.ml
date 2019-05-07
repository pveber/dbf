open Rresult.R.Infix
open Core_kernel


let string_of_cstring s =
  match String.index s '\x00' with
  | Some len -> String.sub s ~pos:0 ~len
  | None -> s

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
      datatype : char ;
      reserved0 : uint32_t ;
      length : uint8_t ;
      decimal_count : uint8_t ;
      reserved1 : uint16_t ;
      work_area_id : uint8_t ;
      reserved2 : uint16_t ;
      field_flags : uint8_t ;
      reserved3 : uint8_t [@len 8] ;
    } [@@little_endian] ]
end

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

let field_type_of_char = function
  | 'C' -> Ok Character
  | 'Y' -> Ok Currency
  | 'N' -> Ok Numeric
  | 'F' -> Ok Float
  | 'D' -> Ok Date
  | 'T' -> Ok DateTime
  | 'B' -> Ok Double
  | 'I' -> Ok Integer
  | 'L' -> Ok Logical
  | 'M' -> Ok Memo
  | 'G' -> Ok General
  | 'P' -> Ok Picture
  | '+' -> Ok Autoincrement
  | 'O' -> Ok Double_level7
  | '@' -> Ok Timestamp
  | 'V' -> Ok Varchar
  | _ -> Error `Unknown_field_type

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

let bit_test x mask =
  x land mask = mask

let field_of_cstruct x =
  let field_name =
    Field.get_t_name x
    |> Cstruct.to_string
    |> string_of_cstring
  in
  field_type_of_char (Field.get_t_datatype x) >>= fun field_type ->
  let flag = Field.get_t_field_flags x in
  Ok {
    field_name ;
    field_type ;
    field_length = Field.get_t_length x ;
    field_decimal_count = Field.get_t_decimal_count x ;
    field_system_column = bit_test flag 0x01 ;
    field_column_can_store_null = bit_test flag 0x02 ;
    field_binary_column = bit_test flag 0x04 ;
    field_column_autoincrementing = bit_test flag 0x0C ;
  }

let fields_of_cstruct t =
  let rec loop acc pos =
    match Cstruct.get_uint8 t pos with
    | 0x0D -> Ok (List.rev acc)
    | _ ->
      let field = Cstruct.sub t pos Field.sizeof_t in
      match field_of_cstruct field with
      | Ok f ->
        loop (f :: acc) (pos + Field.sizeof_t)
      | Error e -> Error e
  in
  loop [] (Header1.sizeof_t + Header2.sizeof_t)

type parser_error = [
  | `Unexpected_end_of_file
  | `Unknown_file_type
  | `Unknown_field_type
]

type header = {
  file_type : file_type ;
  last_update : date ;
  fields : field list ;
  nrecords : int ;
  len_header : int ;
  len_record : int ;
}

let header_of_cstruct x =
  file_type_of_byte (Header1.get_t_file_type x) >>= fun file_type ->
  fields_of_cstruct x >>= fun fields ->
  Ok { file_type ;
       last_update = Header1.(get_t_yy x, get_t_mm x, get_t_dd x) ;
       fields ;
       nrecords = Int32.to_int_exn (Header1.get_t_nrecords x) ;
       len_header = Header1.get_t_len_header x ;
       len_record = Header1.get_t_len_record x }


type column =
  | String_data of string array
  | Float_data of float array

type column_setter =
    Col : {
      elts : 'a array ;
      of_cstruct : Cstruct.t -> 'a ;
      to_column : 'a array -> column ;
      
    } -> column_setter

let column_setter header field_pos { field_type ; field_length ; _ } =
  match field_type with
  | Character ->
    Col { elts = Array.create ~len:header.nrecords "" ;
          of_cstruct = (fun x ->
              Cstruct.sub x field_pos field_length
              |> Cstruct.to_string
              |> Caml.String.trim
            ) ;
          to_column = fun elts -> String_data elts }
  | Numeric ->
    Col {
      elts = Array.create ~len:header.nrecords 0. ;
      of_cstruct = (fun x ->
          Cstruct.sub x field_pos field_length
          |> Cstruct.to_string
          |> Caml.String.trim
          |> Float.of_string
        ) ;
      to_column = fun elts -> Float_data elts ;
    } ;
  | _ -> failwith "Dbf: unsupported field type"

let column_set (Col cs) i x =
  cs.elts.(i) <- cs.of_cstruct x

type t = {
  header : header ;
  columns : (string * column) list ;
}

let read_columns x header =
  let column_setters =
    List.fold header.fields ~init:(0, []) ~f:(fun (pos, acc) field ->
        pos + field.field_length,
        column_setter header pos field :: acc
      )
    |> snd
    |> List.rev
  in
  let rec loop i =
    if i < header.nrecords then (
      let record = Cstruct.sub x (header.len_header + 1 + header.len_record * i) (header.len_record - 1) in
      (* + 1 is for the delete flag *)
      List.iter column_setters ~f:(fun cs -> column_set cs i record) ;
      loop (succ i)
    )
  in
  loop 0 ;
  List.map column_setters ~f:(fun (Col cs) -> cs.to_column cs.elts)

let of_file fn =
  let fd = Unix.openfile fn [O_RDONLY] 0 in
  let t = Unix_cstruct.of_fd fd in
  header_of_cstruct t >>= fun header ->
  let colum_names = List.map header.fields ~f:(fun f -> f.field_name) in
  let columns = read_columns t header in
  Unix.close fd ;
  Ok { header ; columns = List.zip_exn colum_names columns }
