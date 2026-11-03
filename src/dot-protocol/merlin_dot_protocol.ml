(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2019  Frédéric Bour  <frederic.bour(_)lakaban.net>
                         Thomas Refis  <refis.thomas(_)gmail.com>
                         Simon Castellan  <simon.castellan(_)iuwt.fr>

     Permission is hereby granted, free of charge, to any person obtaining a
     copy of this software and associated documentation files (the "Software"),
     to deal in the Software without restriction, including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
     sell copies of the Software, and to permit persons to whom the Software is
     furnished to do so, subject to the following conditions:

     The above copyright notice and this permission notice shall be included in
     all copies or substantial portions of the Software.

     The Software is provided "as is", without warranty of any kind, express or
     implied, including but not limited to the warranties of merchantability,
     fitness for a particular purpose and noninfringement. In no event shall
     the authors or copyright holders be liable for any claim, damages or other
     liability, whether in an action of contract, tort or otherwise, arising
     from, out of or in connection with the software or the use or other dealings
     in the Software.

   )* }}} *)

open Merlin_utils.Std
open Merlin_utils.Std.Result

module Directive = struct
  type include_path =
    [ `B of string
    | `S of string
    | `BH of string
    | `SH of string
    | `CMI of string
    | `CMT of string
    | `INDEX of string
    | `PPX_DEPS of string ]

  type no_processing_required =
    [ `EXT of string list
    | `FLG of string list
    | `STDLIB of string
    | `SOURCE_ROOT of string
    | `UNIT_NAME of string
    | `WRAPPING_PREFIX of string
    | `SUFFIX of string
    | `READER of string list
    | `EXCLUDE_QUERY_DIR
    | `USE_PPX_CACHE
    | `UNKNOWN_TAG of string ]

  module Processed = struct
    type acceptable_in_input = [ include_path | no_processing_required ]

    type t = [ acceptable_in_input | `ERROR_MSG of string ]
  end

  module Raw = struct
    type t =
      [ Processed.acceptable_in_input
      | `PKG of string list
      | `FINDLIB of string
      | `FINDLIB_PATH of string
      | `FINDLIB_TOOLCHAIN of string ]
  end
end

type directive = Directive.Processed.t

type source_kind = Implementation | Interface

type configuration =
  { mode : string;
    is_default : bool;
    kind : source_kind;
    counterpart : string option;
    directives : Csexp.t
  }

module Nonempty_list = struct
  type 'a t = { hd : 'a; tl : 'a list }

  let create hd tl = { hd; tl }
  let to_list { hd; tl } = hd :: tl
end

type read_error = Unexpected_output of string | Csexp_parse_error of string

type configurations_error =
  | Unsupported
  | Server_error of string
  | Protocol_error of read_error

type file_configurations_request =
  { sexp : Csexp.t; unsupported_response : Csexp.t }

module Sexp = struct
  type t = Csexp.t = Atom of string | List of t list

  let atoms_of_strings = List.map ~f:(fun s -> Atom s)

  let strings_of_atoms =
    List.filter_map ~f:(function
      | Atom s -> Some s
      | _ -> None)

  let atom_is_valid s =
    let rec loop i =
      i = String.length s
      ||
      match String.unsafe_get s i with
      | '%' -> after_percent (i + 1)
      | '"' | '(' | ')' | ';' | '\000' .. '\032' | '\127' .. '\255' -> false
      | _ -> loop (i + 1)
    and after_percent i =
      i = String.length s
      ||
      match String.unsafe_get s i with
      | '%' -> after_percent (i + 1)
      | '"' | '(' | ')' | ';' | '\000' .. '\032' | '\127' .. '\255' | '{' ->
        false
      | _ -> loop (i + 1)
    in
    (not (String.equal s "")) && loop 0

  let quoted s =
    let buffer = Buffer.create (String.length s + 2) in
    Buffer.add_char buffer '"';
    let rec loop i =
      if i < String.length s then
        let next =
          match String.unsafe_get s i with
          | ('"' | '\\') as char ->
            Buffer.add_char buffer '\\';
            Buffer.add_char buffer char;
            i + 1
          | '\n' ->
            Buffer.add_string buffer "\\n";
            i + 1
          | '\t' ->
            Buffer.add_string buffer "\\t";
            i + 1
          | '\r' ->
            Buffer.add_string buffer "\\r";
            i + 1
          | '\b' ->
            Buffer.add_string buffer "\\b";
            i + 1
          | '%' when i + 1 < String.length s && s.[i + 1] = '{' ->
            Buffer.add_string buffer "\\%";
            i + 1
          | ' ' .. '~' as char ->
            Buffer.add_char buffer char;
            i + 1
          | char ->
            let decoded = String.get_utf_8_uchar s i in
            if Uchar.utf_decode_is_valid decoded then (
              let length = Uchar.utf_decode_length decoded in
              Buffer.add_substring buffer s i length;
              i + length)
            else (
              Buffer.add_string buffer
                (Printf.sprintf "\\%03d" (Char.code char));
              i + 1)
        in
        loop next
    in
    loop 0;
    Buffer.add_char buffer '"';
    Buffer.contents buffer

  let atom_to_string s = if atom_is_valid s then s else quoted s

  let rec to_string = function
    | Atom s -> atom_to_string s
    | List l -> "(" ^ String.concat ~sep:" " (List.map ~f:to_string l) ^ ")"

  let to_directive sexp =
    match sexp with
    | List [ Atom tag; Atom value ] ->
      begin match tag with
      | "S" -> `S value
      | "B" -> `B value
      | "SH" -> `SH value
      | "BH" -> `BH value
      | "CMI" -> `CMI value
      | "CMT" -> `CMT value
      | "INDEX" -> `INDEX value
      | "STDLIB" -> `STDLIB value
      | "SOURCE_ROOT" -> `SOURCE_ROOT value
      | "UNIT_NAME" -> `UNIT_NAME value
      | "WRAPPING_PREFIX" -> `WRAPPING_PREFIX value
      | "SUFFIX" -> `SUFFIX value
      | "ERROR" -> `ERROR_MSG value
      | "PPX_DEPS" -> `PPX_DEPS value
      | "FLG" ->
        (* This means merlin asked dune 2.6 for configuration.
           But the protocole evolved, only dune 2.8 should be used *)
        `ERROR_MSG "No .merlin file found. Try building the project."
      | tag -> `UNKNOWN_TAG tag
      end
    | List [ Atom tag; List l ] ->
      let value = strings_of_atoms l in
      begin match tag with
      | "EXT" -> `EXT value
      | "FLG" -> `FLG value
      | "READER" -> `READER value
      | tag -> `UNKNOWN_TAG tag
      end
    | List [ Atom "EXCLUDE_QUERY_DIR" ] -> `EXCLUDE_QUERY_DIR
    | List [ Atom "USE_PPX_CACHE" ] -> `USE_PPX_CACHE
    | _ -> `ERROR_MSG "Unexpected output from external config reader"

  let from_directives (directives : Directive.Processed.t list) =
    let f t =
      let tag, body =
        let single s = [ Atom s ] in
        match t with
        | `B s -> ("B", single s)
        | `S s -> ("S", single s)
        | `BH s -> ("BH", single s)
        | `SH s -> ("SH", single s)
        | `CMI s -> ("CMI", single s)
        | `CMT s -> ("CMT", single s)
        | `INDEX s -> ("INDEX", single s)
        | `SOURCE_ROOT s -> ("SOURCE_ROOT", single s)
        | `UNIT_NAME s -> ("UNIT_NAME", single s)
        | `WRAPPING_PREFIX s -> ("WRAPPING_PREFIX", single s)
        | `EXT ss -> ("EXT", [ List (atoms_of_strings ss) ])
        | `FLG ss -> ("FLG", [ List (atoms_of_strings ss) ])
        | `STDLIB s -> ("STDLIB", single s)
        | `SUFFIX s -> ("SUFFIX", single s)
        | `READER ss -> ("READER", [ List (atoms_of_strings ss) ])
        | `EXCLUDE_QUERY_DIR -> ("EXCLUDE_QUERY_DIR", [])
        | `USE_PPX_CACHE -> ("USE_PPX_CACHE", [])
        | `PPX_DEPS dep -> ("PPX_DEPS", single dep)
        | `UNKNOWN_TAG tag ->
          ("ERROR", single @@ Printf.sprintf "Unknown tag in .merlin: %s" tag)
        | `ERROR_MSG s -> ("ERROR", single s)
      in
      List (Atom tag :: body)
    in
    List (List.map ~f directives)

  let string_field name value = List [ Atom name; Atom value ]
  let bool_field name value =
    string_field name (if value then "true" else "false")

  let option_string_field name = function
    | None -> []
    | Some value -> [ string_field name value ]

  let from_configuration { mode; is_default; kind; counterpart; directives } =
    let kind =
      match kind with
      | Implementation -> "implementation"
      | Interface -> "interface"
    in
    List
      (List.concat
         [ [ Atom "CONFIG";
             string_field "MODE" mode;
             bool_field "DEFAULT" is_default;
             string_field "KIND" kind
           ];
           option_string_field "COUNTERPART" counterpart;
           [ List [ Atom "DIRECTIVES"; directives ] ]
         ])

  let configuration_of_sexp sexp =
    let error message = Error (Unexpected_output message) in
    let duplicate field =
      error ("Duplicate " ^ field ^ " configuration field")
    in
    match sexp with
    | List (Atom "CONFIG" :: fields) ->
      let mode = ref None in
      let is_default = ref None in
      let kind = ref None in
      let counterpart = ref None in
      let counterpart_seen = ref false in
      let directives = ref None in
      let rec loop = function
        | [] -> (
          match (!mode, !is_default, !kind, !directives) with
          | Some mode, Some is_default, Some kind, Some directives -> (
            if String.equal mode "" then
              error "Configuration MODE must not be empty"
            else
              match !counterpart with
              | Some path when Filename.is_relative path ->
                error "Configuration COUNTERPART must be an absolute path"
              | counterpart ->
                Ok { mode; is_default; kind; counterpart; directives })
          | None, _, _, _ -> error "Missing MODE configuration field"
          | _, None, _, _ -> error "Missing DEFAULT configuration field"
          | _, _, None, _ -> error "Missing KIND configuration field"
          | _, _, _, None -> error "Missing DIRECTIVES configuration field")
        | List [ Atom "MODE"; Atom value ] :: fields -> (
          match !mode with
          | Some _ -> duplicate "MODE"
          | None ->
            mode := Some value;
            loop fields)
        | List [ Atom "DEFAULT"; Atom value ] :: fields -> (
          match !is_default with
          | Some _ -> duplicate "DEFAULT"
          | None -> (
            match value with
            | "true" ->
              is_default := Some true;
              loop fields
            | "false" ->
              is_default := Some false;
              loop fields
            | _ -> error "Configuration DEFAULT must be true or false"))
        | List [ Atom "KIND"; Atom value ] :: fields -> (
          match !kind with
          | Some _ -> duplicate "KIND"
          | None -> (
            match value with
            | "implementation" ->
              kind := Some Implementation;
              loop fields
            | "interface" ->
              kind := Some Interface;
              loop fields
            | _ ->
              error "Configuration KIND must be implementation or interface"))
        | List [ Atom "COUNTERPART"; Atom value ] :: fields ->
          if !counterpart_seen then duplicate "COUNTERPART"
          else (
            counterpart_seen := true;
            counterpart := Some value;
            loop fields)
        | List [ Atom "DIRECTIVES"; (List _ as value) ] :: fields -> (
          match !directives with
          | Some _ -> duplicate "DIRECTIVES"
          | None ->
            directives := Some value;
            loop fields)
        | List
            (Atom
               (("MODE" | "DEFAULT" | "KIND" | "COUNTERPART" | "DIRECTIVES") as
                field)
            :: _)
          :: _ -> error ("Invalid " ^ field ^ " configuration field")
        | List (Atom _ :: _) :: fields -> loop fields
        | _ -> error "Unexpected configuration field"
      in
      loop fields
    | _ -> error "Unexpected configuration from external config reader"

  let configurations_of_sexp = function
    | List [ Atom "CONFIGURATIONS"; List configurations ] ->
      let rec loop acc = function
        | [] -> (
          match List.rev acc with
          | [] -> Error (Unexpected_output "Empty configuration response")
          | configuration :: configurations ->
            let all = configuration :: configurations in
            let rec validate seen_modes seen_default = function
              | [] -> Ok (Nonempty_list.create configuration configurations)
              | { mode; is_default; _ } :: configurations ->
                if List.exists seen_modes ~f:(String.equal mode) then
                  Error
                    (Unexpected_output ("Duplicate configuration MODE: " ^ mode))
                else if is_default && seen_default then
                  Error (Unexpected_output "Multiple default configurations")
                else
                  validate (mode :: seen_modes)
                    (seen_default || is_default)
                    configurations
            in
            validate [] false all)
        | configuration :: configurations -> (
          match configuration_of_sexp configuration with
          | Ok configuration -> loop (configuration :: acc) configurations
          | Error _ as error -> error)
      in
      loop [] configurations
    | sexp ->
      let msg =
        Printf.sprintf
          "A tagged configuration response was expected, instead got: \"%s\""
          (to_string sexp)
      in
      Error (Unexpected_output msg)

  let from_configurations configurations =
    List
      [ Atom "CONFIGURATIONS";
        List
          (Nonempty_list.to_list configurations
          |> List.map ~f:from_configuration)
      ]
end

let configuration_directives { directives; _ } =
  match directives with
  | Csexp.List directives -> List.map directives ~f:Sexp.to_directive
  | Csexp.Atom _ ->
    [ `ERROR_MSG "Unexpected output from external config reader" ]

type command = File of string | File_configurations of string | Halt | Unknown

module type S = sig
  type 'a io
  type in_chan
  type out_chan

  (** [read] reads one csexp from the channel and returns the list of
      directives it represents *)
  val read :
    in_chan -> (directive list, read_error) Merlin_utils.Std.Result.t io

  val write : out_chan -> directive list -> unit io

  val read_configurations :
    request:file_configurations_request ->
    in_chan ->
    ( configuration Nonempty_list.t,
      configurations_error )
    Merlin_utils.Std.Result.t
    io

  val write_configurations :
    out_chan -> configuration Nonempty_list.t -> unit io

  module Commands : sig
    val read_input : in_chan -> command io

    val send_file : out_chan -> string -> unit io

    val send_file_configurations :
      out_chan -> string -> file_configurations_request io

    val halt : out_chan -> unit io
  end
end

module Make (IO : sig
  type 'a t

  module O : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end) (Chan : sig
  type in_chan
  type out_chan

  val read : in_chan -> (Csexp.t, string) result IO.t

  val write : out_chan -> Csexp.t -> unit IO.t
end) =
struct
  type 'a io = 'a IO.t
  type in_chan = Chan.in_chan
  type out_chan = Chan.out_chan

  module Commands = struct
    let read_input chan =
      let open Sexp in
      let open IO.O in
      let+ input = Chan.read chan in
      match input with
      | Ok (List [ Atom "File"; Atom path ]) -> File path
      | Ok (List [ Atom "File-Configurations"; Atom path ]) ->
        File_configurations path
      | Ok (Atom "Halt") -> Halt
      | Ok _ -> Unknown
      | Error _ -> Halt

    let send_file chan path =
      Chan.write chan Sexp.(List [ Atom "File"; Atom path ])

    let send_file_configurations chan path =
      let open IO.O in
      let sexp = Sexp.(List [ Atom "File-Configurations"; Atom path ]) in
      let unsupported_response =
        Sexp.(
          List [ List [ Atom "ERROR"; Atom ("Bad input: " ^ to_string sexp) ] ])
      in
      let request = { sexp; unsupported_response } in
      let+ () = Chan.write chan sexp in
      request

    let halt chan = Chan.write chan (Sexp.Atom "Halt")
  end

  let read chan =
    let open IO.O in
    let+ res = Chan.read chan in
    match res with
    | Ok (Sexp.List directives) -> Ok (List.map directives ~f:Sexp.to_directive)
    | Ok sexp ->
      let msg =
        Printf.sprintf "A list of directives was expected, instead got: \"%s\""
          (Sexp.to_string sexp)
      in
      Error (Unexpected_output msg)
    | Error msg -> Error (Csexp_parse_error msg)

  let write out_chan (directives : directive list) =
    directives |> Sexp.from_directives |> Chan.write out_chan

  let read_configurations ~request chan =
    let open IO.O in
    let+ res = Chan.read chan in
    match res with
    | Ok sexp when Stdlib.( = ) sexp request.unsupported_response ->
      Error Unsupported
    | Ok (Sexp.List [ Atom "CONFIGURATIONS-ERROR"; Atom message ]) ->
      Error (Server_error message)
    | Ok sexp -> (
      match Sexp.configurations_of_sexp sexp with
      | Ok configurations -> Ok configurations
      | Error error -> Error (Protocol_error error))
    | Error msg -> Error (Protocol_error (Csexp_parse_error msg))

  let write_configurations out_chan configurations =
    configurations |> Sexp.from_configurations |> Chan.write out_chan
end

module Blocking =
  Make
    (struct
      type 'a t = 'a
      module O = struct
        let ( let+ ) x f = f x
      end
    end)
    (struct
      type in_chan = in_channel
      type out_chan = out_channel
      let read = Csexp.input
      let write = Csexp.to_channel
    end)
