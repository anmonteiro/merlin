open Merlin_dot_protocol
open Csexp

let with_temp_file f =
  let file = Filename.temp_file "merlin-dot-protocol" ".csexp" in
  Fun.protect ~finally:(fun () -> Sys.remove file) (fun () -> f file)

let write_sexp file sexp =
  let oc = open_out_bin file in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> Csexp.to_channel oc sexp)

let write_sexps file sexps =
  let oc = open_out_bin file in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> List.iter (Csexp.to_channel oc) sexps)

let write_string file contents =
  let oc = open_out_bin file in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let read_sexp file =
  let ic = open_in_bin file in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> Csexp.input ic)

let read_command file =
  let ic = open_in_bin file in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> Blocking.Commands.read_input ic)

let make_request path =
  with_temp_file (fun file ->
      let oc = open_out_bin file in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> Blocking.Commands.send_file_configurations oc path))

let read_configurations ~request file =
  let ic = open_in_bin file in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> Blocking.read_configurations ~request ic)

let decode request sexp =
  with_temp_file (fun file ->
      write_sexp file sexp;
      read_configurations ~request file)

let write_configurations file configurations =
  let oc = open_out_bin file in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> Blocking.write_configurations oc configurations)

let fail label = failwith ("dot protocol test failed: " ^ label)

let expect_protocol_error label = function
  | Merlin_utils.Std.Result.Error (Protocol_error _) -> ()
  | _ -> fail label

let field name value = Csexp.List [ Atom name; value ]
let atom_field name value = field name (Atom value)

let directives build_dir =
  Csexp.(
    List
      [ List [ Atom "B"; Atom build_dir ];
        List [ Atom "UNIT_NAME"; Atom "foo" ]
      ])

let config ?counterpart ?(extra = []) ~mode ~is_default ~kind build_dir =
  let counterpart =
    match counterpart with
    | None -> []
    | Some path -> [ atom_field "COUNTERPART" path ]
  in
  Csexp.List
    (List.concat
       [ [ Atom "CONFIG";
           atom_field "MODE" mode;
           atom_field "DEFAULT" (if is_default then "true" else "false");
           atom_field "KIND" kind
         ];
         counterpart;
         [ field "DIRECTIVES" (directives build_dir) ];
         extra
       ])

let response configurations =
  Csexp.List [ Atom "CONFIGURATIONS"; List configurations ]

let () =
  with_temp_file (fun file ->
      let oc = open_out_bin file in
      let _request =
        Fun.protect
          ~finally:(fun () -> close_out_noerr oc)
          (fun () -> Blocking.Commands.send_file_configurations oc "foo ml.ml")
      in
      match read_sexp file with
      | Ok Csexp.(List [ Atom "File-Configurations"; Atom "foo ml.ml" ]) -> ()
      | _ -> fail "File-Configurations command encoding");
  with_temp_file (fun file ->
      write_sexp file Csexp.(List [ Atom "File-Configurations"; Atom "foo.ml" ]);
      match read_command file with
      | File_configurations "foo.ml" -> ()
      | _ -> fail "File-Configurations command decoding");

  let request = make_request "/workspace/lib/foo.ml" in
  let ocaml =
    { mode = "ocaml";
      is_default = true;
      kind = Implementation;
      counterpart = Some "/workspace/lib/foo.mli";
      directives = directives "_build/default/.foo.objs/byte"
    }
  in
  let melange =
    { mode = "melange";
      is_default = false;
      kind = Implementation;
      counterpart = Some "/workspace/lib/foo.melange.mli";
      directives = directives "_build/default/.foo.objs/melange"
    }
  in
  let configurations = Nonempty_list.create ocaml [ melange ] in
  with_temp_file (fun file ->
      write_configurations file configurations;
      match read_configurations ~request file with
      | Merlin_utils.Std.Result.Ok configurations' ->
        if Nonempty_list.to_list configurations' <> [ ocaml; melange ] then
          fail "two-configuration round trip"
      | Merlin_utils.Std.Result.Error _ -> fail "two-configuration round trip");

  (* This literal is the response emitted by Dune's encoder. *)
  let dune_golden =
    response
      [ config ~counterpart:"/workspace/lib/foo.mli" ~mode:"ocaml"
          ~is_default:true ~kind:"implementation"
          "_build/default/.foo.objs/byte";
        config ~counterpart:"/workspace/lib/foo.melange.mli" ~mode:"melange"
          ~is_default:false ~kind:"implementation"
          "_build/default/.foo.objs/melange"
      ]
  in
  (match decode request dune_golden with
  | Merlin_utils.Std.Result.Ok configurations ->
    let configurations = Nonempty_list.to_list configurations in
    if
      List.map (fun configuration -> configuration.mode) configurations
      <> [ "ocaml"; "melange" ]
    then fail "Dune golden mode names"
  | Merlin_utils.Std.Result.Error _ -> fail "Dune golden response");

  let future_mode =
    config
      ~extra:[ atom_field "FUTURE-FIELD" "preserved-by-Dune" ]
      ~mode:"future-backend" ~is_default:false ~kind:"interface" "/build/future"
  in
  (match decode request (response [ future_mode ]) with
  | Merlin_utils.Std.Result.Ok configurations ->
    let configuration = configurations.hd in
    if
      not
        (String.equal configuration.mode "future-backend"
        && (not configuration.is_default)
        && configuration.kind = Interface)
    then fail "unknown mode or field preservation"
  | Merlin_utils.Std.Result.Error _ -> fail "sole non-default configuration");

  let config_fields =
    match
      config ~counterpart:"/workspace/lib/foo.mli" ~mode:"ocaml"
        ~is_default:true ~kind:"implementation" "/build/ocaml"
    with
    | Csexp.List (Atom "CONFIG" :: fields) -> fields
    | _ -> assert false
  in
  let without name =
    List.filter
      (function
        | Csexp.List (Atom field_name :: _) ->
          not (String.equal name field_name)
        | _ -> true)
      config_fields
  in
  let replace name replacement =
    List.map
      (function
        | Csexp.List (Atom field_name :: _) when String.equal name field_name ->
          replacement
        | field -> field)
      config_fields
  in
  List.iter
    (fun name ->
      expect_protocol_error ("missing " ^ name)
        (decode request
           (response [ Csexp.List (Atom "CONFIG" :: without name) ])))
    [ "MODE"; "DEFAULT"; "KIND"; "DIRECTIVES" ];
  List.iter
    (fun (name, duplicate) ->
      expect_protocol_error ("duplicate " ^ name)
        (decode request
           (response
              [ Csexp.List ((Atom "CONFIG" :: config_fields) @ [ duplicate ]) ])))
    [ ("MODE", atom_field "MODE" "ocaml");
      ("DEFAULT", atom_field "DEFAULT" "false");
      ("KIND", atom_field "KIND" "interface");
      ("COUNTERPART", atom_field "COUNTERPART" "/workspace/lib/other.mli");
      ("DIRECTIVES", field "DIRECTIVES" (directives "/build/other"))
    ];
  List.iter
    (fun (label, invalid) ->
      expect_protocol_error label (decode request (response [ invalid ])))
    [ ( "invalid boolean",
        Csexp.List
          (Atom "CONFIG" :: replace "DEFAULT" (atom_field "DEFAULT" "yes")) );
      ( "invalid kind",
        config ~mode:"ocaml" ~is_default:false ~kind:"module" "/build/ocaml" );
      ( "empty mode",
        config ~mode:"" ~is_default:false ~kind:"implementation" "/build/ocaml"
      );
      ( "relative counterpart",
        config ~counterpart:"foo.mli" ~mode:"ocaml" ~is_default:false
          ~kind:"implementation" "/build/ocaml" )
    ];
  expect_protocol_error "duplicate mode keys"
    (decode request
       (response
          [ config ~mode:"ocaml" ~is_default:true ~kind:"implementation"
              "/build/a";
            config ~mode:"ocaml" ~is_default:false ~kind:"interface" "/build/b"
          ]));
  expect_protocol_error "multiple defaults"
    (decode request
       (response
          [ config ~mode:"ocaml" ~is_default:true ~kind:"implementation"
              "/build/a";
            config ~mode:"melange" ~is_default:true ~kind:"implementation"
              "/build/b"
          ]));
  expect_protocol_error "empty response" (decode request (response []));
  expect_protocol_error "untagged response"
    (decode request Csexp.(List [ List [ Atom "B"; Atom "/build" ] ]));

  (match
     decode request
       Csexp.(List [ Atom "CONFIGURATIONS-ERROR"; Atom "no configuration" ])
   with
  | Merlin_utils.Std.Result.Error (Server_error "no configuration") -> ()
  | _ -> fail "tagged server error");

  let quoted_request = make_request "/workspace/with space/foo.ml" in
  let unsupported =
    Csexp.(
      List
        [ List
            [ Atom "ERROR";
              Atom
                "Bad input: (File-Configurations \"/workspace/with \
                 space/foo.ml\")"
            ]
        ])
  in
  (match decode quoted_request unsupported with
  | Merlin_utils.Std.Result.Error Unsupported -> ()
  | _ -> fail "exact unsupported response with quoted path");
  let expect_unsupported_path path printed =
    let request = make_request path in
    let response =
      Csexp.(
        List
          [ List
              [ Atom "ERROR";
                Atom ("Bad input: (File-Configurations " ^ printed ^ ")")
              ]
          ])
    in
    match decode request response with
    | Merlin_utils.Std.Result.Error Unsupported -> ()
    | _ -> fail ("exact unsupported response for path " ^ path)
  in
  expect_unsupported_path "/workspace/(semi;)/foo.ml"
    "\"/workspace/(semi;)/foo.ml\"";
  expect_unsupported_path "/workspace/%{name}/foo.ml"
    "\"/workspace/\\%{name}/foo.ml\"";
  expect_unsupported_path "/workspace/back\\slash/foo.ml"
    "/workspace/back\\slash/foo.ml";
  expect_unsupported_path "/workspace/caf\195\169/foo.ml"
    "\"/workspace/caf\195\169/foo.ml\"";
  with_temp_file (fun file ->
      write_sexps file [ unsupported; dune_golden ];
      let ic = open_in_bin file in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          match Blocking.read_configurations ~request:quoted_request ic with
          | Merlin_utils.Std.Result.Error Unsupported -> (
            match Blocking.read_configurations ~request ic with
            | Merlin_utils.Std.Result.Ok configurations
              when List.length (Nonempty_list.to_list configurations) = 2 -> ()
            | _ -> fail "response following unsupported response")
          | _ -> fail "unsupported response stream alignment"));
  let near_miss =
    Csexp.(
      List
        [ List
            [ Atom "ERROR";
              Atom
                "Bad input: (File-Configurations /workspace/with-space/foo.ml)"
            ]
        ])
  in
  expect_protocol_error "non-matching old response"
    (decode quoted_request near_miss);

  with_temp_file (fun file ->
      write_string file "not-csexp";
      match read_configurations ~request file with
      | Merlin_utils.Std.Result.Error (Protocol_error (Csexp_parse_error _)) ->
        ()
      | _ -> fail "malformed Csexp")
