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

let directives build_dir =
  Csexp.(
    List
      [ List [ Atom "B"; Atom build_dir ];
        List [ Atom "UNIT_NAME"; Atom "foo" ]
      ])

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
      write_sexps file [ unsupported; ocaml.directives ];
      let ic = open_in_bin file in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          match Blocking.read_configurations ~request:quoted_request ic with
          | Merlin_utils.Std.Result.Error Unsupported -> (
            match Blocking.read ic with
            | Merlin_utils.Std.Result.Ok directives
              when directives = configuration_directives ocaml -> ()
            | _ -> fail "legacy response following unsupported response")
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

  List.iter
    (fun (label, contents) ->
      with_temp_file (fun file ->
          write_string file contents;
          match read_configurations ~request file with
          | Merlin_utils.Std.Result.Error (Protocol_error (Csexp_parse_error _))
            -> ()
          | _ -> fail label))
    [ ("malformed Csexp", "not-csexp");
      ("end of file", "");
      ("truncated response", "(14:CONFIGURATIONS")
    ]
