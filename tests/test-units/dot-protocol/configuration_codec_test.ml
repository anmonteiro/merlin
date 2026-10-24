open Merlin_dot_protocol
open Csexp

let fail label = failwith ("configuration codec test failed: " ^ label)

let expect_error label = function
  | Merlin_utils.Std.Result.Error (Unexpected_output _) -> ()
  | _ -> fail label

let field name value = Csexp.List [ Atom name; value ]
let atom_field name value = field name (Atom value)

let directives build_dir =
  Csexp.(
    List
      [ List [ Atom "B"; Atom build_dir ];
        List [ Atom "UNIT_NAME"; Atom "foo" ]
      ])

let config ?(extra = []) ~mode ~is_default ~kind build_dir =
  Csexp.List
    (List.concat
       [ [ Atom "CONFIG";
           atom_field "MODE" mode;
           atom_field "DEFAULT" (if is_default then "true" else "false");
           atom_field "KIND" kind
         ];
         [ field "DIRECTIVES" (directives build_dir) ];
         extra
       ])

let response configurations =
  Csexp.List [ Atom "CONFIGURATIONS"; List configurations ]

let () =
  let ocaml =
    { mode = "ocaml";
      is_default = true;
      kind = Implementation;
      directives = directives "_build/default/.foo.objs/byte"
    }
  in
  let melange =
    { mode = "melange";
      is_default = false;
      kind = Implementation;
      directives = directives "_build/default/.foo.objs/melange"
    }
  in
  let configurations = Nonempty_list.create ocaml [ melange ] in
  (match configurations_of_sexp (configurations_to_sexp configurations) with
  | Merlin_utils.Std.Result.Ok configurations' ->
    if Nonempty_list.to_list configurations' <> [ ocaml; melange ] then
      fail "two-configuration round trip"
  | Merlin_utils.Std.Result.Error _ -> fail "two-configuration round trip");

  (* This fixture follows the response format in Dune's prototype. *)
  let dune_golden =
    response
      [ config ~mode:"ocaml" ~is_default:true ~kind:"implementation"
          "_build/default/.foo.objs/byte";
        config ~mode:"melange" ~is_default:false ~kind:"implementation"
          "_build/default/.foo.objs/melange"
      ]
  in
  if configurations_to_sexp configurations <> dune_golden then
    fail "Dune golden encoding";
  (match configurations_of_sexp dune_golden with
  | Merlin_utils.Std.Result.Ok configurations ->
    if Nonempty_list.to_list configurations <> [ ocaml; melange ] then
      fail "Dune golden configurations"
  | Merlin_utils.Std.Result.Error _ -> fail "Dune golden response");

  let future_mode =
    config
      ~extra:[ atom_field "FUTURE-FIELD" "preserved-by-Dune" ]
      ~mode:"future-backend" ~is_default:false ~kind:"interface" "/build/future"
  in
  (match configurations_of_sexp (response [ future_mode ]) with
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
      config ~mode:"ocaml" ~is_default:true ~kind:"implementation"
        "/build/ocaml"
    with
    | Csexp.List (Atom "CONFIG" :: fields) -> fields
    | _ -> fail "test configuration"
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
      expect_error ("missing " ^ name)
        (configurations_of_sexp
           (response [ Csexp.List (Atom "CONFIG" :: without name) ])))
    [ "MODE"; "DEFAULT"; "KIND"; "DIRECTIVES" ];
  List.iter
    (fun (name, duplicate) ->
      expect_error ("duplicate " ^ name)
        (configurations_of_sexp
           (response
              [ Csexp.List ((Atom "CONFIG" :: config_fields) @ [ duplicate ]) ])))
    [ ("MODE", atom_field "MODE" "ocaml");
      ("DEFAULT", atom_field "DEFAULT" "false");
      ("KIND", atom_field "KIND" "interface");
      ("DIRECTIVES", field "DIRECTIVES" (directives "/build/other"))
    ];
  List.iter
    (fun (label, invalid) ->
      expect_error label (configurations_of_sexp (response [ invalid ])))
    [ ( "invalid boolean",
        Csexp.List
          (Atom "CONFIG" :: replace "DEFAULT" (atom_field "DEFAULT" "yes")) );
      ( "invalid kind",
        config ~mode:"ocaml" ~is_default:false ~kind:"module" "/build/ocaml" );
      ( "empty mode",
        config ~mode:"" ~is_default:false ~kind:"implementation" "/build/ocaml"
      )
    ];
  expect_error "duplicate mode keys"
    (configurations_of_sexp
       (response
          [ config ~mode:"ocaml" ~is_default:true ~kind:"implementation"
              "/build/a";
            config ~mode:"ocaml" ~is_default:false ~kind:"interface" "/build/b"
          ]));
  expect_error "multiple defaults"
    (configurations_of_sexp
       (response
          [ config ~mode:"ocaml" ~is_default:true ~kind:"implementation"
              "/build/a";
            config ~mode:"melange" ~is_default:true ~kind:"implementation"
              "/build/b"
          ]));
  expect_error "empty response" (configurations_of_sexp (response []));
  expect_error "untagged response"
    (configurations_of_sexp Csexp.(List [ List [ Atom "B"; Atom "/build" ] ]))
