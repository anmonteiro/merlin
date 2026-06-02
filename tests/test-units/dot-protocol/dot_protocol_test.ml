let with_temp_file f =
  let file = Filename.temp_file "merlin-dot-protocol" ".csexp" in
  Fun.protect ~finally:(fun () -> Sys.remove file) (fun () -> f file)
;;

let write_sexp file sexp =
  let oc = open_out_bin file in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
    Csexp.to_channel oc sexp)
;;

let read_command file =
  let ic = open_in_bin file in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    Merlin_dot_protocol.Blocking.Commands.read_input ic)
;;

let read_configurations file =
  let ic = open_in_bin file in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    Merlin_dot_protocol.Blocking.read_configurations ic)
;;

let write_configurations file configurations =
  let oc = open_out_bin file in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
    Merlin_dot_protocol.Blocking.write_configurations oc configurations)
;;

let () =
  with_temp_file (fun file ->
    write_sexp file Csexp.(List [ Atom "File-Configurations"; Atom "foo.ml" ]);
    match read_command file with
    | File_configurations "foo.ml" -> ()
    | _ -> failwith "expected File_configurations command");
  with_temp_file (fun file ->
    let configurations =
      [ { Merlin_dot_protocol.id = "lib-foo"
        ; mode = Some "ocaml"
        ; is_default = true
        ; directives = [ `B "_build/default/.foo.objs/byte"; `UNIT_NAME "foo" ]
        }
      ; { Merlin_dot_protocol.id = "lib-foo-melange"
        ; mode = Some "melange"
        ; is_default = false
        ; directives = [ `B "_build/default/.foo.objs/melange"; `UNIT_NAME "foo" ]
        }
      ]
    in
    write_configurations file configurations;
    match read_configurations file with
    | Merlin_utils.Std.Result.Ok configurations' ->
      assert (configurations' = configurations)
    | Merlin_utils.Std.Result.Error _ -> failwith "failed to read configurations");
  with_temp_file (fun file ->
    write_sexp
      file
      Csexp.(
        List
          [ List [ Atom "B"; Atom "_build/default/.foo.objs/byte" ]
          ; List [ Atom "UNIT_NAME"; Atom "foo" ]
          ]);
    match read_configurations file with
    | Merlin_utils.Std.Result.Error (Merlin_dot_protocol.Unexpected_output _) -> ()
    | Merlin_utils.Std.Result.Ok _ -> failwith "expected old-style directives to be rejected"
    | Merlin_utils.Std.Result.Error _ -> failwith "expected Unexpected_output")
;;
