Let's make sure that this test doesn't depend on previous state
  $ $MERLIN server stop-server

Let's create an environment with a simple PPX and a simple executable

  $ cat > my_ppx.ml <<EOF
  > open Ppxlib
  > let rule =
  >    let ppx =
  >      let ast_context = Extension.Context.expression in
  >      let pl = Ast_pattern.(pstr nil) in
  >      let expand_fn ~ctxt =
  >        let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >        Ast_builder.Default.eint ~loc 0 in
  >      Extension.V3.declare "just_zero" ast_context pl expand_fn
  > in
  > Ppxlib.Context_free.Rule.extension ppx
  > let () = Driver.register_transformation ~rules:[ rule ] "just_zero"
  > EOF

  $ touch ppx_dep.txt

  $ cat > main.ml <<EOF
  > let () = print_int [%just_zero]
  > EOF

By default, the cache for the reader phase and the PPX phase are disabled
  $ cat > .merlin <<EOF
  > FLG -ppx '_build/default/.ppx/68ba10540cd1df30ebd46af5ef6706d9/ppx.exe -as-ppx
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  0

  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache is disabled: configuration
  --
  # . Phase cache - PPX phase
  Cache is disabled: reader cache is disabled

The cache can be enabled via the USE_PPX_CACHE directive
  $ cat > .merlin <<EOF
  > FLG -ppx '_build/default/.ppx/68ba10540cd1df30ebd46af5ef6706d9/ppx.exe -as-ppx
  > USE_PPX_CACHE
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  0
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache wasn't populated
  --
  # . Phase cache - PPX phase
  Cache wasn't populated

  $ dune exec ./main.exe 2>/dev/null
  0
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache hit

Reader and PPX caches retain distinct parser configurations. The same source is
an interface in configuration A and an implementation in configuration B; the
final A request must both recover A's parse and reuse its cache entries.

  $ cat > mode.ml <<EOF
  > val value : int
  > EOF
  $ cat > .merlin <<EOF
  > SUFFIX .foo .ml
  > USE_PPX_CACHE
  > EOF
  $ $MERLIN server errors -filename mode.ml -log-file merlin_logs < mode.ml \
  >   | jq '.value | length'
  0
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache invalidation
  --
  # . Phase cache - PPX phase
  Cache invalidation

  $ cat > .merlin <<EOF
  > SUFFIX .ml .mli
  > USE_PPX_CACHE
  > EOF
  $ $MERLIN server errors -filename mode.ml -log-file merlin_logs < mode.ml \
  >   | jq '.value | length'
  1
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache invalidation
  --
  # . Phase cache - PPX phase
  Cache invalidation

  $ cat > .merlin <<EOF
  > SUFFIX .foo .ml
  > USE_PPX_CACHE
  > EOF
  $ $MERLIN server errors -filename mode.ml -log-file merlin_logs < mode.ml \
  >   | jq '.value | length'
  0
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache hit

Compiler flags are visible to PPXs through their context. This PPX changes the
type of a binding depending on [-principal], without changing its command.

  $ cat > context_ppx.ml <<'EOF'
  > let () =
  >   Ast_mapper.register "context" (fun _ ->
  >     { Ast_mapper.default_mapper with
  >       structure = (fun _ _ ->
  >         let source =
  >           if !Clflags.principal then "let value = \"principal\""
  >           else "let value = 0"
  >         in
  >         Parse.implementation (Lexing.from_string source))
  >     })
  > EOF
  $ $OCAMLC -I +compiler-libs ocamlcommon.cma context_ppx.ml -o context_ppx.exe
  $ echo 'let value = 0' > context.ml
  $ cat > .merlin <<EOF
  > FLG -ppx $PWD/context_ppx.exe
  > USE_PPX_CACHE
  > EOF
  $ $MERLIN server type-enclosing -position 1:5 -filename context.ml < context.ml | jq -r '.value[0].type'
  int

BUG: the cached expansion ignores the changed PPX context.

  $ cat > .merlin <<EOF
  > FLG -ppx $PWD/context_ppx.exe -principal
  > USE_PPX_CACHE
  > EOF
  $ $MERLIN server type-enclosing -position 1:5 -filename context.ml < context.ml | jq -r '.value[0].type'
  int

Returning to the first configuration reuses its cached expansion.

  $ cat > .merlin <<EOF
  > FLG -ppx $PWD/context_ppx.exe
  > USE_PPX_CACHE
  > EOF
  $ $MERLIN server type-enclosing -position 1:5 -filename context.ml -log-file merlin_logs < context.ml | jq -r '.value[0].type'
  int
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache hit

Without the cache, [-principal] correctly changes the expansion.

  $ cat > .merlin <<EOF
  > FLG -ppx $PWD/context_ppx.exe -principal
  > EOF
  $ $MERLIN server type-enclosing -position 1:5 -filename context.ml < context.ml | jq -r '.value[0].type'
  string

Restore the original PPX before testing invalidation.

  $ cat > .merlin <<EOF
  > FLG -ppx '_build/default/.ppx/68ba10540cd1df30ebd46af5ef6706d9/ppx.exe -as-ppx
  > USE_PPX_CACHE
  > EOF
  $ $MERLIN server errors -filename main.ml < main.ml > /dev/null

Modifying the source code invalidates the cache
  $ cat >main.ml <<EOF
  > let _ = print_int [%just_zero]
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  0
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache invalidation
  --
  # . Phase cache - PPX phase
  Cache invalidation

Also, modifying the PPX invalidates the PPX cache

(Same PPX as before)
  $ cat > my_ppx.ml <<EOF
  > open Ppxlib
  > let rule =
  >    let ppx =
  >      let ast_context = Extension.Context.expression in
  >      let pl = Ast_pattern.(pstr nil) in
  >      let expand_fn ~ctxt =
  >        let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >        Ast_builder.Default.eint ~loc 0 in
  >      Extension.V3.declare "just_zero" ast_context pl expand_fn
  > in
  > Ppxlib.Context_free.Rule.extension ppx
  > let () = Driver.register_transformation ~rules:[ rule ] "just_zero"
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  0
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache hit

(Different PPX than before: expands to 1, instead of 0)
  $ cat > my_ppx.ml <<EOF
  > open Ppxlib
  > let rule =
  >    let ppx =
  >      let ast_context = Extension.Context.expression in
  >      let pl = Ast_pattern.(pstr nil) in
  >      let expand_fn ~ctxt =
  >        let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >        Ast_builder.Default.eint ~loc 1 in
  >      Extension.V3.declare "just_zero" ast_context pl expand_fn
  > in
  > Ppxlib.Context_free.Rule.extension ppx
  > let () = Driver.register_transformation ~rules:[ rule ] "just_zero"
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  1
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache invalidation

Also, modifying the args to the PPX invalidates the PPX cache (and the parsetree
cache since the parsetree depends on some config arguments)
  $ cat > .merlin <<EOF
  > FLG -ppx '_build/default/.ppx/68ba10540cd1df30ebd46af5ef6706d9/ppx.exe -as-ppx -no-color
  > USE_PPX_CACHE
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  1
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache invalidation

-------------

However, modifying not advertised PPX dependencies doesn't invalidate the PPX
cache and therefore can lead to wrong Merlin output, if the cache is enabled.

Let's create a PPX with PPX dependency
  $ cat > my_ppx.ml <<EOF
  > open Ppxlib
  > let rule =
  >    let ppx =
  >      let ast_context = Extension.Context.expression in
  >      let pl = Ast_pattern.(pstr nil) in
  >      let expand_fn ~ctxt =
  >        let c = open_in_bin "ppx_dep.txt" in
  >        let s = input_line c in
  >        let () = close_in c in
  >        let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >        match int_of_string_opt s with
  >        | None -> Ast_builder.Default.pexp_extension ~loc @@ Location.error_extensionf ~loc "It's sunny"
  >        | Some i -> Ast_builder.Default.eint ~loc i
  >      in
  >      Extension.V3.declare "just_zero" ast_context pl expand_fn
  > in
  > Ppxlib.Context_free.Rule.extension ppx
  > let () = Driver.register_transformation ~rules:[ rule ] "just_zero"
  > EOF

When ppx_dep.txt contains an int, there are no errors.
  $ cat > ppx_dep.txt <<EOF
  > 1
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  1
And Merlin does the right thing.
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs < main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache hit

When ppx_dep.txt is changed to contain a non-int, the AST contains an error.
  $ cat > ppx_dep.txt <<EOF
  > this isn't an int
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  [1]

Merlin just uses the cache, though, and doesn't notice the ppx_dep.txt change.
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs < main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache hit

That's why it's important that the build system advertise when the PPX has
dependencies:
  $ cat > .merlin <<EOF
  > FLG -ppx '_build/default/.ppx/68ba10540cd1df30ebd46af5ef6706d9/ppx.exe -as-ppx
  > PPX_DEPS ppx_dep.txt
  > USE_PPX_CACHE
  > EOF

Again: when ppx_dep.txt contains an int, there are no errors.
  $ cat > ppx_dep.txt <<EOF
  > 1
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  1

And Merlin does the right thing.
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs < main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache invalidation

And when we do it again, we hit cache again.
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs < main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache hit

Again: when ppx_dep.txt is changed to contain a non-int, the AST contains an error.
  $ cat > ppx_dep.txt <<EOF
  > this isn't an int
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  [1]

This time, since the PPX dependency has been advertised, Merlin does the right
thing here as well.

  $ $MERLIN server errors -filename main.ml -log-file merlin_logs < main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 18
        },
        "end": {
          "line": 1,
          "col": 30
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "It's sunny"
      }
    ],
    "notifications": []
  }
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache invalidation

Let's clean up
  $ $MERLIN server stop-server
