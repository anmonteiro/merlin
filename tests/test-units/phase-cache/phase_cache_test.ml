let calls = ref 0

module Cache = Merlin_kernel.Phase_cache.With_cache (struct
  type input = string
  type output = string * int

  let title = "test"
  let f input =
    incr calls;
    (input, !calls)

  module Fingerprint = struct
    type t = string

    let make input =
      if String.equal input "uncacheable" then Error "no fingerprint"
      else Ok input

    let equal = String.equal
  end
end)

let check label hit expected { Cache.output; cache_was_hit } =
  if cache_was_hit <> hit || output <> expected then failwith label

let () =
  check "first A" false ("A", 1) (Cache.apply "A");
  check "first B" false ("B", 2) (Cache.apply "B");
  check "reuse A" true ("A", 1) (Cache.apply "A");
  check "insert C" false ("C", 3) (Cache.apply "C");
  check "recent A survives" true ("A", 1) (Cache.apply "A");
  check "least recent B was evicted" false ("B", 4) (Cache.apply "B");

  check "force B" false ("B", 5) (Cache.apply ~force_invalidation:true "B");
  check "forcing B retains A" true ("A", 1) (Cache.apply "A");
  check "B has the new output" true ("B", 5) (Cache.apply "B");
  check "force absent C" false ("C", 6)
    (Cache.apply ~force_invalidation:true "C");
  check "recent B survives forced insertion" true ("B", 5) (Cache.apply "B");
  check "forced insertion evicted A" false ("A", 7) (Cache.apply "A");

  check "disable cache" false ("A", 8)
    (Cache.apply ~cache_disabling:(Some "test") "A");
  check "disabling cleared B" false ("B", 9) (Cache.apply "B");
  check "disabled output was not cached" false ("A", 10) (Cache.apply "A");

  check "fingerprint error" false ("uncacheable", 11)
    (Cache.apply "uncacheable");
  check "fingerprint error cleared B" false ("B", 12) (Cache.apply "B");
  check "fingerprint error cleared A" false ("A", 13) (Cache.apply "A")
