let { Logger.log } = Logger.for_section "Phase cache"

module type S = sig
  type input
  type output

  val f : input -> output

  val title : string

  module Fingerprint : sig
    type t

    val make : input -> (t, string) result
    val equal : t -> t -> bool
  end
end

module With_cache (Phase : S) = struct
  type t = { output : Phase.output; cache_was_hit : bool }
  type cache = { fingerprint : Phase.Fingerprint.t; output : Phase.output }

  let capacity = 2
  let cache = ref []

  let rec find fingerprint before = function
    | [] -> None
    | ({ fingerprint = cached_fingerprint; _ } as cached) :: after ->
      if Phase.Fingerprint.equal cached_fingerprint fingerprint then
        Some (cached, List.rev_append before after)
      else find fingerprint (cached :: before) after

  let add fingerprint output entries =
    cache := { fingerprint; output } :: Std.List.take_n (capacity - 1) entries

  let apply ?(cache_disabling = None) ?(force_invalidation = false) input =
    let title = Phase.title in
    match cache_disabling with
    | Some reason ->
      log ~title "Cache is disabled: %s" reason;
      cache := [];
      let output = Phase.f input in
      { output; cache_was_hit = false }
    | None -> (
      let new_fingerprint = Phase.Fingerprint.make input in
      match new_fingerprint with
      | Ok new_fingerprint -> (
        let cached = find new_fingerprint [] !cache in
        match cached with
        | Some (({ output; _ } as entry), remaining) when not force_invalidation
          ->
          cache := entry :: remaining;
          log ~title "Cache hit";
          { output; cache_was_hit = true }
        | None | Some _ ->
          log ~title
            (match !cache with
            | [] -> "Cache wasn't populated\n"
            | _ :: _ -> "Cache invalidation");
          let entries =
            match cached with
            | None -> !cache
            | Some (_, remaining) -> remaining
          in
          let output = Phase.f input in
          add new_fingerprint output entries;
          { output; cache_was_hit = false })
      | Error err ->
        log ~title "Cache workflow is incomplete: %s" err;
        cache := [];
        let output = Phase.f input in
        { output; cache_was_hit = false })
end
