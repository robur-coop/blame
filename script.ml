open Brr

let jstrf fmt = Fmt.kstr Jstr.v fmt

let search_bar =
  Document.find_el_by_id G.document (Jstr.v "searchBar") |> Option.get

let results_div =
  Document.find_el_by_id G.document (Jstr.v "results") |> Option.get

let status_dot =
  Document.find_el_by_id G.document (Jstr.v "statusDot") |> Option.get

let status_text =
  Document.find_el_by_id G.document (Jstr.v "statusText") |> Option.get

module Meta = struct
  type t = Format.t

  let weight _ = 1
end

module Cache = Lru.M.Make (String) (Meta)

let cache = Cache.create 4096

let set_status_ready () =
  El.set_at (jstrf "class") (Some (jstrf "dot ready")) status_dot;
  El.set_children status_text [ El.txt' "ready" ]

let set_status_loading () =
  El.set_at (jstrf "class") (Some (jstrf "dot loading")) status_dot;
  El.set_children status_text [ El.txt' "loading..." ]

let set_status_error msg =
  El.set_at (jstrf "class") (Some (jstrf "dot")) status_dot;
  El.set_children status_text [ El.txt' (Fmt.str "error: %s" msg) ]

let render scores =
  El.set_children results_div [];
  let scores =
    List.sort (fun (_, a, _) (_, b, _) -> Float.compare b a) scores
  in
  match scores with
  | [] -> El.set_children results_div [ El.p [ El.txt' "no results." ] ]
  | _ ->
      let fn (uid, _score, m) =
        let from = Format.emile_to_utf_8_string m.Format.from in
        let div =
          El.div
            ~at:At.[ class' (jstrf "result") ]
            [
              El.a
                ~at:At.[ href (jstrf "/email/%s" uid) ]
                [ El.txt' m.Format.title ];
              El.div
                ~at:At.[ class' (jstrf "result-meta") ]
                [ El.span [ El.txt' from ] ];
            ]
        in
        El.append_children results_div [ div ]
      in
      List.iter fn scores

let debounce_timer : Jv.t option ref = ref None
let current_abort : Abort.t option ref = ref None

let clear_timer () =
  match !debounce_timer with
  | Some t ->
      ignore (Jv.call Jv.global "clearTimeout" [| t |]);
      debounce_timer := None
  | None -> ()

let abort_previous () =
  match !current_abort with
  | Some ctrl ->
      Abort.abort ctrl;
      current_abort := None
  | None -> ()

let fetch_metadata signal uids_and_scores =
  let tbl = Hashtbl.create 0x7ff in
  let fn (uid, score) = Hashtbl.add tbl uid score in
  Seq.iter fn uids_and_scores;
  let uids = Seq.map (fun (uid, _) -> uid) uids_and_scores in
  let json = Jsont_brr.encode (Format.seq Jsont.string) uids in
  let json = Result.get_ok json in
  let body = Brr_io.Fetch.Body.of_jstr json in
  let method' = jstrf "POST" in
  let headers =
    Brr_io.Fetch.Headers.of_assoc
      [ (jstrf "Content-Type", jstrf "application/json") ]
  in
  let init = Brr_io.Fetch.Request.init ~body ~headers ~method' ~signal () in
  let req = Brr_io.Fetch.Request.v ~init (jstrf "/metadata") in
  let open Fut.Result_syntax in
  let* resp = Brr_io.Fetch.request req in
  let body = Brr_io.Fetch.Response.as_body resp in
  let* json = Brr_io.Fetch.Body.json body in
  let fmt = Format.entries ~uid:Jsont.string in
  let* entries = Jsont_brr.decode_jv fmt json |> Fut.return in
  let fn (uid, m) =
    match Hashtbl.find tbl uid with
    | exception Not_found -> None
    | score ->
        Cache.add uid m cache;
        Some (uid, score, m)
  in
  let entries = Seq.filter_map fn entries in
  Fut.return (Ok entries)

let do_search query =
  abort_previous ();
  set_status_loading ();
  let ctrl = Abort.controller () in
  current_abort := Some ctrl;
  let signal = Abort.signal ctrl in
  let json =
    Jsont_brr.encode
      (Format.query ~lang:Jsont.string)
      { Format.query; lang = "english" }
  in
  let json = Result.get_ok json in
  let body = Brr_io.Fetch.Body.of_jstr json in
  let method' = jstrf "POST" in
  let headers =
    Brr_io.Fetch.Headers.of_assoc
      [ (jstrf "Content-Type", jstrf "application/json") ]
  in
  let init = Brr_io.Fetch.Request.init ~body ~headers ~method' ~signal () in
  let req = Brr_io.Fetch.Request.v ~init (jstrf "/query") in
  let run () =
    let open Fut.Result_syntax in
    let* resp = Brr_io.Fetch.request req in
    let body = Brr_io.Fetch.Response.as_body resp in
    let* json = Brr_io.Fetch.Body.json body in
    let fmt = Format.scores ~uid:Jsont.string in
    let* scores = Jsont_brr.decode_jv fmt json |> Fut.return in
    let fn (uid, score) =
      match Cache.find uid cache with
      | Some m -> Either.Left (uid, score, m)
      | None -> Either.Right (uid, score)
    in
    let r0, to_fetch = Seq.partition_map fn scores in
    let* r1 =
      if Seq.is_empty to_fetch then Fut.return (Ok Seq.empty)
      else fetch_metadata signal to_fetch
    in
    let results = Seq.append r0 r1 in
    render (List.of_seq results);
    Fut.return (Ok ())
  in
  Fut.await (run ()) @@ function
  | Ok () -> set_status_ready ()
  | Error err ->
      let name = Jv.Error.name err in
      if not (Jstr.equal name (Jstr.v "AbortError")) then begin
        let msg = Jv.Error.message err |> Jstr.to_string in
        set_status_error msg;
        El.set_children results_div
          [
            El.p
              ~at:At.[ class' (jstrf "error-msg") ]
              [ El.txt' (Fmt.str "error: %s" msg) ];
          ]
      end

let on_input _ev =
  let query =
    Jv.get (El.to_jv search_bar) "value" |> Jv.to_jstr |> Jstr.to_string
  in
  clear_timer ();
  if String.length query = 0 then begin
    abort_previous ();
    El.set_children results_div []
  end
  else
    let cb = Jv.callback ~arity:1 (fun _ -> do_search query) in
    let t = Jv.call Jv.global "setTimeout" [| cb; Jv.of_int 300 |] in
    debounce_timer := Some t

let () =
  let _ = Ev.listen Ev.input on_input (El.as_target search_bar) in
  set_status_ready ()
