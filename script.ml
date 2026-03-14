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

type meta = { title : string; from : string }

let metadata : (string, meta) Hashtbl.t = Hashtbl.create 0x100

let score_entry =
  let open Jsont in
  let uid = Object.mem "uid" ~enc:fst string in
  let score = Object.mem "score" ~enc:snd number in
  let fn uid score = (uid, score) in
  Object.map fn |> uid |> score |> Object.finish

let truncate_uid uid =
  if String.length uid > 16 then String.sub uid 0 16 ^ "..." else uid

let render_results scores =
  El.set_children results_div [];
  let scores = List.sort (fun (_, a) (_, b) -> Float.compare b a) scores in
  match scores with
  | [] -> El.set_children results_div [ El.p [ El.txt' "no results." ] ]
  | _ ->
      let fn (uid, _score) =
        match Hashtbl.find_opt metadata uid with
        | Some m ->
            let div =
              El.div
                ~at:At.[ class' (jstrf "result") ]
                [
                  El.a
                    ~at:At.[ href (jstrf "/email/%s" uid) ]
                    [ El.txt' m.title ];
                  El.div
                    ~at:At.[ class' (jstrf "result-meta") ]
                    [ El.span [ El.txt' m.from ] ];
                ]
            in
            El.append_children results_div [ div ]
        | None ->
            let div =
              El.div
                ~at:At.[ class' (jstrf "result") ]
                [
                  El.div
                    ~at:At.[ class' (jstrf "result-uid") ]
                    [
                      El.a
                        ~at:At.[ href (jstrf "/email/%s" uid) ]
                        [ El.txt' (truncate_uid uid) ];
                    ];
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

let do_search query =
  abort_previous ();
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
    let* scores =
      Jsont_brr.decode_jv (Jsont.list score_entry) json |> Fut.return
    in
    render_results scores;
    Fut.return (Ok ())
  in
  Fut.await (run ()) @@ function
  | Ok () -> ()
  | Error err ->
      let name = Jv.Error.name err in
      if not (Jstr.equal name (Jstr.v "AbortError")) then
        let msg = Jv.Error.message err |> Jstr.to_string in
        El.set_children results_div
          [
            El.p
              ~at:At.[ class' (jstrf "error-msg") ]
              [ El.txt' (Fmt.str "error: %s" msg) ];
          ]

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

let set_status_ready () =
  El.set_at (jstrf "class") (Some (jstrf "dot ready")) status_dot;
  El.set_children status_text [ El.txt' "ready" ]

let set_status_error msg =
  El.set_at (jstrf "class") (Some (jstrf "dot")) status_dot;
  El.set_children status_text [ El.txt' (Fmt.str "error: %s" msg) ]

let load_metadata () =
  let open Fut.Result_syntax in
  let req = Brr_io.Fetch.Request.v (jstrf "/list") in
  let* resp = Brr_io.Fetch.request req in
  let body = Brr_io.Fetch.Response.as_body resp in
  let* emails = Brr_io.Fetch.Body.json body in
  let* emails =
    let uid = Jsont.string in
    let lang = Jsont.string in
    Jsont_brr.decode_jv (Jsont.list (Format.email ~uid ~lang)) emails
    |> Fut.return
  in
  let fn (e : (string, string) Format.t) =
    let from = Format.emile_to_utf_8_string e.from in
    Hashtbl.replace metadata e.uid { title = e.title; from }
  in
  List.iter fn emails;
  Fut.return (Ok ())

let () =
  let _ = Ev.listen Ev.input on_input (El.as_target search_bar) in
  Fut.await (load_metadata ()) @@ fun result ->
  match result with
  | Ok () -> set_status_ready ()
  | Error err ->
      let msg = Jv.Error.message err |> Jstr.to_string in
      set_status_error msg
