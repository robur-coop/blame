open Brr

let jstrf fmt = Fmt.kstr Jstr.v fmt
let hidden = jstrf "hidden"
let none = jstrf "none"
let title = Document.find_el_by_id G.document (Jstr.v "title") |> Option.get

let search_box =
  Document.find_el_by_id G.document (Jstr.v "searchBox") |> Option.get

let topics_list =
  Document.find_el_by_id G.document (Jstr.v "topicsList") |> Option.get

let search_bar =
  Document.find_el_by_id G.document (Jstr.v "searchBar") |> Option.get

let crt = Document.find_el_by_id G.document (jstrf "crt") |> Option.get
let boot = Document.find_el_by_id G.document (jstrf "boot") |> Option.get

let bootf fmt =
  let fn txt =
    let pre = El.pre [ El.txt' txt ] in
    El.append_children boot [ pre ];
    pre
  in
  Fmt.kstr fn fmt

let on_input _ev =
  El.set_class none false search_box;
  let query =
    Jv.get (El.to_jv search_bar) "value" |> Jv.to_jstr |> Jstr.to_string
  in
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
  let init = Brr_io.Fetch.Request.init ~body ~headers ~method' () in
  let req = Brr_io.Fetch.Request.v ~init (jstrf "/query") in
  let run () =
    let open Fut.Result_syntax in
    let* resp = Brr_io.Fetch.request req in
    let body = Brr_io.Fetch.Response.as_body resp in
    let* scores = Brr_io.Fetch.Body.json body in
    let fmt = Format.scores ~uid:Jsont.string in
    let* scores = Jsont_brr.decode_jv fmt scores |> Fut.return in
    let scores =
      let tbl = Hashtbl.create 0x7ff in
      let fn (uid, score) = Hashtbl.add tbl uid score in
      Seq.iter fn scores;
      tbl
    in
    let topics =
      Brr.El.fold_find_by_selector ~root:topics_list
        (fun el acc -> el :: acc)
        (jstrf ".topic") []
    in
    if Hashtbl.length scores <= 0 then (
      let fn el = El.set_class hidden false el in
      El.set_class none true search_box;
      List.iter fn topics;
      Fut.return (Ok ()))
    else
      let fn acc el =
        let uid = El.prop El.Prop.id el |> Jstr.to_string in
        match Hashtbl.find_opt scores uid with
        | Some score ->
            El.set_class hidden false el;
            (score, el) :: acc
        | None ->
            El.set_class hidden true el;
            acc
      in
      let visible_divs = List.fold_left fn [] topics in
      let visible_divs =
        List.sort (fun (a, _) (b, _) -> Float.compare b a) visible_divs
      in
      let fn (_, el) = El.append_children topics_list [ el ] in
      List.iter fn visible_divs;
      Fut.return (Ok ())
  in
  Fut.await (run ()) @@ function
  | Ok () -> ()
  | Error _err -> print_endline "Got an error"

let run () =
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
  let _ = bootf "> %d email(s)" (List.length emails) in
  let rec go = function
    | [] -> Fut.return (Ok ())
    | x :: r ->
        let from = Format.emile_to_utf_8_string x.Format.from in
        let div =
          El.div
            ~at:At.[ class' (jstrf "topic"); id (jstrf "%s" x.Format.uid) ]
            [
              El.a
                ~at:At.[ href (jstrf "/get/%s" x.Format.uid) ]
                [ El.txt' x.Format.title ];
              El.div
                ~at:At.[ class' (jstrf "details-wrapper") ]
                [ El.div ~at:At.[ class' (jstrf "details") ] [ El.txt' from ] ];
            ]
        in
        El.append_children topics_list [ div ];
        go r
  in
  let* () = go emails in
  let _ = Ev.listen Ev.input on_input (El.as_target search_bar) in
  El.set_class hidden true crt;
  El.set_class hidden false title;
  El.set_class hidden false search_box;
  El.set_class hidden false topics_list;
  Fut.return (Ok ())

let () =
  Fut.await (run ()) @@ function
  | Ok () -> ()
  | Error _err -> print_endline "Got an error"
