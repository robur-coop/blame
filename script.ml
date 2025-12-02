[@@@warning "-32"]

open Brr

let jstrf fmt = Fmt.kstr Jstr.v fmt

let loader = Document.find_el_by_id G.document (Jstr.v "loader") |> Option.get
let title = Document.find_el_by_id G.document (Jstr.v "title") |> Option.get
let search_box = Document.find_el_by_id G.document (Jstr.v "searchBox") |> Option.get
let topics_list = Document.find_el_by_id G.document (Jstr.v "topicsList") |> Option.get
let search_bar = Document.find_el_by_id G.document (Jstr.v "searchBar") |> Option.get

type bm25 =
  { idf : (string, float) Hashtbl.t
  ; avgdl : float }

let bm25_of_documents documents =
  let _N = Float.of_int (List.length documents) in
  let total_length =
    let fn acc { Format.length; _ } = acc + length in
    List.fold_left fn 0 documents |> Float.of_int in
  let df =
    let df = Hashtbl.create 0x7ff in
    let fn { Format.tokens; _ } =
      let fn (token, _) =
        match Hashtbl.find_opt df token with
        | Some freq -> Hashtbl.replace df token (freq + 1)
        | None -> Hashtbl.add df token 1 in
      List.iter fn tokens in
    List.iter fn documents;
    df in
  let avgdl = total_length /. _N in
  let idf = Hashtbl.create 0x7ff in
  let fn token freq =
    let freq = Float.of_int freq in
    let value = Float.(log (1. +. ((_N -. freq +. 0.5) /. (freq +. 0.5)))) in
    Hashtbl.add idf token value in
  Hashtbl.iter fn df;
  { idf; avgdl }

let score bm25 query document =
  let fn acc token =
    match List.assoc_opt token document.Format.tokens with
    | None -> acc
    | Some freq ->
        let freq = Float.of_int freq in
        let idf = Hashtbl.find bm25.idf token in
        let _D = Float.of_int document.length in
        let _n = freq *. (1.5 +. 1.) in
        let _m = freq +. (1.5 *. (1. -. 0.75 +. (0.75 *. _D /. bm25.avgdl))) in
        acc +. (idf *. (_n /. _m)) in
  let sum = List.fold_left fn 0.0 query in
  if sum <= 0.0 then None
  else Some (document.mail, sum)

let on_input documents bm25 _ev =
  let query = Jv.get (El.to_jv search_bar) "value" |> Jv.to_jstr |> Jstr.to_string in
  let json = Jsont_brr.encode (Format.query ~lang:Jsont.string) { Format.query; lang= "english" } in
  let json = Result.get_ok json in
  let body = Brr_io.Fetch.Body.of_jstr json in
  let headers = Brr_io.Fetch.Headers.of_assoc
    [jstrf "Content-Type", jstrf "application/json"] in
  let init = Brr_io.Fetch.Request.init ~body ~headers ~method':(jstrf "POST") () in
  let req = Brr_io.Fetch.Request.v ~init (jstrf "/query") in
  let run () =
    let open Fut.Result_syntax in
    let* resp = Brr_io.Fetch.request req in
    let body = Brr_io.Fetch.Response.as_body resp in
    let* query = Brr_io.Fetch.Body.json body in
    let* query = Jsont_brr.decode_jv Format.response query |> Fut.return in
    let _scores = List.filter_map (score bm25 query) documents in
    Fut.return (Ok ()) in
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
    Jsont_brr.decode_jv (Jsont.list (Format.email ~uid ~lang)) emails |> Fut.return in
  let rec go = function
    | [] -> Fut.return (Ok ())
    | x :: r ->
        let from = Format.emile_to_utf_8_string x.Format.from in
        let div = El.div ~at:At.[class' (jstrf "topic")]
          [ El.txt' x.Format.title
          ; El.div ~at:At.[class' (jstrf "details-wrapper")]
            [ El.div ~at:At.[class' (jstrf "details")]
              [ El.txt' from ] ] ] in
        El.append_children topics_list [ div ];
        go r in
  let* () = go emails in
  let req = Brr_io.Fetch.Request.v (jstrf "/stems") in
  let* resp = Brr_io.Fetch.request req in
  let body = Brr_io.Fetch.Response.as_body resp in
  let* stems = Brr_io.Fetch.Body.json body in
  let* stems =
    let uid = Jsont.string in
    Jsont_brr.decode_jv (Jsont.list uid) stems |> Fut.return in
  let rec go acc = function
    | [] -> Fut.return (Ok (List.rev acc))
    | x :: r ->
        let req = Brr_io.Fetch.Request.v (jstrf "/stem/%s" x) in
        let* resp = Brr_io.Fetch.request req in
        let body = Brr_io.Fetch.Response.as_body resp in
        let* stem = Brr_io.Fetch.Body.json body in
        let* stem = Jsont_brr.decode_jv (Format.stem ~uid:Jsont.string) stem |> Fut.return in
        go (stem :: acc) r in
  let* documents = go [] stems in
  let bm25 = bm25_of_documents documents in
  let _ = Ev.listen Ev.input (on_input documents bm25) (El.as_target search_bar) in
  let hidden = jstrf "hidden" in
  El.set_class hidden true loader;
  El.set_class hidden false title;
  El.set_class hidden false search_box;
  El.set_class hidden false topics_list;
  Fut.return (Ok ())

let () = Fut.await (run ()) @@ function
  | Ok () -> ()
  | Error _err -> print_endline "Got an error"
