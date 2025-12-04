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

type bm25 = { idf : (string, float) Hashtbl.t; avgdl : float }

let bm25_of_documents documents =
  let _N = Float.of_int (List.length documents) in
  let total_length =
    let fn acc { Format.length; _ } = acc + length in
    List.fold_left fn 0 documents |> Float.of_int
  in
  let df =
    let df = Hashtbl.create 0x7ff in
    let fn { Format.tokens; _ } =
      let fn (token, _) =
        match Hashtbl.find_opt df token with
        | Some freq -> Hashtbl.replace df token (freq + 1)
        | None -> Hashtbl.add df token 1
      in
      List.iter fn tokens
    in
    List.iter fn documents;
    df
  in
  let avgdl = total_length /. _N in
  let idf = Hashtbl.create 0x7ff in
  let fn token freq =
    let freq = Float.of_int freq in
    let value = Float.(log (1. +. ((_N -. freq +. 0.5) /. (freq +. 0.5)))) in
    Hashtbl.add idf token value
  in
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
        acc +. (idf *. (_n /. _m))
  in
  let sum = List.fold_left fn 0.0 query in
  if sum <= 0.0 then None else Some (document.mail, sum)

let on_input documents bm25 _ev =
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
    let* query = Brr_io.Fetch.Body.json body in
    let* query = Jsont_brr.decode_jv Format.response query |> Fut.return in
    let scores =
      let lst = List.filter_map (score bm25 query) documents in
      let tbl = Hashtbl.create 0x7ff in
      let fn (uid, score) = Hashtbl.add tbl uid score in
      List.iter fn lst;
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

let progress total =
  let current = ref 0 in
  let el = bootf "> 0/%d document(s) downloaded" total in
  let reporter n = current := !current + n in
  let display () =
    let percent = !current * 100 / total in
    let str =
      Fmt.str "> %d/%d document(s) downloaded (%d%%)" !current total percent
    in
    El.set_children el El.[ txt' str ]
  in
  (reporter, display)

let bulk len lst =
  let rec go cur acc rem lst =
    match (rem, cur, lst) with
    | _, [], [] -> acc
    | _, cur, [] -> cur :: acc
    | 0, cur, x :: r -> go [ x ] (cur :: acc) (len - 1) r
    | n, cur, x :: r -> go (x :: cur) acc (n - 1) r
  in
  go [] [] len lst

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
  let req = Brr_io.Fetch.Request.v (jstrf "/stems") in
  let* resp = Brr_io.Fetch.request req in
  let body = Brr_io.Fetch.Response.as_body resp in
  let* stems = Brr_io.Fetch.Body.json body in
  let* stems =
    let uid = Jsont.string in
    Jsont_brr.decode_jv (Jsont.list uid) stems |> Fut.return
  in
  let _ = bootf "> %d document(s)" (List.length stems) in
  let reporter, display = progress (List.length stems) in
  let fn uids =
    let method' = jstrf "POST"
    and json = Jsont_brr.encode Jsont.(list string) uids
    and headers =
      Brr_io.Fetch.Headers.of_assoc
        [ (jstrf "Content-Type", jstrf "application/json") ]
    in
    let json = Result.get_ok json in
    let body = Brr_io.Fetch.Body.of_jstr json in
    let init = Brr_io.Fetch.Request.init ~body ~headers ~method' () in
    let req = Brr_io.Fetch.Request.v ~init (jstrf "/stems") in
    let* resp = Brr_io.Fetch.request req in
    let body = Brr_io.Fetch.Response.as_body resp in
    let* stems = Brr_io.Fetch.Body.json body in
    let stems =
      Jsont_brr.decode_jv (Jsont.list (Format.stem ~uid:Jsont.string)) stems
    in
    reporter 50;
    display ();
    Fut.return stems
  in
  let stems = bulk 50 stems in
  let documents = List.map fn stems in
  let* documents = Fut.of_list documents |> Fut.map Result.ok in
  let documents = List.filter_map Result.to_option documents in
  let documents = List.flatten documents in
  let _ = bootf "> document(s) downloaded!" in
  let _ = bootf "> synthetize them" in
  let bm25 = bm25_of_documents documents in
  let _ = bootf "> document(s) synthetized!" in
  let _ =
    Ev.listen Ev.input (on_input documents bm25) (El.as_target search_bar)
  in
  El.set_class hidden true crt;
  El.set_class hidden false title;
  El.set_class hidden false search_box;
  El.set_class hidden false topics_list;
  Fut.return (Ok ())

let () =
  Fut.await (run ()) @@ function
  | Ok () -> ()
  | Error _err -> print_endline "Got an error"
