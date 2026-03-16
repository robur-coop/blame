type t = { title : string; date : Ptime.t; from : Emile.mailbox }

let emile_to_utf_8_string = function
  | { Emile.name = None; local; domain }
  | { Emile.name = Some _; local; domain } ->
      Emile.address_to_string (local, domain)

let email =
  let open Jsont in
  let title = Object.mem "title" ~enc:(fun t -> t.title) string in
  let date =
    let dec str =
      match Ptime.of_rfc3339 str with
      | Ok (t, _, _) -> t
      | Error _ -> Fmt.invalid_arg "Invalid RFC 3339 date"
    in
    map ~dec ~enc:(fun t -> Ptime.to_rfc3339 t) string
  in
  let date = Object.mem "date" ~enc:(fun t -> t.date) date in
  let mailbox =
    let dec str =
      match Emile.of_string str with
      | Ok t -> t
      | Error _ -> Fmt.invalid_arg "Invalid email address: %S" str
    in
    map ~dec ~enc:emile_to_utf_8_string string
  in
  let from = Object.mem "from" ~enc:(fun t -> t.from) mailbox in
  let fn title date from = { title; date; from } in
  Object.map fn |> title |> date |> from |> Object.finish

type 'lang query = { lang : 'lang; query : string }

let query ~lang =
  let open Jsont in
  let lang = Object.mem "lang" ~enc:(fun t -> t.lang) lang in
  let query = Object.mem "query" ~enc:(fun t -> t.query) string in
  let fn lang query = { lang; query } in
  Object.map fn |> lang |> query |> Object.finish

let iter fn acc seq =
  let rec go acc idx seq =
    match Seq.uncons seq with
    | Some (elt, seq) ->
        let acc = fn acc idx elt in
        go acc (succ idx) seq
    | None -> acc
  in
  go acc 0 seq

let seq (elt : 'elt) =
  let enc = { Jsont.Array.enc = iter }
  and dec_empty = Seq.empty
  and dec_add _idx elt node = Seq.Cons (elt, fun () -> node)
  and dec_finish _meta _idx node = fun () -> node in
  Jsont.Array.map ~enc ~dec_empty ~dec_add ~dec_finish elt |> Jsont.Array.array

let scores ~uid =
  let entry =
    let open Jsont in
    let mail = Object.mem "uid" ~enc:fst uid in
    let score = Object.mem "score" ~enc:snd number in
    let fn mail score = (mail, score) in
    Object.map fn |> mail |> score |> Object.finish
  in
  seq entry

let entries ~uid =
  let entry =
    let open Jsont in
    let mail = Object.mem "uid" ~enc:fst uid in
    let metadata = Object.mem "metadata" ~enc:snd email in
    let fn mail metadata = (mail, metadata) in
    Object.map fn |> mail |> metadata |> Object.finish
  in
  seq entry
