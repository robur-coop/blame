type ('uid, 'lang) t = {
  title : string;
  uid : 'uid;
  date : Ptime.t;
  from : Emile.mailbox;
  docs : ('uid, 'lang) doc list;
}

and ('uid, 'lang) doc = { mime : string; lang : 'lang; contents : 'uid }

let emile_to_utf_8_string = function
  | { Emile.name = None; local; domain }
  | { Emile.name = Some _; local; domain } ->
      Emile.address_to_string (local, domain)

let email ~uid ~lang =
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
  let doc =
    let mime = Object.mem "mime" ~enc:(fun t -> t.mime) string in
    let lang = Object.mem "lang" ~enc:(fun t -> t.lang) lang in
    let contents = Object.mem "contents" ~enc:(fun t -> t.contents) uid in
    let fn mime lang contents = { mime; lang; contents } in
    Object.map fn |> mime |> lang |> contents |> Object.finish
  in
  let uid = Object.mem "uid" ~enc:(fun t -> t.uid) uid in
  let docs = Object.mem "docs" ~enc:(fun t -> t.docs) (list doc) in
  let fn title uid date from docs = { title; uid; date; from; docs } in
  Object.map fn |> title |> uid |> date |> from |> docs |> Object.finish

type 'uid document = {
  length : int;
  mail : 'uid;
  blob : 'uid;
  tokens : (string * int) list;
}

let token =
  let open Jsont in
  let stem = Object.mem "stem" ~enc:(fun (a, _) -> a) string in
  let count = Object.mem "count" ~enc:(fun (_, b) -> b) int in
  let fn stem count = (stem, count) in
  Object.map fn |> stem |> count |> Object.finish

let stem ~uid =
  let open Jsont in
  let length = Object.mem "length" ~enc:(fun t -> t.length) int in
  let mail = Object.mem "mail" ~enc:(fun t -> t.mail) uid in
  let blob = Object.mem "blob" ~enc:(fun t -> t.blob) uid in
  let tokens = Object.mem "tokens" ~enc:(fun t -> t.tokens) (list token) in
  let fn length mail blob tokens = { length; mail; blob; tokens } in
  Object.map fn |> length |> mail |> blob |> tokens |> Object.finish

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

let seq elt =
  Jsont.Array.map ~enc:{ Jsont.Array.enc = iter } elt |> Jsont.Array.array

let scores ~uid =
  let entry =
    let open Jsont in
    let document = Object.mem "uid" ~enc:fst uid in
    let score = Object.mem "score" ~enc:snd number in
    let fn document score = (document, score) in
    Object.map fn |> document |> score |> Object.finish
  in
  seq entry
