type ('uid, 'lang) t =
  { title : string
  ; uid : 'uid
  ; date : Ptime.t
  ; from : Emile.mailbox
  ; docs : ('uid, 'lang) doc list }
and ('uid, 'lang) doc =
  { mime : string
  ; lang : 'lang
  ; contents : 'uid }

let emile_to_utf_8_string = function
  | { Emile.name= None; _ } as m -> Emile.to_string m
  | { Emile.name= Some phrase; local; domain; } ->
      let buf = Buffer.create 0x7ff in
      let fn = function
        | `Dot -> Buffer.add_string buf ". "
        | `Word (`Atom str) ->
            Buffer.add_string buf str;
            Buffer.add_char buf ' '
        | `Word (`String str) ->
            Buffer.add_char buf '"';
            Buffer.add_string buf str;
            Buffer.add_string buf "\" "
        | `Encoded (charset, Emile.(Quoted_printable (Ok str) | Base64 (Ok str))) ->
            let str' = Rosetta.to_utf_8_string ~charset str in
            let str' = Option.value ~default:str str' in
            Buffer.add_string buf str';
            Buffer.add_char buf ' '
        | `Encoded _ -> () in
      List.iter fn phrase;
      let name = Buffer.contents buf in
      Fmt.str "%s<%s>" name (Emile.address_to_string (local, domain))

let email ~uid ~lang =
  let open Jsont in
  let title = Object.mem "title" ~enc:(fun t -> t.title) string in
  let date =
    let dec str = match Ptime.of_rfc3339 str with
      | Ok (t, _, _) -> t
      | Error _ -> Fmt.invalid_arg "Invalid RFC 3339 date" in
    map ~dec ~enc:(fun t -> Ptime.to_rfc3339 t) string in
  let date = Object.mem "date" ~enc:(fun t -> t.date) date in
  let mailbox =
    let dec str = match Emile.of_string str with
      | Ok t -> t
      | Error _ -> Fmt.invalid_arg "Invalid email address: %S" str in
    map ~dec ~enc:emile_to_utf_8_string string in
  let from = Object.mem "from" ~enc:(fun t -> t.from) mailbox in
  let doc =
    let mime = Object.mem "mime" ~enc:(fun t -> t.mime) string in
    let lang = Object.mem "lang" ~enc:(fun t -> t.lang) lang in
    let contents = Object.mem "contents" ~enc:(fun t -> t.contents) uid in
    let fn mime lang contents = { mime; lang; contents } in
    Object.map fn
    |> mime
    |> lang
    |> contents
    |> Object.finish in
  let uid = Object.mem "uid" ~enc:(fun t -> t.uid) uid in
  let docs = Object.mem "docs" ~enc:(fun t -> t.docs) (list doc) in
  let fn title uid date from docs = { title; uid; date; from; docs } in
  Object.map fn
  |> title
  |> uid
  |> date
  |> from
  |> docs
  |> Object.finish

type 'uid document =
  { length : int
  ; mail : 'uid
  ; blob : 'uid
  ; tokens : (string * int) list }

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
  Object.map fn
  |> length |> mail |> blob |> tokens
  |> Object.finish

type 'lang query =
  { lang : 'lang
  ; query : string }

let query ~lang =
  let open Jsont in
  let lang = Object.mem "lang" ~enc:(fun t -> t.lang) lang in
  let query = Object.mem "query" ~enc:(fun t -> t.query) string in
  let fn lang query = { lang; query } in
  Object.map fn
  |> lang |> query |> Object.finish

let response = Jsont.(list string)
