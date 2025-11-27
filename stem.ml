type 'uid document =
  { length : int
  ; uid : 'uid
  ; tokens : (string * int) list }

type 'uid t =
  { idf : float
  ; avgdl : float
  ; k1 : float
  ; b : float
  ; docs : 'uid document list }

let sha1 =
  let dec = Digestif.SHA1.of_hex
  and enc = Digestif.SHA1.to_hex in
  Jsont.map ~doc:"hash" ~dec ~enc Jsont.string

let document =
  let open Jsont in
  let length = Object.mem "length" int in
  let uid = Object.mem "uid" sha1 in
  let token =
    let stem = Object.mem "stem" string in
    let count = Object.mem "count" int in
    let fn stem count = (stem, count) in
    Object.map fn
    |> stem |> count |> Object.finish in
  let tokens = Object.mem "tokens" (list token) in
  let fn length uid tokens = { length; uid; tokens } in
  Object.map fn
  |> length |> uid |> tokens
  |> Object.finish

let bm25 =
  let open Jsont in
  let idf = Object.mem "idf" float_as_hex_string in
  let avgdl = Object.mem "avgdl" float_as_hex_string in
  let k1 = Object.mem "k1" float_as_hex_string in
  let b = Object.mem "b" float_as_hex_string in
  let docs = Object.mem "docs" (list document) in
  let fn idf avgdl k1 b docs =
    { idf; avgdl; k1; b; docs } in
  Object.map fn
  |> idf |> avgdl |> k1 |> b |> docs
  |> Object.finish
