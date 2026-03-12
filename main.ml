[@@@warning "-32"]
[@@@warning "-26"]

module RNG = Mirage_crypto_rng.Fortuna

let () = Printexc.record_backtrace true
let ( let@ ) finally fn = Fun.protect ~finally fn

let uid =
  let open Vifu.Uri in
  let dec str =
    let hash = Ohex.decode str in
    if String.length hash = Digestif.SHA1.digest_size then
      Carton.Uid.unsafe_of_string hash
    else Fmt.invalid_arg "Invalid UID"
  in
  let enc (uid : Carton.Uid.t) = Ohex.encode (uid :> string) in
  conv dec enc (string `Path)

let language_of_string =
  let fn (lang : Snowball.Language.t) = ((lang :> string), lang) in
  let lst = List.map fn Snowball.languages in
  fun str -> List.assoc str lst

let juid =
  let open Jsont in
  let enc (uid : Carton.Uid.t) = Ohex.encode (uid :> string) in
  let dec str = Carton.Uid.unsafe_of_string (Ohex.decode str) in
  map ~enc ~dec string

let jlang =
  let open Jsont in
  let enc (lang : Snowball.Language.t) = (lang :> string) in
  let dec = language_of_string in
  map ~enc ~dec string

let list (entries, hash) req _server () =
  let open Vifu.Response.Syntax in
  let hdrs = Vifu.Request.headers req in
  let if_none_match =
    match Vifu.Headers.get hdrs "if-none-match" with
    | Some hash' -> String.equal hash' hash
    | None -> false
  in
  if if_none_match then
    let* () = Vifu.Response.empty in
    Vifu.Response.respond `Not_modified
  else
    let* () = Vifu.Response.add ~field:"Etag" hash in
    let* () =
      Vifu.Response.with_json ~compression:`DEFLATE req
        (Jsont.list (Format.email ~uid:juid ~lang:jlang))
        entries
    in
    Vifu.Response.respond `OK

let stems (documents, hash) req _server _ =
  let open Vifu.Response.Syntax in
  let hdrs = Vifu.Request.headers req in
  let if_none_match =
    match Vifu.Headers.get hdrs "if-none-match" with
    | Some hash' -> String.equal hash' hash
    | None -> false
  in
  if if_none_match then
    let* () = Vifu.Response.empty in
    Vifu.Response.respond `Not_modified
  else
    let* () = Vifu.Response.add ~field:"Etag" hash in
    let* () =
      Vifu.Response.with_json ~compression:`DEFLATE req (Jsont.list juid)
        documents
    in
    Vifu.Response.respond `OK

let stem_of_uid pack uid =
  let size = Carton.size_of_uid pack ~uid Carton.Size.zero in
  let blob = Carton.Blob.make ~size in
  let value = Carton.of_uid pack blob ~uid in
  match Carton.Value.kind value with
  | `A | `B | `D -> Fmt.invalid_arg "Invalid stem object"
  | `C ->
      let str = Carton.Value.string value in
      let stem = Stem.of_string str in
      let mail, blob, length, tbl = Result.get_ok stem in
      let tokens = List.of_seq (Hashtbl.to_seq tbl) in
      let mail = Carton.Uid.unsafe_of_string mail
      and blob = Carton.Uid.unsafe_of_string blob in
      { Format.mail; blob; length; tokens }

let pstem pack req _server _ =
  let open Vifu.Response.Syntax in
  try
    match Vifu.Request.of_json req with
    | Ok uids ->
        let pack = Carton.copy pack in
        let ts = List.map (stem_of_uid pack) uids in
        let* () =
          Vifu.Response.with_json req (Jsont.list (Format.stem ~uid:juid)) ts
        in
        Vifu.Response.respond `OK
    | Error _ ->
        let* () = Vifu.Response.with_text req "Invalid JSON object!\n" in
        Vifu.Response.respond `Bad_request
  with exn ->
    let str = Fmt.str "Got an exception: %s" (Printexc.to_string exn) in
    let* () = Vifu.Response.with_text req str in
    Vifu.Response.respond `Not_found

let stem pack req uid _server () =
  let open Vifu.Response.Syntax in
  try
    let pack = Carton.copy pack in
    let t = stem_of_uid pack uid in
    let* () = Vifu.Response.with_json req (Format.stem ~uid:juid) t in
    Vifu.Response.respond `OK
  with exn ->
    let str = Fmt.str "Got an exception: %s" (Printexc.to_string exn) in
    let* () = Vifu.Response.with_text req str in
    Vifu.Response.respond `Not_found

let show pack req uid _server _ =
  let open Vifu.Response.Syntax in
  try
    let size = Carton.size_of_uid pack ~uid Carton.Size.zero in
    let blob = Carton.Blob.make ~size in
    let value = Carton.of_uid pack blob ~uid in
    match Carton.Value.kind value with
    | `B | `C | `D ->
        let* () = Vifu.Response.with_text req "Invalid object (bad type)\n" in
        Vifu.Response.respond `Not_found
    | `A -> (
        let str = Carton.Value.string value in
        match Email.of_string str with
        | Error _ ->
            let* () = Vifu.Response.with_text req "Invalid skeleton\n" in
            Vifu.Response.respond `Not_found
        | Ok (t, _) ->
            let load uid =
              let uid = Carton.Uid.unsafe_of_string uid in
              let size = Carton.size_of_uid pack ~uid Carton.Size.zero in
              let blob = Carton.Blob.make ~size in
              let value = Carton.of_uid pack blob ~uid in
              let len = Carton.Value.length value in
              let bstr = Carton.Value.bigstring value in
              Bstr.sub bstr ~off:0 ~len
            in
            let seq = Email.to_seq ~load t in
            let fn = function
              | `String str -> str
              | `Value bstr -> Bstr.to_string bstr
            in
            let seq = Seq.map fn seq in
            let from = Flux.Source.seq seq in
            let* () =
              Vifu.Response.add ~field:"content-type"
                "text/plain; charset=utf-8"
            in
            let* () = Vifu.Response.with_source req from in
            Vifu.Response.respond `OK)
  with _ ->
    let* () = Vifu.Response.empty in
    Vifu.Response.respond `Not_found

let from_documents ~mime contents =
  let hash =
    let rec go ctx idx =
      if idx >= Array.length contents then Digestif.SHA1.(to_hex (get ctx))
      else go (Digestif.SHA1.feed_string ctx contents.(idx)) (succ idx)
    in
    go Digestif.SHA1.empty 0
  in
  fun req _server _ ->
    let open Vifu.Response.Syntax in
    let* () = Vifu.Response.add ~field:"content-type" mime in
    let hdrs = Vifu.Request.headers req in
    let if_none_match =
      match Vifu.Headers.get hdrs "if-none-match" with
      | Some hash' -> String.equal hash' hash
      | None -> false
    in
    if if_none_match then
      let* () = Vifu.Response.empty in
      Vifu.Response.respond `Not_modified
    else
      let from = Flux.Source.array contents in
      let* () = Vifu.Response.add ~field:"Etag" hash in
      let* () = Vifu.Response.with_source req ~compression:`DEFLATE from in
      Vifu.Response.respond `OK

let script = from_documents ~mime:"application/javascript" Documents.script
let style = from_documents ~mime:"text/css" Documents.style
let index = from_documents ~mime:"text/html" Documents.index

let none_if_stop lang =
  match List.assoc_opt lang Stopwords.words with
  | Some stops -> fun stem -> if List.mem stem stops then None else Some stem
  | None -> Option.some

let kv ~name =
  let fn blk () = Mpart.make blk in
  Mkernel.(map fn [ block name ])

type bm25 = { idf : Mpart.t; avgdl : float }

let score bm25 query (tokens, length, mail) =
  let fn acc token =
    match Hashtbl.find_opt tokens (token :> string) with
    | None -> acc
    | Some freq ->
        let freq = Float.of_int freq in
        let idf =
          Mpart.reader bm25.idf (fun reader ->
              Mpart.lookup reader (Rowex.key token))
        in
        let idf = Int64.float_of_bits idf in
        let _D = Float.of_int length in
        let _n = freq *. (1.5 +. 1.) in
        let _m = freq +. (1.5 *. (1. -. 0.75 +. (0.75 *. _D /. bm25.avgdl))) in
        acc +. (idf *. (_n /. _m))
  in
  let sum = List.fold_left fn 0.0 query in
  if sum <= 0.0 then None else Some (mail, sum)

let score pool stems bm25 query q =
  Cattery.use pool @@ fun (pack, blob) ->
  let fn uid =
    let value = Carton.of_uid pack blob ~uid in
    let str = Carton.Value.string value in
    let mail, _, length, tbl = Result.get_ok (Stem.of_string str) in
    let mail = Carton.Uid.unsafe_of_string mail in
    match score bm25 query (tbl, length, mail) with
    | Some (uid, score) -> Flux.Bqueue.put q (uid, score)
    | None -> Miou.yield ()
  in
  List.iter fn stems;
  Flux.Bqueue.close q

let query bm25 stems req _server pool =
  let open Vifu.Response.Syntax in
  match Vifu.Request.of_json req with
  | Ok { Format.lang; query } ->
      Logs.debug (fun m -> m "Start to search: %S" query);
      let actions = Tokenizer.[ (Whitespace, Remove); (Bert, Remove) ] in
      let tokens = Tokenizer.run ~encoding:UTF_8 actions (Seq.return query) in
      let stemmer = Snowball.create ~encoding:UTF_8 lang in
      let none_if_stop = none_if_stop lang in
      let@ () = fun () -> Snowball.remove stemmer in
      let fn = Fun.compose none_if_stop (Snowball.stem stemmer) in
      let tokens = Seq.filter_map fn tokens in
      let query = List.of_seq tokens in
      let q = Flux.Bqueue.(create with_close 0x7ff) in
      let prm = Miou.async @@ fun () -> score pool stems bm25 query q in
      let seq = Flux.Bqueue.to_seq q in
      let* () = Vifu.Response.with_json req (Format.scores ~uid:juid) seq in
      let* () = Vifu.Response.respond `OK in
      Miou.await_exn prm;
      Vifu.Response.return ()
  | Error _ ->
      let* () = Vifu.Response.with_text req "Invalid JSON object!\n" in
      Vifu.Response.respond `Bad_request

let run _ cidr gateway port =
  let devices =
    let open Mkernel in
    [
      Mnet.stack ~name:"service" ?gateway cidr;
      Emails.emails "archive";
      kv ~name:"rowex";
    ]
  in
  Mkernel.run devices
  @@ fun (daemon, tcp, _) (pool, avgdl, stems, _entries) rowex () ->
  let bm25 = { idf = rowex; avgdl } in
  let rng = Mirage_crypto_rng_mkernel.initialize (module RNG) in
  let@ () =
   fun () ->
    Mirage_crypto_rng_mkernel.kill rng;
    Mnet.kill daemon
  in
  let cfg = Vifu.Config.v port in
  let jquery = Format.query ~lang:jlang in
  let jstem = Jsont.(list juid) in
  let routes =
    let open Vifu.Route in
    let open Vifu.Uri in
    let open Vifu.Type in
    let any = Vifu.Uri.any in
    [
      get (rel / "script.js" /?? any) --> script;
      get (rel / "style.css" /?? any) --> style;
      post (json_encoding jquery) (rel / "query" /?? any) --> query bm25 stems;
      get (rel /?? any) --> index;
    ]
  in
  Logs.info (fun m -> m "HTTP server launched");
  Vifu.run ~cfg tcp routes pool

open Cmdliner

let output_options = "OUTPUT OPTIONS"
let verbosity = Logs_cli.level ~docs:output_options ()
let renderer = Fmt_cli.style_renderer ~docs:output_options ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc)

let t0 = Mkernel.clock_monotonic ()
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let neg fn = fun x -> not (fn x)

let reporter sources ppf =
  let re = Option.map Re.compile sources in
  let print src =
    let some re = (neg List.is_empty) (Re.matches re (Logs.Src.name src)) in
    Option.fold ~none:true ~some re
  in
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let pp header _tags k ppf fmt =
      let t1 = Mkernel.clock_monotonic () in
      let delta = Float.of_int (t1 - t0) in
      let delta = delta /. 1_000_000_000. in
      Fmt.kpf k ppf
        ("[+%a][%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue (fmt "%04.04f"))
        delta
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    match (level, print src) with
    | Logs.Debug, false -> k ()
    | _, true | _ -> msgf @@ fun ?header ?tags fmt -> pp header tags k ppf fmt
  in
  { Logs.report }

let regexp =
  let parser str =
    match Re.Pcre.re str with
    | re -> Ok (str, `Re re)
    | exception _ -> error_msgf "Invalid PCRegexp: %S" str
  in
  let pp ppf (str, _) = Fmt.string ppf str in
  Arg.conv (parser, pp)

let sources =
  let doc = "A regexp (PCRE syntax) to identify which log we print." in
  let open Arg in
  value & opt_all regexp [ ("", `None) ] & info [ "l" ] ~doc ~docv:"REGEXP"

let setup_sources = function
  | [ (_, `None) ] -> None
  | res ->
      let res = List.map snd res in
      let res =
        List.fold_left
          (fun acc -> function `Re re -> re :: acc | _ -> acc)
          [] res
      in
      Some (Re.alt res)

let setup_sources = Term.(const setup_sources $ sources)

let setup_logs utf_8 style_renderer sources level =
  Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Logs.set_level level;
  Logs.set_reporter (reporter sources Fmt.stdout);
  Option.is_none level

let setup_logs =
  Term.(const setup_logs $ utf_8 $ renderer $ setup_sources $ verbosity)

let ipv4 =
  let doc = "The IP address of the unikernel." in
  let ipaddr = Arg.conv (Ipaddr.V4.Prefix.of_string, Ipaddr.V4.Prefix.pp) in
  let open Arg in
  required & opt (some ipaddr) None & info [ "ipv4" ] ~doc ~docv:"IPv4"

let ipv4_gateway =
  let doc = "The IP gateway." in
  let ipaddr = Arg.conv (Ipaddr.V4.of_string, Ipaddr.V4.pp) in
  let open Arg in
  value & opt (some ipaddr) None & info [ "ipv4-gateway" ] ~doc ~docv:"IPv4"

let port =
  let doc = "The HTTP port" in
  let open Arg in
  value & opt int 80 & info [ "p"; "port" ] ~doc ~docv:"PORT"

let _cachesize =
  let doc = "The size of the cache (must be a power of two)." in
  let open Arg in
  value & opt int 0x100 & info [ "cachesize" ] ~doc ~docv:"SIZE"

let term =
  let open Term in
  const run $ setup_logs $ ipv4 $ ipv4_gateway $ port

let cmd =
  let info = Cmd.info "blame" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
