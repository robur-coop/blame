[@@@warning "-33"]

module RNG = Mirage_crypto_rng.Fortuna

let uid =
  let open Vifu.Uri in
  let dec str =
    let hash = Ohex.decode str in
    if String.length hash = Digestif.SHA1.digest_size
    then Carton.Uid.unsafe_of_string hash else Fmt.invalid_arg "Invalid UID" in
  let enc (uid : Carton.Uid.t) = Ohex.encode (uid :> string) in
  conv dec enc (string `Path)

let list entries req _server () =
  let open Vifu.Response.Syntax in
  let* () = Vifu.Response.add ~field:"content-type" "application/json; charset=utf-8" in
  let* () = Vifu.Response.with_json ~compression:`DEFLATE req
    (Jsont.list Emails.json) entries in
  Vifu.Response.respond `OK

let show pack req uid _server () =
  let open Vifu.Response.Syntax in
  try
    let size = Carton.size_of_uid pack ~uid Carton.Size.zero in
    let blob = Carton.Blob.make ~size in
    let value = Carton.of_uid pack blob ~uid in
    match Carton.Value.kind value with
    | `B | `C | `D ->
        let* () = Vifu.Response.with_text req "Invalid object (bad type)\n" in
        Vifu.Response.respond `Not_found
    | `A ->
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
          Bstr.sub bstr ~off:0 ~len in
        let seq = Email.to_seq ~load t in
        let fn = function
          | `String str -> str
          | `Value bstr -> Bstr.to_string bstr in
        let seq = Seq.map fn seq in
        let from = Flux.Source.seq seq in
        let* () = Vifu.Response.add ~field:"content-type" "message/rfc822; charset=utf-8" in
        let* () = Vifu.Response.with_source req from in
        Vifu.Response.respond `OK
  with _ ->
    let* () = Vifu.Response.empty in
    Vifu.Response.respond `Not_found

let index req _server () =
  let open Vifu.Response.Syntax in
  let from = Flux.Source.array Documents.index_html in
  let* () = Vifu.Response.add ~field:"content-type" "text/html" in
  let* () = Vifu.Response.with_source req from in
  Vifu.Response.respond `OK

let run _ cidr gateway port =
  let devices =
    let open Mkernel in
    [ Mnet.stackv4 ~name:"service" ?gateway cidr
    ; Emails.emails "archive" ]
  in
  Mkernel.run devices @@ fun (daemon, tcpv4, _udpv4) (pack, entries) () ->
  Logs.info (fun m -> m "%d email(s)" (List.length entries));
  let rng = Mirage_crypto_rng_mkernel.initialize (module RNG) in
  let finally () =
    Mirage_crypto_rng_mkernel.kill rng;
    Mnet.kill daemon
  in
  Fun.protect ~finally @@ fun () ->
  let cfg = Vifu.Config.v port in
  let routes =
    let open Vifu.Route in
    let open Vifu.Uri in
    [ get (rel / "list" /?? any) --> (list entries)
    ; get (rel / "get" /% uid /?? any) --> (show pack)
    ; get (rel /?? any) --> index ]
    (* [ get (rel /?? any) --> index emails
    ; get (rel /% sha1 /?? any) --> one emails ] *)
  in
  Vifu.run ~cfg tcpv4 routes ()

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
    let k _ = over (); k () in
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
