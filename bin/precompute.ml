let record_and_filter index (value, cursor, uid) =
  Hashtbl.add index uid cursor;
  match Carton.Value.kind value with
  | `A | `B | `D -> None
  | `C ->
      let str = Carton.Value.string value in
      let stem = Stem.of_string str in
      let mail, blob, length, tbl = Result.get_ok stem in
      let tokens = List.of_seq (Hashtbl.to_seq tbl) in
      let mail = Carton.Uid.unsafe_of_string mail
      and blob = Carton.Uid.unsafe_of_string blob in
      Some { Format.mail; blob; length; tokens }

let sha1 =
  let module Hash = Digestif.SHA1 in
  let feed_bigstring bstr ctx = Hash.feed_bigstring ctx bstr in
  let feed_bytes buf ~off ~len ctx = Hash.feed_bytes ctx ~off ~len buf in
  let hash =
    {
      Carton.First_pass.feed_bytes;
      feed_bigstring;
      serialize = Fun.compose Hash.to_raw_string Hash.get;
      length = Hash.digest_size;
    }
  in
  Carton.First_pass.Digest (hash, Hash.empty)

let identify =
  let ( $ ) f g = fun x -> f (g x) in
  let pp_kind ppf = function
    | `A -> Fmt.string ppf "mail"
    | `B -> Fmt.string ppf "blob"
    | `C | `D -> Fmt.string ppf "deadbeef"
  in
  let open Digestif in
  let init kind (len : Carton.Size.t) =
    let hdr = Fmt.str "%a %d\000" pp_kind kind (len :> int) in
    let ctx = SHA1.empty in
    SHA1.feed_string ctx hdr
  in
  let feed bstr ctx = SHA1.feed_bigstring ctx bstr in
  let serialize = SHA1.(Carton.Uid.unsafe_of_string $ to_raw_string $ get) in
  { Carton.First_pass.init; feed; serialize }

let run _quiet archive rowex =
  Miou.run @@ fun () ->
  let fd = Unix.openfile archive Unix.[ O_RDONLY ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let rowex = Bancos.openfile ~readers:1 ~writers:1 rowex in
  let finally () = Bancos.close rowex in
  Fun.protect ~finally @@ fun () ->
  let ref_length = Digestif.SHA1.digest_size in
  let from = Flux.Source.file ~filename:archive 0x7ff in
  let via = Carton_miou_flux.first_pass ~digest:sha1 ~ref_length in
  let into = Carton_miou_flux.oracle ~identify in
  let oracle, _leftover = Flux.Stream.run ~from ~via ~into in
  let pack =
    let z = Bstr.create De.io_buffer_size in
    let allocate bits = De.make_window ~bits in
    let index _uid = assert false in
    let cache =
      let map (fd, max) ~pos len =
        let len = Int.min (max - pos) len in
        let pos = Int64.of_int pos in
        let open Bigarray in
        let barr = Unix.map_file fd ~pos char c_layout false [| len |] in
        array1_of_genarray barr
      in
      let { Unix.st_size; _ } = Unix.fstat fd in
      Cachet.make ?cachesize:None (*FIXME*) ~pagesize:512 ~map (fd, st_size)
    in
    Carton.of_cache cache ~z ~allocate ~ref_length index
  in
  let index = Hashtbl.create 0x7ff in
  let from = Carton_miou_flux.entries ~threads:0 pack oracle in
  let via = Flux.Flow.filter_map (record_and_filter index) in
  let into = Flux.Sink.list in
  let documents, _leftover = Flux.Stream.run ~from ~via ~into in
  let _N = Float.of_int (List.length documents) in
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
  let fn token freq cmds =
    let freq = Float.of_int freq in
    let value = Float.(log (1. +. ((_N -. freq +. 0.5) /. (freq +. 0.5)))) in
    let value = Int64.bits_of_float value in
    let token = Rowex.key token in
    Bancos.insert rowex token value :: cmds
  in
  let cmds = Hashtbl.fold fn df [] in
  let fn cmd = match Bancos.await cmd with `Ok -> () | _ -> assert false in
  List.iter fn cmds (* NOTE(dinosaure): we should use [rev_iter]. *)

open Cmdliner

let archive =
  let doc = Arg.info ~doc:"The email archive" [ "archive"; "email-archive" ] in
  Arg.(required & opt (some file) None & doc)

let rowex =
  let doc = Arg.info ~doc:"The resulting rowex file" [ "rowex" ] in
  Arg.(required & opt (some string) None & doc)

let cmd =
  let info = Cmd.info "precompute" in
  let term = Term.(const run $ const () $ archive $ rowex) in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
