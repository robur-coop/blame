let src = Logs.Src.create "blame.emails"

module Log = (val Logs.src_log src : Logs.LOG)

let source_of_blk blk =
  let pagesize = Mkernel.Block.pagesize blk in
  let bstr = Bstr.create pagesize in
  let rec go src_off () =
    try
      Mkernel.Block.atomic_read blk ~src_off ~dst_off:0 bstr;
      Seq.Cons (Bstr.to_string bstr, go (src_off + pagesize))
    with _ -> Seq.Nil
  in
  Flux.Source.seq (go 0)

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

let semantic_to_docs =
  let fn acc (mime, lang, contents) =
    let contents = Carton.Uid.unsafe_of_string contents in
    { Format.mime; lang; contents } :: acc
  in
  Email.Semantic.fold fn []

let headers =
  let rec consume decoder fields = function
    | `Await -> (`Continue decoder, fields)
    | `End _ -> (`End, fields)
    | `Field field ->
        let field = Mrmime.Location.prj field in
        Mrmime.Hd.decode decoder |> consume decoder (field :: fields)
    | `Malformed _ -> (`Malformed, fields)
  in
  let init () =
    let decoder = Mrmime.Hd.decoder Mrmime.Field_name.Map.empty in
    let[@warning "-8"] (`Await : Mrmime.Hd.decode) = Mrmime.Hd.decode decoder in
    (`Continue decoder, [])
  and push (state, fields) str =
    match state with
    | `Continue decoder ->
        Mrmime.Hd.src decoder str 0 (String.length str);
        consume decoder fields (Mrmime.Hd.decode decoder)
    | `End | `Malformed -> (state, fields)
  and full (state, _) =
    match state with `End | `Malformed -> true | _ -> false
  and stop (_, fields) = fields in
  Flux.Sink { init; push; full; stop }

let bstr_to_string =
  let flow (Flux.Sink k) =
    let init () =
      let tmp = Bytes.create 0x7ff in
      let acc = k.init () in
      (tmp, acc)
    and push (tmp, acc) bstr =
      let rec go acc src_off =
        if src_off < Bstr.length bstr then begin
          let len = Int.min (Bstr.length bstr - src_off) (Bytes.length tmp) in
          Bstr.blit_to_bytes bstr ~src_off tmp ~dst_off:0 ~len;
          let acc = k.push acc (Bytes.sub_string tmp 0 len) in
          if k.full acc then (tmp, acc) else go acc (src_off + len)
        end
        else (tmp, acc)
      in
      go acc 0
    and full _ = false
    and stop (_, acc) = k.stop acc in
    Flux.Sink { init; push; full; stop }
  in
  { Flux.flow }

let record_and_filter index (value, cursor, uid) =
  Hashtbl.add index uid cursor;
  let ( let* ) = Option.bind in
  match Carton.Value.kind value with
  | `B | `D -> None
  | `C -> Some (Either.Left uid)
  | `A ->
      let str = Carton.Value.string value in
      let m = Email.of_string str in
      let* { Email.Skeleton.headers; _ }, s = Result.to_option m in
      let docs = semantic_to_docs s in
      if docs = [] then None
      else Some (Either.Right (uid, Carton.Uid.unsafe_of_string headers, docs))

let crlf = Bstr.of_string "\r\n"

let end_encoded_word uchr0 uchr1 =
  let uchr0 = Uchar.to_int uchr0 and uchr1 = Uchar.to_int uchr1 in
  uchr0 = 0x3f && uchr1 = 0x3d

let begin_encoded_word uchr0 uchr1 =
  let uchr0 = Uchar.to_int uchr0 and uchr1 = Uchar.to_int uchr1 in
  uchr0 = 0x3d && uchr1 = 0x3f

let unstrctrd_with_encoded_words_to_string v =
  let ( let* ) = Option.bind in
  let unstrctrd_to_string v =
    let v = Unstrctrd.of_list v in
    let v = Result.get_ok v in
    Unstrctrd.to_utf_8_string v
  in
  let rec until_end_encoded_word acc = function
    | `Uchar uchr0 :: `Uchar uchr1 :: rest when end_encoded_word uchr0 uchr1 ->
        let acc = `Uchar uchr1 :: `Uchar uchr0 :: acc in
        let v = Unstrctrd.of_list (List.rev acc) in
        let* v = Result.to_option v in
        let str = Unstrctrd.to_utf_8_string v in
        begin match Mrmime.Encoded_word.of_string str with
        | Ok { Mrmime.Encoded_word.data = Ok str; _ } -> Some (str, rest)
        | _ -> None
        end
    | (#Unstrctrd.elt as x) :: (#Unstrctrd.elt as y) :: rest ->
        until_end_encoded_word (y :: x :: acc) rest
    | (`Open _ | `Close) :: rest -> until_end_encoded_word acc rest
    | _ -> None
  in
  let rec go acc unstrctrd = function
    | `Uchar uchr0 :: `Uchar uchr1 :: rest when begin_encoded_word uchr0 uchr1
      ->
        let acc =
          if List.is_empty unstrctrd then acc
          else unstrctrd_to_string (List.rev unstrctrd) :: acc
        in
        begin match
          until_end_encoded_word [ `Uchar uchr1; `Uchar uchr0 ] rest
        with
        | Some (ew, rest) -> go (ew :: acc) [] rest
        | None -> go acc [ `Uchar uchr1; `Uchar uchr0 ] rest
        end
    | (#Unstrctrd.elt as elt) :: rest -> go acc (elt :: unstrctrd) rest
    | (`Open _ | `Close) :: rest -> go acc unstrctrd rest
    | [] ->
        if List.is_empty unstrctrd then List.rev acc
        else List.rev (unstrctrd_to_string (List.rev unstrctrd) :: acc)
  in
  go [] [] v |> String.concat ""

let hdopt = function x :: _ -> Some x | [] -> None

let to_entry pack (uid, hdrs, docs) =
  let ( let* ) = Option.bind in
  let size = Carton.size_of_uid pack ~uid:hdrs Carton.Size.zero in
  let blob = Carton.Blob.make ~size in
  let value = Carton.of_uid pack blob ~uid:hdrs in
  let len = Carton.Value.length value in
  let bstr = Carton.Value.bigstring value in
  let bstr = Bstr.sub bstr ~off:0 ~len in
  let from = Flux.Source.list [ bstr; crlf ] in
  let via = bstr_to_string in
  let into = headers in
  let hdrs, _leftover = Flux.Stream.run ~from ~via ~into in
  let open Mrmime in
  let* title =
    let fn = function
      | Field.Field (fn, Field.Unstructured, v) ->
          if Field_name.equal fn Field_name.subject then
            Some (unstrctrd_with_encoded_words_to_string v)
          else None
      | _ -> None
    in
    List.find_map fn hdrs
  in
  let* from =
    let fn = function
      | Field.Field (fn, Field.Mailboxes, v) ->
          if Field_name.equal fn Field_name.from then hdopt v else None
      | _ -> None
    in
    List.find_map fn hdrs
  in
  let* date, _ =
    let fn = function
      | Field.Field (fn, Field.Date, v) ->
          if Field_name.equal fn Field_name.date then
            Result.to_option (Date.to_ptime v)
          else None
      | _ -> None
    in
    List.find_map fn hdrs
  in
  Some { Format.title; from; date; uid; docs }

let emails ?cachesize name =
  let map blk ~pos len =
    let bstr = Bstr.create len in
    Mkernel.Block.read blk ~src_off:pos bstr;
    bstr
  in
  let fn blk () =
    let pagesize = Mkernel.Block.pagesize blk in
    let ref_length = Digestif.SHA1.digest_size in
    let from = source_of_blk blk in
    let via = Carton_miou_flux.first_pass ~digest:sha1 ~ref_length in
    let into = Carton_miou_flux.oracle ~identify in
    let oracle, _leftover = Flux.Stream.run ~from ~via ~into in
    Log.info (fun m ->
        m "Number of objects: %d" oracle.Carton.number_of_objects);
    let pack =
      let z = Bstr.create De.io_buffer_size in
      let allocate bits = De.make_window ~bits in
      let index _uid = assert false in
      let cache = Cachet.make ?cachesize ~pagesize ~map blk in
      Carton.of_cache cache ~z ~allocate ~ref_length index
    in
    let index = Hashtbl.create 0x7ff in
    let from = Carton_miou_flux.entries ~threads:0 pack oracle in
    let via = Flux.Flow.filter_map (record_and_filter index) in
    let into = Flux.Sink.list in
    let documents_and_emails, _leftover = Flux.Stream.run ~from ~via ~into in
    let documents, emails = List.partition_map Fun.id documents_and_emails in
    Log.info (fun m ->
        m "%d document(s) and %d email(s)" (List.length documents)
          (List.length emails));
    let pack =
      let index uid = Carton.Local (Hashtbl.find index uid) in
      Carton.with_index pack index
    in
    let from = Flux.Source.list emails in
    let via = Flux.Flow.filter_map (to_entry pack) in
    let into = Flux.Sink.list in
    let entries, _leftover = Flux.Stream.run ~from ~via ~into in
    ((pack, oracle.Carton.hash), documents, entries)
  in
  let open Mkernel in
  map fn [ block name ]
