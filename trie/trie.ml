type 'a node = Leaf of string * 'a | Node of 'a node array | Null
type 'a t = { mutable root : 'a node }

let create () = { root = Null }
let ( .![] ) str idx = Char.code (String.unsafe_get str idx)
(* NOTE(dinosaure): [unsafe_get] is really important here because we probably
   would like to go far more than the length of the key but we expect (via the
   padding) to reach ['\x00']. *)

let rec find_node key idx = function
  | Leaf (key', v) when String.equal key key' -> v
  | Leaf _ | Null -> raise Not_found
  | Node arr -> find_node key (succ idx) arr.(key.![idx])

let find t key = find_node key 0 t.root

let rec insert_node key idx value = function
  | Null -> Leaf (key, value)
  | Leaf (key', _) when String.equal key key' -> Leaf (key, value)
  | Leaf (key', v') ->
      let arr = Array.make 256 Null in
      let c0 = key.![idx] and c1 = key'.![idx] in
      if c0 = c1 then begin
        arr.(c0) <- insert_node key (succ idx) value (Leaf (key', v'));
        Node arr
      end
      else begin
        arr.(c0) <- Leaf (key, value);
        arr.(c1) <- Leaf (key', v');
        Node arr
      end
  | Node arr ->
      let chr = key.![idx] in
      arr.(chr) <- insert_node key (succ idx) value arr.(chr);
      Node arr

let insert t key value = t.root <- insert_node key 0 value t.root

let iter fn t =
  let rec go = function
    | Null -> ()
    | Leaf (key, value) -> fn key value
    | Node arr -> Array.iter go arr
  in
  go t.root

(* Serialization *)

let output_int64_le =
  let tmp = Bytes.create 8 in
  fun oc value ->
    Bytes.set_int64_le tmp 0 value;
    output_bytes oc tmp

let output_varint oc value =
  let rec go value =
    if value < 0x80 then output_byte oc value
    else begin
      output_byte oc (0x80 lor (value land 0x7f));
      go (value lsr 7)
    end
  in
  go value

let output_caml_string oc str =
  let len = String.length str in
  let wosize = (len / 8) + 1 in
  let hdr = (wosize lsl 10) lor 0b11111100 in
  output_int64_le oc (Int64.of_int hdr);
  output_string oc str;
  let pad = (wosize * 8) - len in
  let rem = String.make (pad - 1) '\x00' in
  output_string oc rem;
  output_byte oc (pad - 1)

let output_variable_length oc str =
  output_varint oc (String.length str);
  output_string oc str

let rec serialize_node to_string oc = function
  | Null -> 0L
  | Leaf (key, value) ->
      let pos = LargeFile.pos_out oc in
      output_byte oc 0x00;
      output_caml_string oc key;
      let str = to_string value in
      output_variable_length oc str;
      pos
  | Node arr ->
      let addresses = Array.map (serialize_node to_string oc) arr in
      let pos = LargeFile.pos_out oc in
      output_byte oc 0x01;
      Array.iter (output_int64_le oc) addresses;
      pos

let serialize to_string oc ~pagesize t =
  output_byte oc 0xff;
  output_int64_le oc 0x00L;
  let root = serialize_node to_string oc t.root in
  let pos = Int64.to_int (LargeFile.pos_out oc) in
  let rem = pos mod pagesize in
  if rem <> 0 then begin
    let padding = pagesize - rem in
    let str = String.make padding '\x00' in
    output_string oc str
  end;
  let tmp = LargeFile.pos_out oc in
  LargeFile.seek_out oc 1L;
  output_int64_le oc root;
  LargeFile.seek_out oc tmp

(* Deserialization *)

let get_varint cache pos =
  let rec go pos shift acc =
    let byte = Cachet.get_uint8 cache pos in
    let acc = acc lor ((byte land 0x7f) lsl shift) in
    if byte land 0x80 = 0 then (acc, pos + 1) else go (pos + 1) (shift + 7) acc
  in
  go pos 0 0

let get_caml_string cache off =
  let hdr = Cachet.get_int64_le cache off in
  let wosize = Int64.to_int (Int64.shift_right_logical hdr 10) in
  let off = off + 8 in
  let wlen = wosize * 8 in
  let pad = Cachet.get_uint8 cache (off + wlen - 1) + 1 in
  let slen = wlen - pad in
  let str = Cachet.get_string cache ~len:slen off in
  (str, off + wlen)

let get_variable_length cache pos =
  let len, pos = get_varint cache pos in
  let str = Cachet.get_string cache ~len pos in
  (str, pos + len)

let rec lookup_node cache of_string key idx addr =
  if addr = 0L then None
  else
    let off = Int64.to_int addr in
    let tag = Cachet.get_uint8 cache off in
    match tag with
    | 0x00 ->
        let key', next = get_caml_string cache (off + 1) in
        if String.equal key key' then
          let value, _ = get_variable_length cache next in
          Some (of_string value)
        else None
    | 0x01 ->
        if idx >= String.length key + 1 then None
        else
          let chr = key.![idx] in
          let addr' = Cachet.get_int64_le cache (off + 1 + (chr * 8)) in
          lookup_node cache of_string key (idx + 1) addr'
    | _ -> invalid_arg "Trie.lookup: invalid node tag"

let lookup cache of_string key =
  let magic = Cachet.get_uint8 cache 0 in
  if magic <> 0xff then invalid_arg "Trie.lookup: invalid magic byte";
  let root = Cachet.get_int64_le cache 1 in
  lookup_node cache of_string key 0 root
