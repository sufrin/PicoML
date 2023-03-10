(* 

   Concise implementation of utf8 strings and unicode code-points 
   See http://www.faqs.org/rfcs/rfc3629.html
*)

type utf8string = string

type t = utf8string

type unicodechar = Uchar.t (* Not a very useful type; but compatible with modern buffers *)

exception IllformedUtf8

let next_from_char_stream:  char Stream.t -> unicodechar = fun s -> 
  let code = match Stream.next s with
    | '\000'..'\127' as c ->
        Char.code c
    | '\192'..'\223' as c ->
        let n1 = Char.code c in
        let n2 = Char.code (Stream.next s) in
        if (n2 lsr 6 != 0b10) then raise IllformedUtf8;
        ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f)
    | '\224'..'\239' as c ->
        let n1 = Char.code c in
        let n2 = Char.code (Stream.next s) in
        let n3 = Char.code (Stream.next s) in
        if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then raise IllformedUtf8;
        ((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
    | '\240'..'\247' as c ->
        let n1 = Char.code c in
        let n2 = Char.code (Stream.next s) in
        let n3 = Char.code (Stream.next s) in
        let n4 = Char.code (Stream.next s) in
        if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10)
        then raise IllformedUtf8;
        ((n1 land 0x07) lsl 18) lor ((n2 land 0x3f) lsl 12) lor
        ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f)
    | _ -> raise IllformedUtf8
   in Uchar.of_int code
    
let put_unicode_char: Buffer.t -> unicodechar -> unit = Buffer.add_utf_8_uchar

module Width : sig
   (* Width (in bytes) of a utf8-coded unicode point starting with the given char *)
   val char : char -> int 
   (* Width (in bytes) of a utf8-coded unicode point starting with the given byte *)   
   val byte : int -> int
end =
struct
    let count = Array.make 256 0
    let _ = 
       for i = 0   to 127 do count.(i) <- 1 done;
       for i = 192 to 223 do count.(i) <- 2 done;
       for i = 224 to 239 do count.(i) <- 3 done;
       for i = 240 to 247 do count.(i) <- 4 done
    let char ch = count.(Char.code ch)
    and byte b  = count.(b)
end

let length: utf8string -> int = fun s ->
    let count = String.length s in 
    let rec len n i =
      if i = count then n else
      if i > count then raise IllformedUtf8
      else 
        let skip = Width.char(s.[i]) 
        in  if skip > 0 then len (succ n) (i + skip) 
        else 
            raise IllformedUtf8
    in
        len 0 0

let from_string = fun str -> 
    let s = Stream.of_string str
    in  fun() -> try Some(next_from_char_stream s) with Stream.Failure -> None
    
let from_bytes = fun str -> 
    let s = Stream.of_bytes str
    in  fun() -> try Some(next_from_char_stream s) with Stream.Failure -> None
    
let from_channel = fun chan -> 
    let s = Stream.of_channel chan
    in  fun() -> try  Some(next_from_char_stream s) 
                 with Stream.Failure -> (* close_in chan; *) None

    
let to_utf8string: unicodechar -> utf8string = fun u ->
    let b = Buffer.create 4
    in  put_unicode_char b u;
        Buffer.contents b
        
let to_unicodechar: utf8string -> unicodechar = fun str ->
    let s = Stream.of_string str in next_from_char_stream s

