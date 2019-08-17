let utf16_to_utf8  : string -> string = fun s ->
  let buf = UTF8.Buffer.create 64 in
  UTF16.fold (fun () -> UTF8.Buffer.add_char buf) () s;
  UTF8.Buffer.contents buf

let utf32_to_utf8  : string -> string = fun s ->
  let buf = UTF8.Buffer.create 64 in
  UTF32.fold (fun () -> UTF8.Buffer.add_char buf) () s;
  UTF8.Buffer.contents buf

let utf8_to_utf16  : string -> string = fun s ->
  let buf = UTF16.Buffer.create 64 in
  UTF8.fold (fun () -> UTF16.Buffer.add_char buf) () s;
  UTF16.Buffer.contents buf

let utf32_to_utf16 : string -> string = fun s ->
  let buf = UTF16.Buffer.create 64 in
  UTF32.fold (fun () -> UTF16.Buffer.add_char buf) () s;
  UTF16.Buffer.contents buf

let utf8_to_utf32  : string -> string = fun s ->
  let buf = UTF32.Buffer.create 64 in
  UTF8.fold (fun () -> UTF32.Buffer.add_char buf) () s;
  UTF32.Buffer.contents buf

let utf16_to_utf32 : string -> string = fun s ->
  let buf = UTF32.Buffer.create 64 in
  UTF16.fold (fun () -> UTF32.Buffer.add_char buf) () s;
  UTF32.Buffer.contents buf
