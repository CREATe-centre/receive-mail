module Conversation : sig
  val start : Unix.file_descr -> string -> (string -> unit) -> unit
end = struct
  
  let response_220 = "220 receive-mail SMTP Server"
  let response_250 = "250 OK"
  let response_354 = "354"
  let response_221 = "221 Goodbye"
  
  type request =
    | Helo
    | Mail of string
    | Rcpt of string
    | Data of string
    | Quit
    | Unknown
 
  let write_response out_c id response = 
    LOG "[%s] Sent: %s" id response LEVEL DEBUG;
    output_string out_c response;
    output_string out_c "\r\n";
    flush out_c
    
  let read_data id in_c out_c =
    write_response out_c id response_354;
    let module S = Core.Core_string in
    let module B = Buffer in
    let to_read = 3 in
    let ibuf = S.make to_read (char_of_int 0) in
    let buf = B.create to_read in
    let rec read () = 
      let data = input in_c ibuf 0 to_read |>
        String.sub ibuf 0 in
      B.add_string buf data;
      match S.is_suffix (B.contents buf) "\r\n.\r\n" with
      | true -> B.sub buf 0 (B.length buf - 5)
      | false -> read () in
    read ()

  let parse_request data_reader line = 
    Re_perl.re "^(\\S*)\\s*(.*)$" 
    |> Re.compile
    |> fun re -> Re.exec re line
    |> fun subs -> Re.get subs 1
    |> String.lowercase
    |> function
      | "helo" | "ehlo" -> Helo
      | "mail" -> Mail (Re.get subs 2)
      | "rcpt" -> Rcpt (Re.get subs 2)
      | "data" -> Data (data_reader ())
      | "quit" -> Quit
      | _ -> Unknown

  let rec process_request ?previous:(previous) callback buf id in_c out_c =
    let wr = write_response out_c id in 
    let line = input_line in_c in
    LOG "[%s] Received: %s" id line LEVEL DEBUG;
    let req = parse_request (fun () -> read_data id in_c out_c) line in 
    (match req with
    | Helo -> wr response_250
    | Mail from ->
      Buffer.add_string buf from;
      Buffer.add_string buf "\r\n";
      wr response_250
    | Rcpt rcpt ->
      Buffer.add_string buf rcpt;
      Buffer.add_string buf "\r\n";
      wr response_250
    | Data data -> 
      Buffer.add_string buf "\r\n";
      Buffer.add_string buf data;
      LOG "[%s] Received mail, pushing to callback" id LEVEL DEBUG;
      Buffer.contents buf |> callback;
      Buffer.clear buf;
      wr response_250
    | Quit -> wr response_221
    | Unknown -> ());
    match req with
    | Quit -> ()
    | r -> process_request ~previous:r callback buf id in_c out_c
  
  let start fd id callback = 
    LOG "[%s] Accepted a connection" id LEVEL DEBUG;
    let in_c = Unix.in_channel_of_descr fd in
    let out_c = Unix.out_channel_of_descr fd in
    let buf = Buffer.create 80 in
    write_response out_c id response_220;
    process_request callback buf id in_c out_c;
    close_in in_c;
    try close_out out_c with _ -> ()
  
end

let sockaddr_to_string = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (a, p) ->
    Printf.sprintf "%s:%i" (Unix.string_of_inet_addr a) p 
    
let start addr port callback =
  let a = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  try
    Unix.bind sock a ;
    Unix.listen sock 3;
    while true do
      LOG "Waiting for a connection" LEVEL DEBUG;
      let fd, a = Unix.accept sock in
      let id = sockaddr_to_string a in
      Conversation.start fd id callback;
      LOG "[%s] Connection closed" (sockaddr_to_string a) LEVEL DEBUG;
    done
  with e -> 
    Unix.close sock;
    raise e
  