module Lio = Lwt_io
module Lu = Lwt_unix

let (>>=) = Lwt.(>>=)
let (>>) a b = a >>= fun () -> b 

module Conversation = struct
  
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
    Lio.write out_c response >>
    Lio.write out_c "\r\n" >>
    Lio.flush out_c
    
  let read_data id in_c out_c =
    write_response out_c id response_354 >>
    let module S = Core.Core_string in
    let module B = Buffer in
    let buf = B.create 0 in
    let rec read () = 
      Lio.read ~count:80 in_c >>= fun data ->
      B.add_string buf data;
      match S.is_suffix (B.contents buf) "\r\n.\r\n" with
      | true -> Lwt.return (B.sub buf 0 (B.length buf - 5))
      | false -> read () in
    read ()

  let parse_request data_reader line = 
    Re_perl.re "^(\\S*)\\s*(.*)$" 
    |> Re.compile
    |> fun re -> Re.exec re line
    |> fun subs -> Re.get subs 1
    |> String.lowercase
    |> function
      | "helo" | "ehlo" -> Lwt.return Helo
      | "mail" -> Lwt.return (Mail (Re.get subs 2))
      | "rcpt" -> Lwt.return (Rcpt (Re.get subs 2))
      | "data" -> data_reader () >>= fun data -> Lwt.return (Data (data))
      | "quit" -> Lwt.return Quit
      | _ -> Lwt.return Unknown

  let rec process_request ?previous:(previous) callback buf id in_c out_c =
    let wr = write_response out_c id in 
    Lio.read_line in_c >>= fun line ->
    LOG "[%s] Received: %s" id line LEVEL DEBUG;
    parse_request (fun () -> read_data id in_c out_c) line >>= fun req -> 
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
      let msg = Buffer.contents buf in
      Buffer.clear buf;
      Lwt.join [
        (Lwt_unix.yield () >> (callback msg; Lwt.return ()));
        wr response_250;
      ]
    | Quit -> wr response_221
    | Unknown -> Lwt.return ()) >>
    match req with
    | Quit -> Lwt.return ()
    | r -> process_request ~previous:r callback buf id in_c out_c
  
  let start fd id callback = 
    LOG "[%s] Accepted a connection" id LEVEL DEBUG;
    let in_c = Lio.of_fd Lio.Input fd in
    let out_c = Lio.of_fd Lio.Output fd in
    let buf = Buffer.create 80 in
    write_response out_c id response_220 >>
    process_request callback buf id in_c out_c >>
    try_lwt Lio.close in_c with _ -> Lwt.return () >>
    try_lwt Lio.close out_c with _ -> Lwt.return () 
  
end

let sockaddr_to_string = function
  | Lu.ADDR_UNIX s -> s
  | Lu.ADDR_INET (a, p) ->
    Printf.sprintf "%s:%i" (Unix.string_of_inet_addr a) p 
    
let start ?max_pending:(max_pending=3) inet_addr port callback =
  let address = Unix.ADDR_INET (inet_addr, port) in
  let socket = Lu.socket Lu.PF_INET Lu.SOCK_STREAM 0 in
  Lu.setsockopt socket Lu.SO_REUSEADDR true;
  Lu.bind socket address;
  Lu.listen socket max_pending;
  let rec listen () =
    LOG "Waiting for a connection" LEVEL DEBUG;
    Lu.accept socket >>= fun (fd, a) -> 
    Lwt.join [
      listen ();
      (Lu.yield () >>
      let id = sockaddr_to_string a in
      Conversation.start fd id callback >>
      (LOG "[%s] Connection closed" id LEVEL DEBUG;
      Lwt.return ()));
    ] in
  listen () 
  |> (fun t -> Lwt.on_cancel t (fun () -> 
    LOG "Cancelled, closing socket" LEVEL DEBUG;
    Lu.close socket |> ignore); t)
    
  