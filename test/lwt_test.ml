open Lwt.Infix
       

module X = Lwt_rsocket 
module B = Lwt_rsocket.Bytes
(*
module X = Lwt_unix
module B = Lwt_bytes
 *)
             
let client host port =
  Lwt_io.printlf "client (%s,%i)%!" host port >>= fun () ->
  let addr = Unix.inet_addr_of_string host in
  let fd = X.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sa = Unix.ADDR_INET (addr,port) in
  X.connect fd sa >>= fun () ->
  let len = 1024 in
  let rest = len - 4 in
  let bytes_out = B.create len in
  let bytes_in  = B.create len in
  let rec loop n =
    if n = 0
    then Lwt.return_unit
    else
      B.send fd bytes_out 0 len [] >>= fun sent ->
      assert (sent = len);
      Lwt_io.printlf "(n:%i) sent %i%!" n sent >>= fun () ->
      B.recv fd bytes_in  0 len [] >>= fun received ->
      Lwt_io.printlf "\t received %i%!" received >>= fun () ->
      assert (received = len);
      loop (n-1)
  in
  loop 1000
                                     
let server host port =
  Lwt_io.printlf "server (%s,%i)%!" host port >>= fun () ->
  let addr = Unix.inet_addr_of_string host in
  let server_socket = X.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_io.printlf "got a socket" >>= fun () ->
  let sa = Unix.ADDR_INET (addr, port) in
  let () = X.setsockopt server_socket Unix.SO_REUSEADDR true in 
  Lwt_io.printlf "set REUSEADDR true%!" >>= fun () ->
  (* *)
  let () = X.bind server_socket sa in
  Lwt_io.printlf "bind ok%!" >>= fun () ->
  let () = X.listen server_socket 10 in
  Lwt_io.printlf "listen ok%!" >>= fun () ->
  
  let protocol client_fd =
    let len = 4096 in
    let bytes = Bytes.create len in
    let rec loop () =
      X.recv client_fd bytes 0 len      [] >>= fun received ->
      X.send client_fd bytes 0 received [] >>= fun sent ->
      assert (received = sent);
      loop ()
    in
    loop ()
  in
  let detach_safe_connection fd peer protocol =
    let f () =
      Lwt.catch
        (fun () ->
          Lwt_io.printlf "running protocol" >>= fun () ->
          protocol fd
        )
        (fun ex ->
          Lwt_io.printlf "end of protocol:%s%!" (Printexc.to_string ex)
          >>= fun () ->
          X.close fd)
    in
    Lwt.ignore_result (f ())
  in
  let rec loop () =
    Lwt_io.printlf "pre_accept%!" >>= fun () ->
    X.accept server_socket >>= fun (fd, peer) ->
    Lwt_io.printlf "accepted ... detaching%!" >>= fun () ->
    let () = detach_safe_connection fd peer protocol in
    loop ()
  in
  Lwt.finalize
    (fun () -> loop ())
    (fun () -> X.close server_socket)
    
  (*

  let protocol (client_fd, client_addr) =
    try
      let len = 4096 in
      let bytes = Bytes.create len in
      let rec loop () =
        let received = Rsocket.rrecv client_fd bytes 0 len [] in
        let sent = Rsocket.rsend     client_fd bytes 0 received [] in
        loop ()
      in
      loop ()
      with _ ->
        (* shutdown client_sock SHUTDOWN_ALL ?? *)
        Rsocket.rclose client_fd
  in 
  *)


let () =
  
  if Array.length Sys.argv <> 4
  then
    Printf.printf "%s <client|server> host port\n%!" Sys.argv.(0)
  else
    let host = Sys.argv.(2) in
    let port_s = Sys.argv.(3) in
    let port = int_of_string port_s in
    Lwt_engine.set (new Lwt_engine.select :> Lwt_engine.t);
    match Sys.argv.(1) with
    | "client" -> Lwt_main.run (client host port)
    | "server" -> Lwt_main.run (server host port)
                               
