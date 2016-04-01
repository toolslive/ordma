open Lwt.Infix
       

module X = Lwt_rsocket 
module B = Lwt_rsocket.Bytes
(*
module X = Lwt_unix
module B = Lwt_bytes
 *)
external set32_prim : Lwt_bytes.t -> int -> int32 -> unit = "%caml_bigstring_set32"
external get32_prim : Lwt_bytes.t -> int -> int32 = "%caml_bigstring_get32"

let really_send client_fd bytes off len =
    let rec loop count off todo =
      if todo = 0
      then Lwt.return count
      else
        B.send client_fd bytes off todo [] >>= fun sent ->
        loop (count + 1) (off + sent) (todo - sent)
    in
    loop 0 off len >>= function
    | 0 | 1 -> Lwt.return_unit
    | n -> Lwt_io.printlf "send (%i bytes) in %i steps" len n

let really_read client_fd bytes off len =
  let rec loop count off todo =
    if todo = 0
    then Lwt.return count
    else 
      B.recv client_fd bytes off todo [] >>= fun received ->
      loop (count +1) (off + received) (todo - received)
  in
  loop 0 off len >>= function
  | 0 | 1 -> Lwt.return_unit
  | n -> Lwt_io.printf "received (%i bytes_ in %i steps" len n


let client host port len =

  Lwt_io.printlf "client (%s,%i)%!" host port >>= fun () ->
  let addr = Unix.inet_addr_of_string host in
  let fd = X.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sa = Unix.ADDR_INET (addr,port) in
  X.connect fd sa >>= fun () ->
  let rest = len - 4 in
  let bytes_out = B.create len in
  let () = set32_prim bytes_out 0 (rest |> Int32.of_int) in
  let bytes_in  = B.create len in
  let rec loop n =
    if n = 0
    then Lwt.return_unit
    else
      really_send fd bytes_out 0 len >>= fun () ->
      Lwt_io.printlf "(n:%i) ==>%!" n >>= fun () ->
      really_read fd bytes_in 0 len >>= fun () ->
      Lwt_io.printlf "\t <== %!"  >>= fun () ->
      assert (bytes_out = bytes_in);
      loop (n-1)
  in
  loop 40 >>= fun () ->
  X.close fd
                                     
let server host port len =
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
    let bytes = B.create len in
    let rec loop () =
      Lwt_io.printlf "protocol: recv%!" >>= fun () ->
      B.recv client_fd bytes 0 4      [] >>= fun received ->
      Lwt_io.printlf "wanted 4, got %i" received >>= fun () ->
      assert (received = 4);
      let rest = get32_prim bytes 0 |> Int32.to_int in
      Lwt_io.printlf "rest: %i" rest >>= fun () ->
      really_read client_fd bytes 4 rest >>= fun () ->
      let total = rest + 4 in
      Lwt_io.printlf "protocol: received (%i)%!" (rest + 4) >>= fun () ->
      Lwt_io.printlf "protocol: send (%i)%!" total  >>= fun () ->
      really_send client_fd bytes 0 total >>= fun () ->
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
    
let () =
  let () =
    let open Rsocket.Version in
    Printf.printf "ordma_version (%i,%i,%i,%s)\n" major minor patch git_revision
  in
  let engine = new Lwt_rsocket.rselect in
  Lwt_engine.set engine; 
  let () = Printf.printf "set engine to `rselect`\n%!" in
  if Array.length Sys.argv <> 5
  then
    Printf.printf "%s <client|server> host port len \n%!" Sys.argv.(0)
  else
    let host = Sys.argv.(2) in
    let port_s = Sys.argv.(3) in
    let port = int_of_string port_s in
    let len_s = Sys.argv.(4) in
    let len = int_of_string len_s in
    match Sys.argv.(1) with
    | "client" -> Lwt_main.run (client host port len)
    | "server" -> Lwt_main.run (server host port len)
                               
