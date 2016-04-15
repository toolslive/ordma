open Lwt.Infix
open Cmdliner       

(*
module X = Lwt_rsocket 
module B = Lwt_rsocket.Bytes
 *)

module X = struct
  include Lwt_unix
  let identifier fd =
    let ufd = unix_file_descr fd in
    let (r:int) = Obj.magic ufd in
    r
end
module B = Lwt_bytes


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


let client host port len n =
  let t = 
    Lwt_io.printlf "client (%s,%i)%!" host port >>= fun () ->
    let addr = Unix.inet_addr_of_string host in
    let fd = X.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let sa = Unix.ADDR_INET (addr,port) in
    (*Lwt_io.printlf "connecting%!" >>= fun () -> *)
    X.connect fd sa >>= fun () ->
    (* Lwt_io.printlf "connected%!" >>= fun () -> *)
    let rest = len - 4 in
    let bytes_out = B.create len in
    let () = set32_prim bytes_out 0 (rest |> Int32.of_int) in
    let bytes_in  = B.create len in
    let rec loop n =
      if n = 0
      then Lwt.return_unit
      else
        (if n mod 1000 = 0
         then Lwt_io.printlf "%i\n" n
         else Lwt.return_unit
        )
        >>= fun () ->
        really_send fd bytes_out 0 len >>= fun () ->
        really_read fd bytes_in 0 len >>= fun () ->
        assert (bytes_out = bytes_in);
        loop (n-1)
    in
    loop n >>= fun () ->
    X.close fd
  in
  Lwt_main.run t
                                     
let server host port buffer_size =
  let t = 
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
      let bytes = B.create buffer_size in
      let rec loop () =
        (* Lwt_io.printlf "protocol: recv%!" >>= fun () -> *)
        B.recv client_fd bytes 0 4      [] >>= fun received ->
        (* Lwt_io.printlf "wanted 4, got %i" received >>= fun () -> *)
        assert (received = 4);
        let rest = get32_prim bytes 0 |> Int32.to_int in
        (* Lwt_io.printlf "rest: %i" rest >>= fun () -> *)
        really_read client_fd bytes 4 rest >>= fun () ->
        let total = rest + 4 in
        (* Lwt_io.printlf "protocol: received (%i)%!" (rest + 4) >>= fun () ->
        Lwt_io.printlf "protocol: send (%i)%!" total  >>= fun () -> *)
        really_send client_fd bytes 0 total >>= fun () ->
        loop ()
      in
      loop ()
    in
    let detach_safe_connection fd peer protocol =
      let id = X.identifier fd in
      let f () =
        Lwt.catch
          (fun () ->
            let t0 = Unix.gettimeofday() in
            Lwt_io.printlf "%f (%i) running protocol%!" t0 id  >>= fun () ->
            protocol fd
          )
          (fun ex ->
            let t1 = Unix.gettimeofday() in
            Lwt_io.printlf "%f (%i) end of protocol:%s%!" t1 id (Printexc.to_string ex)
            >>= fun () ->
            X.close fd)
      in
      Lwt.ignore_result (f ())
    in
    let rec loop () =
      let t0 = Unix.gettimeofday() in
      Lwt_io.printlf "%f pre_accept%!" t0 >>= fun () ->
      X.accept server_socket >>= fun (fd, peer) ->
      let t0 = Unix.gettimeofday() in
      Lwt_io.printlf "%f accepted ... detaching%!" t0 >>= fun () ->
      let () = detach_safe_connection fd peer protocol in
      loop ()
    in
    Lwt.finalize
      (fun () -> loop ())
      (fun () -> X.close server_socket)
  in
  Lwt_main.run t
    
let () =
  let () =
    let open Rsocket.Version in
    Printf.printf "ordma_version (%i,%i,%i,%s)\n" major minor patch git_revision
  in
  let engine = new Lwt_rsocket.rselect in
  Lwt_engine.set engine; 
  let () = Printf.printf "set engine to `rselect`\n%!" in
  let default_cmd =
    let copts_sect = "COMMON OPTIONS" in
    let copts_t = Term.pure "x" (* ?? *) in
    let doc = "test" in
    let man = [] in
    Term.(ret (pure (fun _ -> `Help (`Pager,None)) $copts_t)),
    Term.info "test" ~sdocs:copts_sect ~doc ~man
  in
  let host =
    Arg.(value
         & opt string "192.168.1.1"
         & info ["h";"host"] ~docv:"HOST" ~doc:"host"
    )
  in
  let port default =
    Arg.(value
         & opt int default
         & info ["p";"port"] ~docv:"PORT" ~doc:"port"
    )
  in
  let len default =
    Arg.(value
         & opt int default
         & info ["l";"length"] ~docv:"LENGTH" ~doc:"length of message"
    )
  in
  let buffer_size default =
    Arg.(value
         & opt int default
         & info ["b";"buffer-size"] ~docv:"BUFER_SIZE" ~doc:"size of server side buffer"
    )
  in
  let n =
    Arg.(value
         & opt int 10
         & info ["n"] ~docv:"N" ~doc:"number of iterations"
    )
  in
  let client_cmd =
    let t = Term.(pure client
                  $ host
                  $ port 10_000
                  $ len 4096
                  $ n)
    in
    let info =
      Term.info "client" ~doc:"run echo client"
    in
    t,info
  in
  let server_cmd =
    let t = Term.(pure server
                  $ host
                  $ port 10_000
                  $ buffer_size 4096
            )
    in
    let info =
      Term.info "server" ~doc:"start echo server"
    in
    t,info
  in
  let cmds =
    [client_cmd;
     server_cmd;
    ]
  in
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0

