
                     
type lwt_rsocket = Lwt_unix.file_descr

let unix_fd_of lwt_rsocket = Lwt_unix.unix_file_descr lwt_rsocket
                                                      
let rsocket_of lwt_rsocket =
  let u = unix_fd_of lwt_rsocket in
  let (r : Rsocket.rsocket) = Obj.magic u  in
  r

let identifier lwt_rsocket =
  let u = unix_fd_of lwt_rsocket in
  let (i:int) = Obj.magic u in
  i
    
let wrap rsocket =
  let (unix_fd : Unix.file_descr) = Obj.magic rsocket in
  let lwt_fd = Lwt_unix.of_unix_file_descr unix_fd ~blocking:false in
  (lwt_fd : lwt_rsocket)
    
let socket domain typ proto =
  let rsocket = Rsocket.rsocket domain typ proto in
  let () = Rsocket.set_nonblock rsocket in
  wrap rsocket
  
let show lwt_rsocket =
  let rs = rsocket_of lwt_rsocket in
  Rsocket.show rs

open Lwt.Infix
       
let connect lwt_rsocket socketaddr =
  (* from Lwt_unix.ml : *)
  let id = identifier lwt_rsocket in
  Lwt_log.debug_f "(%i) connect" id >>= fun () ->
  let in_progress = ref false in
  let rsocket = rsocket_of lwt_rsocket in
  Lwt_unix.wrap_syscall
    Lwt_unix.Write
    lwt_rsocket
    begin fun () ->
    if !in_progress
    then
      (* If the connection is in progress, [getsockopt_error] tells
           wether it succceed: *)
      match Rsocket.rgetsockopt_error rsocket with
      | None ->
         (* The socket is connected *)
         ()
      | Some err ->
         (* An error happened: *)
         raise (Unix.Unix_error(err, "connect", ""))
    else
      try
        (* We should pass only one time here, unless the system call
             is interrupted by a signal: *)
        Rsocket.rconnect rsocket socketaddr
      with
      | Unix.Unix_error (Unix.EINPROGRESS, _, _) ->
         in_progress := true;
         raise Lwt_unix.Retry
    end

type state = Lwt_unix.state = Opened | Closed | Aborted of exn 


(* this is a copy of Lwt_unix.file_descr *)
type rfile_descr = (* Lwt_unix.file_descr = *) {
  fd : Unix.file_descr;

  mutable state: state;
  mutable set_flags : bool;
  mutable blocking : bool Lwt.t Lazy.t;
  mutable event_readable : Lwt_engine.event option;
  mutable event_writable : Lwt_engine.event option;
  hooks_readable : (unit -> unit) Lwt_sequence.t;
  hooks_writable : (unit -> unit) Lwt_sequence.t;
  }
                                                 
let set_state rch state =
    rch.state <- Closed

let rec check_descriptor rch =
  match rch.state with
    | Opened ->
        ()
    | Aborted e ->
        raise e
    | Closed ->
       raise (Unix.Unix_error (Unix.EBADF, "check_descriptor", ""))

let clear_events rch =
  Lwt_sequence.iter_node_l (fun node -> Lwt_sequence.remove node; Lwt_sequence.get node ()) rch.hooks_readable;
  Lwt_sequence.iter_node_l (fun node -> Lwt_sequence.remove node; Lwt_sequence.get node ()) rch.hooks_writable;
  begin
    match rch.event_readable with
      | Some ev ->
         rch.event_readable <- None;
         Lwt_engine.stop_event ev
      | None ->
          ()
  end;
  begin
    match rch.event_writable with
      | Some ev ->
         rch.event_writable <- None;
         Lwt_engine.stop_event ev
      | None ->
         ()
  end
 
let close lwt_rsocket =
  let (rch : rfile_descr) = Obj.magic lwt_rsocket in
  if rch.state = Closed then check_descriptor rch;
  set_state rch Closed;
  clear_events rch;
  (* TODO: via job *)
  Lwt.return (Rsocket.rclose (rsocket_of lwt_rsocket))

(* the original hack:
  let id = identifier lwt_rsocket in
  Lwt.finalize
    (fun () ->
      Lwt_unix.close lwt_rsocket)
    (fun () ->
      let () =try 
        let rsocket = rsocket_of lwt_rsocket in
        Rsocket.rclose rsocket
      with
        exn  -> Lwt_log.ign_debug_f ~exn "(ignoring) on close (%i)" id
      in
      Lwt.return_unit
    )
   *)
  

let bind lwt_rsocket socketaddr =
  let rsocket = rsocket_of lwt_rsocket in
  Rsocket.rbind rsocket socketaddr

let listen lwt_rsocket n =
  let rsocket = rsocket_of lwt_rsocket in
  Rsocket.rlisten rsocket n
                                       
let setsockopt lwt_rsocket sockopt value =
  let rsocket = rsocket_of lwt_rsocket in
  Rsocket.rsetsockopt rsocket sockopt value

let send lwt_rsocket buffer offset len flags =
  let rsocket = rsocket_of lwt_rsocket in
  Lwt_preemptive.detach
    (fun () ->
      Rsocket.rsend rsocket buffer offset len flags
    )
    ()



external _ordma_lwt_unix_recv :
  Unix.file_descr ->
  Bytes.t -> int -> int -> Unix.msg_flag list -> int =
  "ordma_lwt_unix_recv"

external _ordma_lwt_unix_send :
  Unix.file_descr -> Bytes.t
  -> int -> int -> Unix.msg_flag list -> int =
  "ordma_lwt_unix_send"

  
    
let recv (lwt_rsocket: lwt_rsocket) buffer offset len flags =
  let unix_fd = unix_fd_of lwt_rsocket in
  if offset < 0
     || len < 0
     || offset > Bytes.length buffer - len
  then
    invalid_arg "Lwt_socket.recv"
  else
    Lwt_unix.wrap_syscall
      Lwt_unix.Read
      lwt_rsocket
      (fun () -> _ordma_lwt_unix_recv unix_fd buffer offset len flags)
    >>= fun r ->
    (* TODO: This is a symptom of something else? *)
    if r = 0
    then Lwt.fail End_of_file
    else Lwt.return r

  
let accept lwt_rsocket =
  (* 
     (* from Lwt_unix.ml :  *)
     let accept ch =
     wrap_syscall Read ch (fun _ -> let (fd, addr) = 
     Unix.accept ch.fd in (mk_ch ~blocking:false fd, addr)) 
   *)
  (*
  Lwt_unix.wrap_syscall
    Lwt_unix.Read
    lwt_rsocket
    (fun () -> 
       let (fd, addr) = Rsocket.raccept (rsocket_of lwt_rsocket) in
       let (ufd:Unix.file_descr) = Obj.magic fd in
       (Lwt_unix.of_unix_file_descr ~blocking:false ufd, addr))
   *)
  Lwt_unix.wait_read lwt_rsocket >>= fun () ->
  let rsocket = rsocket_of lwt_rsocket in
  let client_s,client_addr = Rsocket.raccept rsocket in
  let client_rs = wrap client_s in
  let r =  client_rs,client_addr in
  Lwt.return r
  (*
  Lwt_preemptive.detach
    (fun () ->
      let rsocket = rsocket_of lwt_rsocket in
      let client_s,client_addr = Rsocket.raccept rsocket in
      let client_rs = wrap client_s in
      client_rs,client_addr
    )
    ()
   *)
   
    
open Bigarray

type ba = (char, int8_unsigned_elt, c_layout) Array1.t
                                              
external _ordma_lwt_unix_bytes_recv:
  Unix.file_descr ->
  ba -> int -> int -> Unix.msg_flag list -> int =
  "ordma_lwt_unix_bytes_recv"

external _ordma_lwt_unix_bytes_send :
  Unix.file_descr -> ba -> int -> int -> Unix.msg_flag list -> int =
  "ordma_lwt_unix_bytes_send"

module Bytes = struct
  
  type t = ba

  let create len =
    Array1.create Char C_layout len
                           
  let send lwt_rsocket t  pos len flags =
    if pos < 0
       || len < 0
       || pos > Array1.dim t - len
    then
    invalid_arg "send"
    else
      (*let id = identifier lwt_rsocket in*)
      let unix_fd = unix_fd_of lwt_rsocket in
      Lwt_unix.wrap_syscall
        Lwt_unix.Write
        lwt_rsocket
        (fun () -> _ordma_lwt_unix_bytes_send unix_fd t pos len flags)

  let recv lwt_rsocket buffer offset len flags =
    (*let id = identifier lwt_rsocket in*)
    if offset < 0
       || len < 0
       || offset > Array1.dim buffer - len
    then
      invalid_arg "Lwt_rsocket.recv"
    else
      let unix_fd = unix_fd_of lwt_rsocket in
      Lwt_unix.wrap_syscall
        Lwt_unix.Read
        lwt_rsocket
        (fun () -> _ordma_lwt_unix_bytes_recv unix_fd buffer offset len flags)
      >>= fun r ->
      if r = 0
      then Lwt.fail End_of_file
      else Lwt.return r
 
end

open Lwt_engine
class rpoll = object(self)
  inherit poll_based
  method private poll : 
                   (Unix.file_descr * bool * bool) list ->
                   float -> (Unix.file_descr * bool * bool) list
    =
    failwith "todo: rpoll.poll"
end

external _rselect_select :
  Unix.file_descr list ->
  Unix.file_descr list ->
  Unix.file_descr list ->
  float ->
  Unix.file_descr list * Unix.file_descr list = "ordma_rselect_select"
                                         
class rselect = object
  inherit select_based
  method private 
      select : 
        Unix.file_descr list ->
        Unix.file_descr list ->
        float -> Unix.file_descr list * Unix.file_descr list
    = fun fds_r fds_w timeout ->
    (*let fds2s fds =
      let i_of fd : int = Obj.magic fd in
      let s_of fd = i_of fd |> string_of_int in
      String.concat ";" (List.map s_of fds)
    in*)
    (*let () = Printf.eprintf
               "rselect.select: ([%s],[%s],%f) begin\n%!"
               (fds2s fds_r) (fds2s fds_w) timeout
    in*)
    let r = _rselect_select fds_r fds_w [] timeout in
    (*
    let rr,rw = r in
    let () = Printf.eprintf
               "rselect.select => ([%s],[%s]) end\n%!"
               (fds2s rr) (fds2s rw)
    in*)
    r
end
