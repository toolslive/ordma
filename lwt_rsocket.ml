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
  let lwt_fd = Lwt_unix.of_unix_file_descr unix_fd in
  (lwt_fd : lwt_rsocket)
    
let socket domain typ proto =
  let rsocket = Rsocket.rsocket domain typ proto in
  wrap rsocket
  
let show lwt_rsocket =
  let rs = rsocket_of lwt_rsocket in
  Rsocket.show rs

open Lwt.Infix
       
let connect lwt_rsocket socketaddr =
  let id = identifier lwt_rsocket in
  Lwt_log.debug_f "(%i) connect" id >>= fun () ->
  let rsocket = rsocket_of lwt_rsocket in
  Lwt_preemptive.detach
    (fun () ->
      Rsocket.rconnect rsocket socketaddr)
    ()

let close lwt_rsocket =
  let id = identifier lwt_rsocket in
  Lwt_log.debug_f "(%i) close" id >>= fun () ->
  Lwt.finalize
    (fun () ->
      Lwt_unix.close lwt_rsocket)
    (fun () ->
      Lwt.catch
        (fun () ->
          let rsocket = rsocket_of lwt_rsocket in
          Lwt_preemptive.detach Rsocket.rclose rsocket
        )
        (fun exn -> Lwt_log.debug_f ~exn "(ignoring) on close (%i)" id)
    )
  

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
  let lwt_fd = Lwt_unix.of_unix_file_descr unix_fd in
  if offset < 0
     || len < 0
     || offset > Bytes.length buffer - len
  then
    invalid_arg "Lwt_socket.recv"
  else
    Lwt_unix.wrap_syscall
      Lwt_unix.Read
      lwt_fd
      (fun () -> _ordma_lwt_unix_recv unix_fd buffer offset len flags)
    >>= fun r ->
    (* TODO: This is a symptom of something else? *)
    if r = 0
    then Lwt.fail End_of_file
    else Lwt.return r

  
let accept lwt_rsocket =
  Lwt_preemptive.detach
    (fun () ->
      let rsocket = rsocket_of lwt_rsocket in
      let client_s,client_addr = Rsocket.raccept rsocket in
      let client_rs = wrap client_s in
      client_rs,client_addr
    )
    ()

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
      let id = identifier lwt_rsocket in
      Lwt_log.debug_f "(%i) Lwt_rsocket.Bytes.send (%i bytes)" id len >>= fun () ->
      let unix_fd = unix_fd_of lwt_rsocket in
      Lwt_unix.wrap_syscall
        Lwt_unix.Write
        lwt_rsocket
        (fun () -> _ordma_lwt_unix_bytes_send unix_fd t pos len flags)

  let recv lwt_rsocket buffer offset len flags =
    let id = identifier lwt_rsocket in
    Lwt_log.debug_f "(%i) Lwt_rsocket.Bytes.recv on rsocket" id >>= fun () ->
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
    let fds2s fds =
      let i_of fd : int = Obj.magic fd in
      let s_of fd = i_of fd |> string_of_int in
      String.concat ";" (List.map s_of fds)
    in
    let () = Lwt_log.ign_debug_f
               "rselect.select: ([%s],[%s],%f) begin\n%!"
               (fds2s fds_r) (fds2s fds_w) timeout
    in
    let r = _rselect_select fds_r fds_w [] timeout in
    let rr,rw = r in
    let () = Lwt_log.ign_debug_f
               "rselect.select => ([%s],[%s]) end\n%!"
               (fds2s rr) (fds2s rw)
    in
    r
end
