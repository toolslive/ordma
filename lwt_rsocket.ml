type rsocket = Rsocket.rsocket
let socket domain typ proto = Rsocket.rsocket domain typ proto
let show rsocket = Rsocket.show rsocket

let connect rsocket socketaddr =
  Lwt_preemptive.detach
    (fun () ->
      Rsocket.rconnect rsocket socketaddr)
    ()

let close rsocket =
  Lwt_preemptive.detach Rsocket.rclose rsocket

let bind rsocket socketaddr = Rsocket.rbind rsocket socketaddr

let listen rsocket n = Rsocket.rlisten rsocket n
                                       
let setsockopt rsocket sockopt value = Rsocket.rsetsockopt rsocket sockopt value

let send rsocket buffer offset len flags =
  Lwt_preemptive.detach
    (fun () ->
      Rsocket.rsend rsocket buffer offset len flags
    )
    ()

let recv rsocket buffer offset len flags =
  Lwt_preemptive.detach
    (fun () ->
      Rsocket.rrecv rsocket buffer offset len flags
    )
    ()

let accept rsocket =
  Lwt_preemptive.detach
    (fun () ->
      Rsocket.raccept rsocket
    )
    ()


module Bytes = struct
  open Bigarray
  type t =  (char, int8_unsigned_elt, c_layout) Array1.t

  let create len =
    Bigarray.Array1.create Bigarray.Char Bigarray.C_layout len
                           
  let send rsocket t off len flags =
    Lwt_preemptive.detach
      (fun () -> Rsocket.rsend_ba rsocket t off len flags)
      ()

  let recv rsocket t off len flags =
    Lwt_preemptive.detach
      (fun () -> Rsocket.rrecv_ba rsocket t off len flags)
      ()
 
end
