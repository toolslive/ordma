type rsocket
val socket     : Unix.socket_domain -> Unix.socket_type -> int -> rsocket
                                                                 
(* val show    : rsocket -> string *)
val connect    : rsocket -> Unix.sockaddr -> unit Lwt.t
val close      : rsocket -> unit Lwt.t
val bind       : rsocket -> Unix.sockaddr -> unit
val setsockopt : rsocket -> Unix.socket_bool_option -> bool -> unit
val listen     : rsocket -> int -> unit
val recv       : rsocket -> bytes -> int -> int -> Unix.msg_flag list -> int Lwt.t
val send       : rsocket -> bytes -> int -> int -> Unix.msg_flag list -> int Lwt.t
val accept     : rsocket -> (rsocket * Unix.sockaddr ) Lwt.t

open Bigarray
module Bytes : sig
  type t =  (char, int8_unsigned_elt, c_layout) Array1.t
  val create : int -> t
  val send : rsocket ->  t -> int -> int -> Unix.msg_flag list -> int Lwt.t
  val recv : rsocket ->  t -> int -> int -> Unix.msg_flag list -> int Lwt.t
end


