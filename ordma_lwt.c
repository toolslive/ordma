#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>

#include <caml/bigarray.h>

#include <rdma/rsocket.h>

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};


value ordma_lwt_unix_recv(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = rrecv(Int_val(fd),
              &Byte(String_val(buf), Long_val(ofs)),
              Long_val(len),
              convert_flag_list(flags, msg_flag_table)
              );
  if (ret == -1) {
    uerror("Lwt_rsocket.recv", Nothing);
  }
  return Val_int(ret);
}

value ordma_lwt_unix_bytes_recv(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = rrecv(Int_val(fd),
              (char*)Caml_ba_array_val(buf)->data + Long_val(ofs),
              Long_val(len),
              convert_flag_list(flags, msg_flag_table)
              );
  if (ret == -1) {
    uerror("Lwt_socket.Bytes.recv", Nothing);
  }
  return Val_int(ret);
}


value ordma_lwt_unix_send(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = rsend(Int_val(fd), &Byte(String_val(buf), Long_val(ofs)), Long_val(len),
             convert_flag_list(flags, msg_flag_table));
  if (ret == -1) {
    uerror("Lwt_socket.send", Nothing);
  }
  return Val_int(ret);
}

value ordma_lwt_unix_bytes_send(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = rsend(Int_val(fd), (char*)Caml_ba_array_val(buf)->data + Long_val(ofs), Long_val(len),
             convert_flag_list(flags, msg_flag_table));
  if (ret == -1) {
    uerror("Lwt_socket.Bytes.send", Nothing);
  }
  return Val_int(ret);
}

//TODO: ordma_close....
