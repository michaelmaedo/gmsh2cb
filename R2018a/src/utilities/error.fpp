use error_mod, only: set_error_status, set_error_msg, NO_ERROR

#define RAISE(info, err) call set_error_status(info, err); return

#define EXCEPT(msg, info, err) if (err % status == info) then; call set_error_msg(msg, err); return; end if