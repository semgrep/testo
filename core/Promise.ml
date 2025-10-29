(*
   Extension of the Monad module with generic utilities.
*)

include Monad

module Operators = struct
  let ( >>= ) = bind
end

open Operators

let catch_result func =
  catch
    (fun () -> func () >>= fun res -> return (Ok res))
    (fun e trace -> return (Error (e, trace)))

let protect func ~finally =
  let safe_finally () =
    catch finally (fun exn trace ->
        Testo_util.Error.user_error
          (Printf.sprintf
             "Internal error in test framework: exception raised by 'finally': \
              %s\n\
              %s\n"
             (Printexc.to_string exn)
             (Printexc.raw_backtrace_to_string trace)))
  in
  catch
    (fun () ->
      func () >>= fun res ->
      safe_finally () >>= fun () -> return res)
    (fun exn trace ->
      safe_finally () >>= fun () -> Printexc.raise_with_backtrace exn trace)
