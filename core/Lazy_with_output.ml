(*
   Lazy computations that capture and restore stdout/stderr

   A computation is a function that takes no arguments. During this
   computation, text may get written to stdout or stderr.

   The computation is performed at most once.
   It ends either by returning a value or by raising an exception.
   Either outcome is captured and stored along with the captured
   stdout and stderr output.

   Calling the 'force' function performs the computation only if it wasn't
   performed already (in the current process; there's no sharing
   across Testo workers).
*)

open Promise.Operators (* >>= *)

type redirect = Stdout_to_stderr | Stderr_to_stdout

type 'a uncomputed = {
  func: unit -> 'a Promise.t;
  redirect: redirect option;
}

type 'a computed = {
  result: ('a, exn * Printexc.raw_backtrace) result;
  stdout: string option;
  stderr: string option;
}

type 'a state =
  | Uncomputed of 'a uncomputed
  | Computed of 'a computed

type 'a t = 'a state ref

let create ?redirect func =
  ref (Uncomputed {func; redirect})

let restore_result (x : 'a computed) : 'a Promise.t =
  Option.iter (fun s -> print_string s; flush stdout) x.stdout;
  Option.iter (fun s -> prerr_string s; flush stderr) x.stderr;
  match x.result with
  | Ok x -> Promise.return x
  | Error (e, trace) ->
      (* Restore the same exception and the same stack trace for all calls
         to 'force'.*)
      Printexc.raise_with_backtrace e trace

let capture_result (x : 'a uncomputed) : 'a computed Promise.t =
  match x.redirect with
  | None ->
      Store.with_capture stderr
        (fun () -> Store.with_capture stdout
            (fun () -> Promise.catch_result x.func))
      >>= fun ((result, out), err) ->
      Promise.return { result; stdout = Some out; stderr = Some err }
  | Some Stderr_to_stdout ->
      Store.with_capture stdout
        (Store.with_redirect_fds ~from:[Unix.stderr] ~to_:Unix.stdout
           (fun () -> Promise.catch_result x.func))
      >>= fun (result, out) ->
      Promise.return { result; stdout = Some out; stderr = None }
  | Some Stdout_to_stderr ->
      Store.with_capture stderr
        (Store.with_redirect_fds ~from:[Unix.stdout] ~to_:Unix.stderr
           (fun () -> Promise.catch_result x.func))
      >>= fun (result, err) ->
      Promise.return { result; stdout = None; stderr = Some err }

let force (x : 'a t) : 'a Promise.t =
  match !x with
  | Computed computed -> restore_result computed
  | Uncomputed uncomputed ->
      capture_result uncomputed >>= fun computed ->
      x := Computed computed;
      restore_result computed
