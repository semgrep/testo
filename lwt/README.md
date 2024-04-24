`Testo_lwt`
===========

This is the implementation of Testo that supports deferred
computations in the form of Lwt promises.

Instead of using functors like Alcotest does, we share the generic
code via symbolic links. Only the `Monad` module has its own
implementation. Most of the other modules depend on it.

As a result, each time an ml or mli file is added to the `/core`
folder, a symlink must be created here.
