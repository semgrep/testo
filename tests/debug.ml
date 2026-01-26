set_binary_mode_out stdout true;
Testo.with_capture
  ~is_binary_mode:(fun _ -> true)
  stdout
  (fun () -> output_string stdout "hello\r\nworld\n")
