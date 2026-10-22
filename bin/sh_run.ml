open Core

let run () ~prog ~args ~allow_output =
  match
    let { Core_unix.Process_info.pid; stdin = _; stdout; stderr } =
      Core_unix.create_process ~prog ~args
    in
    let output = Core_unix.in_channel_of_descr stdout in
    let error_output = Core_unix.in_channel_of_descr stderr in
    let result = Core_unix.waitpid pid in
    let output = In_channel.input_all output in
    let error_output = In_channel.input_all error_output in
    let result = Core_unix.Exit_or_signal.or_error result in
    (result, output, error_output)
  with
  | Ok (), "", "" -> Ok ()
  | Ok (), _stdout, "" when allow_output -> Ok ()
  | Ok (), stdout, "" ->
      Or_error.error_s
        [%message
          "unexpected output when running"
            (prog : string)
            (args : string list)
            (stdout : string)]
  | ((Ok () | Error _) as result), stdout, stderr ->
      Or_error.error_s
        [%message
          "error when running"
            (prog : string)
            (args : string list)
            (result : unit Or_error.t)
            (stdout : string)
            (stderr : string)]
  | exception exn ->
      Or_error.error_s
        [%message
          "failed to run" (prog : string) (args : string list) (exn : Exn.t)]
