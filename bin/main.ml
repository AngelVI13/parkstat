open Core

(* NOTE: taken from here:
    https://github.com/mattlianje/layoutz/tree/master/layoutz-ocaml
*)
open! Layoutz

let update_interval = 100 * 1000

let scp_file ~remote ~local =
  let script =
    "eval \"$(keychain --eval /home/angel/.ssh/id_ed25519)\" && "
    ^ "exec scp \"$1\" \"$2\""
  in
  let prog = "/bin/sh" in
  let args = [ "-c"; script; remote; local ] in
  let s =
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
    | Ok (), "", "" -> "Success"
    | Ok (), stdout, "" ->
        let s_args = [%sexp (args : string list)] in
        sprintf "unexpected output: `%s %s`\nstdout:%s" prog
          (Sexp.to_string_hum s_args)
          stdout
    | ((Ok () | Error _) as result), stdout, stderr ->
        let s_args = [%sexp (args : string list)] in
        let s_error = [%sexp (result : unit Or_error.t)] in
        sprintf "error when running: `%s %s\nstdout:%s\nstderr:%s\nerr:%s" prog
          (Sexp.to_string_hum s_args)
          stdout stderr
          (Sexp.to_string_hum s_error)
    | exception exn ->
        let s_args = [%sexp (args : string list)] in
        let s_exc = [%sexp (exn : Exn.t)] in
        sprintf "failed to run: `%s %s\nexc:%s" prog
          (Sexp.to_string_hum s_args)
          (Sexp.to_string_hum s_exc)
  in
  printf "Command '%s' status: %s" prog s

(* match Core_unix.waitpid process.pid with *)
(* | Ok _ -> () *)
(* | Error e -> *)
(*     failwith *)
(*       (sprintf "scp failed: %s" *)
(*          (Core_unix.Exit_or_signal.sexp_of_error e |> Sexp.to_string_hum)) *)

let create_timestamp () =
  let now = Time_ns_unix.now () in
  Time_ns_unix.format now "%Y-%m-%d %H:%M:%S" ~zone:Time_ns_unix.Zone.utc

module Data = struct
  type t = { last_updated : string }

  let create () = { last_updated = create_timestamp () }

  let refresh t =
    let _ = t in
    let local = "/tmp/parking.json" in
    scp_file
      ~remote:
        "tmt@172.20.2.200:/home/tmt/Documents/slack/slack-bot/parking.json"
      ~local;

    { last_updated = create_timestamp () }
end

(* type msg = Refresh | Exit *)
(* let counter_app = *)
(*   { *)
(*     init = (Data.create (), CmdNone); *)
(*     update = *)
(*       (fun msg data -> *)
(*         match msg with *)
(*         | Refresh -> (Data.refresh data, CmdNone) *)
(*         | Exit -> (data, CmdExit)); *)
(*     subscriptions = *)
(*       (fun _ -> *)
(*         sub_batch *)
(*           [ *)
(*             sub_key_press (fun key -> *)
(*                 match key with *)
(*                 | KeyChar 'r' -> Some Refresh *)
(*                 | KeyChar 'q' -> Some Exit *)
(*                 | _ -> None); *)
(*             sub_every_ms update_interval Refresh; *)
(*           ]); *)
(*     view = *)
(*       (fun data -> *)
(*         layout *)
(*           [ *)
(*             center (row [ s "Parking Status" |> styleBold ]); *)
(*             br; *)
(*             row *)
(*               [ *)
(*                 statusCard ~label:(s "Last Updated") *)
(*                   ~content:(s (sprintf "%s" data.last_updated)); *)
(*               ]; *)
(*           ]); *)
(*   } *)
(* let () = run_app counter_app *)

let () =
  let _ = update_interval in
  let d = Data.create () in
  let _ = d.last_updated in
  ignore (Data.refresh d)
