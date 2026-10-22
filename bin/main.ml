open Core

(* NOTE: taken from here:
    https://github.com/mattlianje/layoutz/tree/master/layoutz-ocaml
*)
open Layoutz

let update_interval = 10 * 1000

let create_timestamp () =
  let now = Time_ns_unix.now () in
  Time_ns_unix.format now "%Y-%m-%d %H:%M:%S" ~zone:Time_ns_unix.Zone.utc

module Data = struct
  type t = { last_updated : string }

  let create () = { last_updated = create_timestamp () }

  let refresh t =
    let _ = t in
    { last_updated = create_timestamp () }
end

type msg = Refresh | Exit

let counter_app =
  {
    init = (Data.create (), CmdNone);
    update =
      (fun msg data ->
        match msg with
        | Refresh -> (Data.refresh data, CmdNone)
        | Exit -> (data, CmdExit));
    subscriptions =
      (fun _ ->
        sub_batch
          [
            sub_key_press (fun key ->
                match key with
                | KeyChar 'r' -> Some Refresh
                | KeyChar 'q' -> Some Exit
                | _ -> None);
            sub_every_ms update_interval Refresh;
          ]);
    view =
      (fun data ->
        layout
          [
            center (row [ s "Parking Status" |> styleBold ]);
            br;
            row
              [
                statusCard ~label:(s "Last Updated")
                  ~content:(s (sprintf "%s" data.last_updated));
              ];
          ]);
  }

let () = run_app counter_app
