open Core
module StringHTbl = Hashtbl.Make (String)

module SBBHtbl = Hashtbl.Make (struct
  type t = string * bool * bool [@@deriving sexp, compare, hash]
end)

let rec parse_input ?(h = StringHTbl.create ()) () =
  match In_channel.(input_line stdin) with
  | None -> h
  | Some line -> (
      match String.split line ~on:':' with
      | [ device; outputs ] ->
          let device = String.strip device in
          let outputs =
            String.strip outputs |> String.split ~on:' '
            |> List.map ~f:String.strip
          in
          StringHTbl.set h ~key:device ~data:outputs;
          parse_input ~h ()
      | _ -> failwith "Invalid input")

let rec dfs ?(dac_visited = false) ?(fft_visited = false)
    ?(d = SBBHtbl.create ()) h = function
  | "out" -> if dac_visited && fft_visited then 1 else 0
  | device ->
      let outputs = StringHTbl.find_exn h device in
      let dac_visited = dac_visited || String.equal device "dac" in
      let fft_visited = fft_visited || String.equal device "fft" in
      List.fold outputs ~init:0 ~f:(fun acc output ->
          let key = (output, dac_visited, fft_visited) in
          (if not (SBBHtbl.mem d key) then
           let result = dfs ~dac_visited ~fft_visited ~d h output in
           SBBHtbl.set d ~key ~data:result);
          acc + SBBHtbl.find_exn d key)

let () =
  let h = parse_input () in
  let result = dfs h "svr" in
  printf !"Result: %d\n" result
