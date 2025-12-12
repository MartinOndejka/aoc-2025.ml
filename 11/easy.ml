open Core
module H = Hashtbl.Make (String)

let rec parse_input ?(h = H.create ()) () =
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
          H.set h ~key:device ~data:outputs;
          parse_input ~h ()
      | _ -> failwith "Invalid input")

let rec dfs h = function
  | "out" -> 1
  | device ->
      let outputs = H.find_exn h device in
      List.fold outputs ~init:0 ~f:(fun acc output -> acc + dfs h output)

let () =
  let h = parse_input () in
  let result = dfs h "you" in
  printf !"Result: %d\n" result
