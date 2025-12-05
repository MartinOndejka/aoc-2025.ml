open Core

let rec parse_ranges () =
  match In_channel.(input_line stdin) with
  | None -> []
  | Some line ->
      if String.(line = "") then []
      else
        let range =
          match String.split_on_chars line ~on:[ '-' ] with
          | [ low; high ] -> (Int.of_string low, Int.of_string high)
          | _ -> failwith "Invalid range"
        in
        range :: parse_ranges ()

let rec parse_ingredients () =
  match In_channel.(input_line stdin) with
  | None -> []
  | Some line -> Int.of_string line :: parse_ingredients ()

let is_in_range ingredient ranges =
  List.exists ranges ~f:(fun (low, high) ->
      low <= ingredient && ingredient <= high)

let () =
  let ranges = parse_ranges () in
  let ingredients = parse_ingredients () in
  let result =
    List.count ingredients ~f:(fun ingredient -> is_in_range ingredient ranges)
  in
  printf !"Result: %d\n" result
