open Core

let rec parse_ranges () =
  match In_channel.(input_line stdin) with
  | None -> failwith "Invalid input"
  | Some "" -> []
  | Some line ->
      let range =
        match String.split_on_chars line ~on:[ '-' ] with
        | [ low; high ] -> (Int.of_string low, Int.of_string high)
        | _ -> failwith "Invalid range"
      in
      range :: parse_ranges ()

let merge_2_ranges (a, b) (c, d) =
  if b < c then `No_overlap
  else if a > d then `No_overlap
  else `Overlap (min a c, max b d)

let rec merge_all_ranges = function
  | first :: second :: rest -> (
      match merge_2_ranges first second with
      | `Overlap r -> merge_all_ranges (r :: rest)
      | `No_overlap -> first :: merge_all_ranges (second :: rest))
  | ranges -> ranges

let () =
  let result =
    parse_ranges ()
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> merge_all_ranges
    |> List.fold ~init:0 ~f:(fun acc (l, h) -> acc + (h - l + 1))
  in
  printf !"Result: %d\n" result
