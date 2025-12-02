open Core

let parse_ranges line =
  String.split_on_chars line ~on:[ ',' ]
  |> List.map ~f:(String.split_on_chars ~on:[ '-' ])
  |> List.map ~f:(function
       | [ low; high ] -> (Int.of_string low, Int.of_string high)
       | _ -> failwith "Invalid range")

let rec is_magic_number ?(repeat_pattern = 2) str =
  if repeat_pattern > String.length str then false
  else
    let chunk_size = String.length str / repeat_pattern in
    if chunk_size = 0 then false
    else
      String.to_list str
      |> List.chunks_of ~length:chunk_size
      |> List.stable_dedup |> List.length = 1
      || is_magic_number ~repeat_pattern:(repeat_pattern + 1) str

let () =
  match In_channel.(input_line stdin) with
  | None -> failwith "No input"
  | Some line ->
      let ranges = parse_ranges line in
      let result =
        List.fold ranges ~init:0 ~f:(fun acc (low, high) ->
            List.range low (high + 1)
            |> List.fold ~init:acc ~f:(fun acc n ->
                   if is_magic_number (Int.to_string n) then n + acc else acc))
      in
      printf "Result: %d\n" result
