open Core

let c_to_i c =
  if Char.is_digit c then Some (Char.to_int c - Char.to_int '0') else None

let parse_bank line = String.to_list line |> List.filter_map ~f:c_to_i

let find_best_batteries bank =
  List.foldi bank ~init:0 ~f:(fun index acc i ->
      max acc
        (List.slice bank (index + 1) (List.length bank)
        |> List.fold ~init:acc ~f:(fun acc j ->
               let n = (i * 10) + j in
               max acc n)))

let rec solve_bank acc =
  match In_channel.(input_line stdin) with
  | None -> acc
  | Some line ->
      let bank = parse_bank line in
      let best_batteries = find_best_batteries bank in
      printf "Best batteries: %d\n" best_batteries;
      solve_bank (acc + best_batteries)

let () =
  let result = solve_bank 0 in
  printf "Result: %d\n" result
