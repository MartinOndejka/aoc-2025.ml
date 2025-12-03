open Core

let c_to_i c =
  if Char.is_digit c then Some (Char.to_int c - Char.to_int '0') else None

let i_to_c i = Char.of_int_exn (i + Char.to_int '0')
let parse_bank line = String.to_list line |> List.filter_map ~f:c_to_i

let rec find_best_battery n bank =
  match n with
  | 0 -> []
  | n ->
      let place_to_look_for = List.slice bank 0 (List.length bank - (n - 1)) in
      let i, battery =
        List.foldi place_to_look_for ~init:(0, 0) ~f:(fun i (acc_i, acc_b) b ->
            if b > acc_b then (i, b) else (acc_i, acc_b))
      in
      battery
      :: find_best_battery (n - 1) (List.slice bank (i + 1) (List.length bank))

let rec solve_bank acc =
  match In_channel.(input_line stdin) with
  | None -> acc
  | Some line ->
      let bank = parse_bank line in
      let best_batteries = find_best_battery 12 bank in
      let best_batteries =
        List.map best_batteries ~f:i_to_c
        |> String.of_char_list |> Int.of_string
      in
      printf "Best batteries: %d\n" best_batteries;
      solve_bank (acc + best_batteries)

let () =
  let result = solve_bank 0 in
  printf "Result: %d\n" result
