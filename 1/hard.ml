open Core

let clamp n = ((n mod 100) + 100) mod 100

let rec step (old_state, count) =
  match In_channel.(input_line stdin) with
  | Some line ->
      let fn = if Char.(line.[0] = 'R') then ( + ) else ( - ) in
      let diff = String.drop_prefix line 1 |> Int.of_string in
      let new_state = fn old_state diff in
      let count_diff =
        let count_diff = new_state / 100 |> abs in
        if old_state = 0 then count_diff
        else if new_state < 0 then count_diff + 1
        else if new_state = 0 then 1
        else count_diff
      in
      step (clamp new_state, count + count_diff)
  | None -> count

let () =
  let result = step (50, 0) in
  print_endline (Int.to_string result)
