open Core

let init = 50
let clamp t = t mod 100
let l t n = t - n |> clamp
let r t n = t + n |> clamp

let rec step state count =
  match In_channel.(input_line stdin) with
  | Some line ->
      let step_fn = if Char.(line.[0] = 'L') then l else r in
      let new_state =
        step_fn state (String.drop_prefix line 1 |> Int.of_string)
      in
      step new_state (if new_state = 0 then count + 1 else count)
  | None -> count

let () =
  let result = step init 0 in
  print_endline (Int.to_string result)
