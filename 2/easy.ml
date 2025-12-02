open Core

let parse_ranges line =
  String.split_on_chars line ~on:[ ',' ]
  |> List.map ~f:(String.split_on_chars ~on:[ '-' ])
  |> List.map ~f:(function
       | [ low; high ] -> (Int.of_string low, Int.of_string high)
       | _ -> failwith "Invalid range")

let make_mirror n =
  let str = Int.to_string n in
  String.concat [ str; str ] |> Int.of_string

let split_mirror n =
  let str = Int.to_string n in
  let len = String.length str in
  let half = len / 2 in
  let a = String.sub str ~pos:0 ~len:half in
  let b = String.sub str ~pos:half ~len:(len - half) in
  let int_of_str = function "" -> 0 | s -> Int.of_string s in
  (int_of_str a, int_of_str b)

let check_if_in_ranges ranges n =
  List.exists ranges ~f:(fun (low, high) -> n >= low && n <= high)

let () =
  match In_channel.(input_line stdin) with
  | None -> failwith "No input"
  | Some line ->
      let ranges = parse_ranges line in
      let min =
        List.map ranges ~f:fst
        |> List.min_elt ~compare:Int.compare
        |> Option.value_exn ~message:"No minimum found"
        |> split_mirror
        |> fun (a, b) -> min a b
      in
      let max =
        List.map ranges ~f:snd
        |> List.max_elt ~compare:Int.compare
        |> Option.value_exn ~message:"No maximum found"
        |> split_mirror
        |> fun (a, b) -> max a b
      in
      let result =
        List.range min max
        |> List.filter ~f:(fun n -> check_if_in_ranges ranges (make_mirror n))
      in
      printf !"Result: %{sexp: int list}\n" result;
      printf "Result: %d\n"
        (List.fold result ~init:0 ~f:(fun acc n -> acc + make_mirror n))
