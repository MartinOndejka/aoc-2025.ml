open Core

let fst_char s = String.get s 0

let parse_input () =
  let rec parse_rows () =
    match In_channel.(input_line stdin) with
    | None -> failwith "Invalid input"
    | Some line when Char.(fst_char line = '*' || fst_char line = '+') ->
        ( [],
          String.split_on_chars line ~on:[ ' ' ]
          |> List.filter_map ~f:(function
               | "+" -> Some ( + )
               | "*" -> Some ( * )
               | _ -> None) )
    | Some line ->
        let numbers, operators = parse_rows () in
        let row =
          String.split_on_chars line ~on:[ ' ' ]
          |> List.filter_map ~f:(fun s ->
                 try Some (Int.of_string s) with _ -> None)
        in
        (row :: numbers, operators)
  in
  let numbers_as_columns, operators = parse_rows () in
  match
    List.zip operators
      (List.transpose numbers_as_columns
      |> Option.value_exn ~message:"Invalid matrix")
  with
  | List.Or_unequal_lengths.Ok l -> l
  | Unequal_lengths -> failwith "Invalid input"

let () =
  let assignments = parse_input () in
  let result =
    List.fold assignments ~init:0 ~f:(fun acc (operator, numbers) ->
        let res =
          List.fold (List.tl_exn numbers) ~init:(List.hd_exn numbers)
            ~f:(fun acc n -> operator acc n)
        in
        acc + res)
  in
  printf "Result: %d\n" result
