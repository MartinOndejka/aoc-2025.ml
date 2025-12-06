open Core

type operator = int -> int -> int [@@deriving sexp]

let fst_char s = String.get s 0

let rec pair_numbers_and_operators numbers = function
  | [] -> []
  | operator :: rest_operators ->
      let assignment, rest_assignments =
        List.split_while numbers ~f:(fun s -> String.(equal "" s |> not))
      in
      (operator, List.map assignment ~f:Int.of_string)
      :: pair_numbers_and_operators
           (List.tl rest_assignments |> Option.value ~default:[])
           rest_operators

let parse_input () : (operator * int list) list =
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
        (String.to_list line :: numbers, operators)
  in
  let numbers, operators =
    parse_rows ()
    |> Tuple.T2.map_fst ~f:(fun x ->
           List.transpose x
           |> Option.value_exn ~message:"Invalid matrix"
           |> List.map ~f:String.of_char_list
           |> List.map ~f:String.strip)
  in
  pair_numbers_and_operators numbers operators

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
