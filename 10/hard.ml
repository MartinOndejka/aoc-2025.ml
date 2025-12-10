open Core

module Button = struct
  type t = int list [@@deriving sexp]

  let rec parse_buttons = function
    | [] -> ([], [])
    | str :: rest ->
        if String.is_prefix str ~prefix:"(" then
          let button : int list =
            String.filter str ~f:(fun c -> Char.(c <> '(' && c <> ')'))
            |> String.split ~on:',' |> List.map ~f:Int.of_string
          in
          let buttons, rest = parse_buttons rest in
          (button :: buttons, rest)
        else ([], str :: rest)
end

module Joltage = struct
  type t = int array [@@deriving sexp]

  let parse line =
    String.filter line ~f:(fun c -> Char.(c <> '{' && c <> '}'))
    |> String.split ~on:',' |> List.map ~f:Int.of_string |> Array.of_list
end

type problem = Joltage.t * Button.t list

let rec parse_input () : problem list =
  match In_channel.(input_line stdin) with
  | None -> []
  | Some line -> (
      match String.split line ~on:' ' with
      | _light :: rest ->
          let buttons, joltage = Button.parse_buttons rest in
          let joltage =
            match joltage with
            | [ str ] -> Joltage.parse str
            | _ -> failwith "Invalid input"
          in
          (joltage, buttons) :: parse_input ()
      | _ -> failwith "Invalid input")

let solve ((target_joltage, buttons) : problem) =
  let open Z3 in
  let ctx = Z3.mk_context [] in

  let bs =
    List.mapi buttons ~f:(fun i _ ->
        Arithmetic.Integer.mk_const_s ctx (Printf.sprintf "b%d" i))
    |> Array.of_list
  in

  let constraints =
    Array.to_list target_joltage
    |> List.mapi ~f:(fun j t ->
           let t = Arithmetic.Integer.mk_numeral_i ctx t in
           let lhs =
             List.filter_mapi buttons ~f:(fun i button ->
                 if List.mem button j ~equal:Int.equal then Some bs.(i)
                 else None)
             |> Arithmetic.mk_add ctx
           in
           Boolean.mk_eq ctx lhs t)
  in

  let positive_constraints =
    Array.to_list bs
    |> List.map ~f:(fun b ->
           Arithmetic.mk_ge ctx b (Arithmetic.Integer.mk_numeral_i ctx 0))
  in

  let o = Optimize.mk_opt ctx in
  Optimize.add o constraints;
  Optimize.add o positive_constraints;

  let sum = Arithmetic.mk_add ctx (Array.to_list bs) in
  let _handle = Optimize.minimize o sum in

  match Optimize.check o with
  | Solver.SATISFIABLE ->
      let m = Optimize.get_model o |> Option.value_exn in
      Model.eval m sum true |> Option.value_exn |> Expr.to_string
      |> Int.of_string
  | Solver.UNSATISFIABLE -> failwith "UNSATISFIABLE"
  | Solver.UNKNOWN -> failwith "UNKNOWN"

let () =
  let input = parse_input () in
  let result = List.map input ~f:solve |> List.fold ~init:0 ~f:( + ) in
  printf !"Result: %d\n" result
