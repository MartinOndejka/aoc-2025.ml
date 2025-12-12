open Core

let number_of_tiles = 6

let rec parse_input () =
  let rec parse_lines () =
    match In_channel.(input_line stdin) with
    | None -> []
    | Some line -> line :: parse_lines ()
  in
  let lines = parse_lines () in
  let tiles_raw, grids_raw = List.split_n lines (5 * number_of_tiles) in
  let tiles =
    List.take tiles_raw (5 * number_of_tiles)
    |> List.chunks_of ~length:5 |> List.map ~f:List.tl_exn
    |> List.map ~f:(Fn.flip List.take 3)
    |> List.map ~f:(fun tile ->
           List.fold tile ~init:0 ~f:(fun acc row ->
               List.fold (String.to_list row) ~init:acc ~f:(fun acc cell ->
                   if Char.(cell = '#') then acc + 1 else acc)))
    |> Array.of_list
  in
  assert (Array.length tiles = number_of_tiles);
  let grids =
    List.map grids_raw ~f:(fun grid ->
        match String.split grid ~on:':' with
        | [ size_raw; grid_raw ] ->
            let size =
              match
                String.split size_raw ~on:'x' |> List.map ~f:Int.of_string
              with
              | [ w; h ] -> (w, h)
              | _ -> failwith "Invalid input"
            in
            let tile_counts =
              String.strip grid_raw |> String.split ~on:' '
              |> List.map ~f:Int.of_string |> Array.of_list
            in
            assert (Array.length tile_counts = number_of_tiles);
            (size, tile_counts)
        | _ -> failwith "Invalid input")
  in
  (tiles, grids)

let () =
  let tiles, grids = parse_input () in
  let result =
    List.filter grids ~f:(fun ((w, h), tile_counts) ->
        let grid_size = w * h in
        let tiles_size =
          Array.foldi tile_counts ~init:0 ~f:(fun i acc tile_count ->
              (tiles.(i) * tile_count) + acc)
        in
        printf !"Tiles size: %d, Grid size: %d\n" tiles_size grid_size;
        let h = 1.3 in
        Float.(of_int tiles_size *. h < of_int grid_size))
    |> List.length
  in
  printf !"Result: %d\n" result
