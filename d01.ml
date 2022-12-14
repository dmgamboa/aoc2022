let get_max_cals f =
    let file = open_in f in
    let rec read_file curr all =
        try
            match (input_line file) with
            | "" -> read_file 0 (curr::all)
            | cal -> read_file (curr + (int_of_string cal)) all
        with
        | End_of_file -> close_in file ;
            all |> List.sort (fun x y -> if x > y then -1 else 1) |> List.hd
    in read_file 0 []

let () =
    let file_name = Sys.argv.(1) in
    Printf.printf ("Max Calories: %d\n") (get_max_cals file_name) ;