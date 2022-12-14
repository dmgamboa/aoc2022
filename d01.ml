let get_cals f =
    let file = open_in f in
    let rec read_file curr all =
        try
            match (input_line file) with
            | "" -> read_file 0 (curr::all)
            | cal -> read_file (curr + (int_of_string cal)) all
        with
        | End_of_file -> close_in file ;
            all |> List.sort (fun x y -> if x > y then -1 else 1)
    in read_file 0 []

let get_top_cals l n =
    let rec get_top_cals' l i acc =
        if i > n - 1 then acc
        else match l with
        | x::xs -> get_top_cals' xs (i + 1) (x + acc)
        | _ -> acc
    in get_top_cals' l 0 0

let () =
    let file_name = Sys.argv.(1) in
    let cals_to_sum = int_of_string Sys.argv.(2) in
    let cals = get_cals file_name in
    Printf.printf ("Max Calories: %d\n") (get_top_cals cals 1) ;
    Printf.printf("Top %d Calories: %d\n") cals_to_sum 
        (get_top_cals cals cals_to_sum) ;