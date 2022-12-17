let map_choice = function
    | "A" | "X" -> 0
    | "B" | "Y" -> 1
    | _ -> 2

let lose_match c = (c + 1) mod 3

let choice_score x = x + 1

let round_score (x, y) = 
    match (x, y) with
    | (x, y) when x = (lose_match y) -> 0
    | (x, y) when x = y -> 3
    | _ -> 6

let get_score ((x, y) as result) = (choice_score y) + (round_score result)

let get_scores f =
    let file = open_in f in
    let rec get_scores' total =
        try
            let line = input_line file in
            let score = Scanf.sscanf line "%s %s %[^\n]"
                (fun x y s -> get_score (map_choice x, map_choice y))
            in get_scores' (total + score)
        with
        | End_of_file -> total
    in get_scores' 0

let () =
    let file_name = Sys.argv.(1) in
    Printf.printf("Total score is %d") (get_scores file_name)
