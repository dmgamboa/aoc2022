let lose_match c = (c + 1) mod 3

(* Part 1: Assumes X, Y, Z refers to Rock, Paper, Scissors, respectively *)
let map_choice = function
    | "A" | "X" -> 0
    | "B" | "Y" -> 1
    | _ -> 2

(* Part 2: Assumes X, Y, Z refers to lose, draw, win, respectively *)
let map_choice2 x = function
    | "X" -> lose_match @@ lose_match x
    | "Y" -> x
    | _ -> lose_match x

let choice_score x = x + 1

let round_score (x, y) = 
    match (x, y) with
    | (x, y) when x = (lose_match y) -> 0
    | (x, y) when x = y -> 3
    | _ -> 6

let get_score ((x, y) as result) = (choice_score y) + (round_score result)

let xyz_as_rps x y s = get_score (map_choice x, map_choice y)

let xyz_as_ldw x y s = 
    let x = map_choice x
    in get_score (x, map_choice2 x y)

let get_scores file_name mapper =
    let file = open_in file_name in
    let rec get_scores' total =
        try
            let line = input_line file in
            let score = Scanf.sscanf line "%s %s %[^\n]" mapper
            in get_scores' (total + score)
        with
        | End_of_file -> total
    in get_scores' 0

let () =
    let file_name = Sys.argv.(1) in
    let mapper =
        match Sys.argv.(2) with
        | "2" -> xyz_as_ldw
        | _ -> xyz_as_rps
    in Printf.printf("Total score is %d") (get_scores file_name mapper)
