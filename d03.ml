exception Error of string

let is_lowercase x = (x >= int_of_char 'a') && (x <= int_of_char 'z')

let get_priority x = 
    let x = int_of_char x in
        if (is_lowercase x) then x - (int_of_char 'a') + 1
        else x - (int_of_char 'A') + 27

module Item = struct
    type t = char
    let compare x y = Int.compare (get_priority x) (get_priority y)
end

module RuckSack = struct
    include Set.Make(Item)

    let get_items f =
        let file = open_in f in
        let rec get_items' l =
            try
                get_items' ((input_line file)::l)
            with
            | End_of_file -> l
        in get_items' []

    let rec take n l =
        match l with
        | (x::xs) when n > 0 -> x::(take (n - 1) xs)
        | _ -> []

    let rec drop n l =
        match l with
        | (x::xs) when n > 0 -> drop (n - 1) xs
        | _ -> l

    let get_dup r =
        let rucksack = List.init (String.length r) (String.get r) in
        let c_size = (List.length rucksack) / 2  in
        let c1 = of_list (take c_size rucksack) in
        let c2 = of_list (drop c_size rucksack) in
        min_elt @@ inter c1 c2

    let get_priority_sum f =
        let rec get_priority_sum' total = function
            | x::xs -> get_priority_sum' (total + (get_priority @@ get_dup x)) xs
            | _ -> total
        in get_priority_sum' 0 (get_items f)
end

let () =
    let file_name = Sys.argv.(1)
    in Printf.printf("Sum of priorities is %d") (RuckSack.get_priority_sum file_name)