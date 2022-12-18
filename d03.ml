exception Error of string

let is_lowercase x = (x >= int_of_char 'a') && (x <= int_of_char 'z')

let get_priority x = 
    let x = int_of_char x in
        if (is_lowercase x) then x - (int_of_char 'a') + 1
        else x - (int_of_char 'A') + 27

let get_items f =
    let file = open_in f in
    let rec get_items' l =
        try
            let line = input_line file in
            let as_list = List.init (String.length line) (String.get line)
            in get_items' (as_list::l)
        with
        | End_of_file -> close_in file ; l
    in get_items' []

module Item = struct
    type t = char
    let compare x y = Int.compare (get_priority x) (get_priority y)
end

module RuckSack = struct
    include Set.Make(Item)

    let rec take n l =
        match l with
        | (x::xs) when n > 0 -> x::(take (n - 1) xs)
        | _ -> []

    let rec drop n l =
        match l with
        | (x::xs) when n > 0 -> drop (n - 1) xs
        | _ -> l

    let get_dup l = List.fold_left (fun acc curr -> 
            if acc = empty then of_list curr
                else inter acc (of_list curr)) empty l

    (* Part 1: Gets priority sum from each line, halving each line to
        find the common item *)
    let get_priority_sum l =
        let rec get_sum total = function
            | x::xs ->
                let c_size = (List.length x) / 2 in
                let c1 = take c_size x in
                let c2 = drop c_size x in
                let total' = total + (get_priority @@ min_elt @@ get_dup (c1::c2::[]))
                in get_sum total' xs
            | _ -> total
        in get_sum 0 l

    (* Part 2: Gets priority sum from every n lines *)
        let get_priority_sum2 l n =
        let rec get_sum total i g = function
            | x::xs ->
                if (i + 1) mod n <> 0 then get_sum total (i + 1) (x::g) xs
                else
                    let total'= total + (get_priority @@ min_elt @@ get_dup (x::g))
                    in get_sum total' 0 [] xs
            | _ -> total
        in get_sum 0 0 [] l

end

let () =
    let file_name = Sys.argv.(1) in
    let num_groups = int_of_string Sys.argv.(2) in
    let items = get_items file_name in
    Printf.printf("Sum of priorities is %d\n") (RuckSack.get_priority_sum items) ;
    Printf.printf("Sum of priorities in groups of %d is %d")
        num_groups (RuckSack.get_priority_sum2 items num_groups) ;
