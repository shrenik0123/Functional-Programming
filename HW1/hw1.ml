type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Question 1 - Checks if a is subset of b *)
let rec subset a b = match a with
    | [] -> true
    | h::t -> (subset t b) && (List.mem h b)

(* Question 2 - Checks if a and b are equal *)
let equal_sets a b = 
    (subset a b) && (subset b a)

(* Auxiliary function for Q3 - Returns a list containing unique values *)
let rec find_unique a b = match a with
    | [] -> b
    | h::t ->   if (List.mem h b) 
                    then (find_unique t b) 
                else 
                    (find_unique t (h::b))

(* Question 3 - Finds the union of a and b *)
let set_union a b =
    (find_unique (a @ b) [])

(* Question 4 - Finds the union of all the sets in a *)
let rec set_all_union a = match a with
    | [] -> []
    | h::t -> set_union h (set_all_union t)

(* Question 5 - Russel's Paradox explanation
    It is not possible to write such a function in OCaml.
    This is because the elements that belong to an OCaml list must all be of the same type.
    Now consider a primitive type variable(s) has level 0, 
    a list of a primitive type variable(s) has a level of 1,
    lists of lists of primitive type variable(s) has a level of 2 and so forth.
    Since all the elements of an OCaml list need to have the same type/level, we consider
    the elements within the list have a level of n. 
    For a list to contain itself, it is necessary to wrap all the elements at level n currently
    in the list and then add that new list to the set. But this new element must have a level
    of n+1, which means that it has a different type from all the other elements in the list.
    Since such a list is not valid in OCaml, we can't check whether a set is a member of itself.
 *)

(* Question 6 - Returns the computed fixed point if there is one *)
let rec computed_fixed_point eq f x =
    if (eq (f x) x)
        then x
    else
        computed_fixed_point eq f (f x)

(* Helper function for Q7 - Gets all the non-terminal symbols from a rule *)
let rec get_non_terminal_symbols rule = match rule with
| [] -> []
| N h::t -> h::(get_non_terminal_symbols t)
| _::t -> get_non_terminal_symbols t

(* Helper function for Q7 - Gets all the non-terminal symbols for the rules given a starting variable *)
let rec get_all_nt_symbols non_terminal_symbols rules = match rules with
    | [] -> non_terminal_symbols
    | h::t ->   let symbol = fst h in 
                let right_hand_side = snd h in
                if (List.mem symbol non_terminal_symbols)
                    then get_all_nt_symbols (set_union non_terminal_symbols (get_non_terminal_symbols right_hand_side)) t
                else
                    get_all_nt_symbols non_terminal_symbols t

(* Helper function for Q7 -  Uses all the non-terminal symbols found to get all the reachable rules *)
let rec reachable_rules all_nt_symbols rules = 
    let x = get_all_nt_symbols all_nt_symbols rules in
    let f_x = get_all_nt_symbols x rules in
    if equal_sets x f_x 
        then f_x 
    else 
        reachable_rules f_x rules   

(* Question 7 - Gets all start symbol and all reachable rules *)
let filter_reachable g =
    let start_symbol = [(fst g)] in 
    let rules = (snd g) in
    let reachable_nt_symbols = reachable_rules start_symbol rules in
    (fst g, List.filter (fun x -> List.mem (fst x) reachable_nt_symbols) rules)



(* Midterm Question *)
let rec find_nonterm rules acc=
    match rules with
        | h::t -> 
            let non_term = fst(h) in
            if (subset non_term acc) = true 
                then find_nonterm t acc
            else
                find_nonterm t (non_term @ acc)
        | [] -> acc

let gsyms g =
    let rules = snd(g) in
    get_all_nt_symbols [] rules


