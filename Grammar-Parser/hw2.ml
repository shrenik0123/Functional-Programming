type ('nonterminal, 'terminal) symbol = 
    | N of 'nonterminal
    | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
    | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
    | Leaf of 'terminal


(* Helper function for Q1 - Finds the RHS of all rules given LHS as non_term and all the rules *)
let prod_func rules non_term = 
    let non_term_rules = List.filter (fun rule -> (fst rule) = non_term) rules in
    List.map (fun (lhs, rhs) -> rhs) non_term_rules

(* Question 1 - Converts format of grammars from HW1 to those for HW2 *)
let convert_grammar gram1 = (fst gram1), (prod_func (snd gram1))

(* Question 2 - Gets the leaf nodes by implementing a functional programming variant of DFS *)
let parse_tree_leaves tree = 
    let rec parse_leaves_helper = function
        | [] -> []
        | h::t -> match h with
            | Leaf term-> term::parse_leaves_helper t
            | Node (non_term, rest) -> (parse_leaves_helper rest) @ (parse_leaves_helper t)
    in parse_leaves_helper [tree]



(* Q3 helper function - Gets the next rule, if there is one and passes it to check_rule to check. Returns the result from check rule if accepted *)
let rec get_next_rules prod_f rules acceptor frag = match rules with
    | [] -> None
    | curr_rule::rest_rules -> 
        let result = check_rule prod_f curr_rule acceptor frag in
        if result = None
            then get_next_rules prod_f rest_rules acceptor frag
        else
            result

(* This checks what to do with the frag based on the rule *)
and check_rule prod_f rule acceptor frag = match rule with
    | [] -> acceptor frag (* If no symbols left to check in rule, run acceptor on frag *)
    | curr_symbol::rest_symbols -> match curr_symbol with
        | N non_term -> (* If non-terminal symbol, then chain matchers as seen in discussion *)
            let chained_acceptor = check_rule prod_f rest_symbols acceptor in
            get_next_rules prod_f (prod_f non_term) chained_acceptor frag
        | T term -> match frag with
            | [] -> None (* If rule and symbol don't match, abort the current recursive tree *)
            | first_char::rest ->
                if first_char = term
                    then check_rule prod_f rest_symbols acceptor rest (* Check next symbol *)
                else
                    None

(* Question 3 - Make matcher returns a curried function (a mathcer) for grammar gram *)
let make_matcher gram = 
    (fun acceptor frag -> (get_next_rules (snd gram) ((snd gram) (fst gram)) acceptor frag))


(* Q4 helper function - simple acceptor that only accepts if the entire fragment has been parsed and returns the accumulated path*)
let acceptor_parse frag path = match frag with
    | [] -> Some path
    | _ -> None

(* Q4 helper function - Gets the next rule, if there is one and passes it to check_parse_rule to check. Returns the result from check rule if accepted *)
let rec get_next_parse_rules prod_f rules acceptor start_symbol frag path = match rules with
    | [] -> None
    | curr_rule::rest_rules -> 
        let result = check_parse_rule prod_f curr_rule acceptor start_symbol frag path in
        if result = None
            then get_next_parse_rules prod_f rest_rules acceptor start_symbol frag path
        else
            result

(* This checks what to do with the fragment based on the next symbol in the rule and accumulates the path in variable path *)
and check_parse_rule prod_f rule acceptor start_symbol frag path = match rule with
    | [] -> acceptor frag (Node(start_symbol, path)) (* Call acceptor when frag is empty on frag and the path which is a Node *)
    | curr_symbol::rest_symbols -> match curr_symbol with
        | N non_term -> 
            (* Creates a chained acceptor similar to the chained matchers in Q3. Also, it appends the path taken by the non-terminal symbol tot he current path *)
            let chained_acceptor next_frag recursive_path = check_parse_rule prod_f rest_symbols acceptor start_symbol next_frag (path @ [recursive_path]) in
            get_next_parse_rules prod_f (prod_f non_term) chained_acceptor non_term frag [] (* Finds recursive path *)
        | T term -> match frag with
            | [] -> None 
            | first_char::rest ->
                if first_char = term
                    (* If there is a match, then append the terminal symbol to the path. The non_term symbol is added when calling the acceptor *)
                    then check_parse_rule prod_f rest_symbols acceptor start_symbol rest (path @ [Leaf term])
                else
                    None

(* Question 4 - A curried function that takes a grammar and fragment, and returns the path to get to fully parse that fragment if it exists *)
let make_parser gram = 
    (fun frag -> (get_next_parse_rules (snd gram) ((snd gram) (fst gram)) acceptor_parse (fst gram) frag []))