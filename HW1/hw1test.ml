(* Subset test cases *)
let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [4;5;6]
let my_subset_test2 = subset [4;5;6] [4;5;6]
let my_subset_test3 = not (subset [4;5;6] [])
let my_subset_test4 = subset [6;4;4;5;6] [4;5;6]
let my_subset_test5 = subset [4;5] [4;5;6;7]
let my_subset_test6 = subset [6;5;4] [4;5;6]
let my_subset_test7 = subset [4;5;6;7] [4;5;6]
let my_subset_test8 = subset ["x";"y"] ["x";"y";"z"]
let my_subset_test9 = not (subset ["x";"y";"z"] ["x";"y"])

(* Equal sets test cases *)
let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = not (equal_sets [] [4])
let my_equal_sets_test2 = equal_sets [[1;2];[3;4]] [[3;4];[1;2]]
let my_equal_sets_test3 = not (equal_sets [[1;2];[3;4]] [[2;1];[4;3]])
let my_equal_sets_test4 = equal_sets [4;5] [5;4]
let my_equal_sets_test5 = equal_sets [4;5] [4;5;5;4]
let my_equal_sets_test6 = not (equal_sets [4;5;6] [4;5;5;4])
let my_equal_sets_test7 = not (equal_sets [[]] [])
let my_equal_sets_test8 = not (equal_sets [4;5;4] [4;5;6])
let my_equal_sets_test9 = equal_sets ["x";"y";"z"] ["x";"z";"y"]
let my_equal_sets_test10 = not (equal_sets ["x";"y";"z";"a"] ["x";"z";"y"])

(* Set Union test cases *)
let my_set_union_test0 = equal_sets (set_union [] []) []
let my_set_union_test1 = equal_sets (set_union [4;5;6] []) [4;5;6]
let my_set_union_test2 = equal_sets (set_union [6;4;7] [4;5;6;8]) [4;5;6;7;8]
let my_set_union_test3 = equal_sets (set_union [4;5;6] [4;5;6]) [4;5;6]
let my_set_union_test4 = equal_sets (set_union [4;5;6] [6;4;6;4;5;6]) [4;5;6]
let my_set_union_test5 = equal_sets (set_union [[1]] [[2]]) [[1];[2]]
let my_set_union_test6 = not (equal_sets (set_union [4;5;6] [6;4;6;4;5;6]) [4;5;6;7])
let my_set_union_test7 = equal_sets (set_union [4;5;6] [7;8;9]) [4;5;6;7;8;9]
let my_set_union_test8 = equal_sets (set_union ["a";"b"] ["b";"c"]) ["a";"b";"c"]

(* Set all union test cases *)
let my_set_all_union_test0 = equal_sets (set_all_union []) []
let my_set_all_union_test1 = equal_sets (set_all_union [[4;5;6]]) [4;5;6]
let my_set_all_union_test2 = equal_sets (set_all_union [[4;5;6];[4;5;6]]) [4;5;6]
let my_set_all_union_test3 = equal_sets (set_all_union [[4;5;6];[7;8]]) [4;5;6;7;8]
let my_set_all_union_test4 = equal_sets (set_all_union [[4;5;6];[];[4;7];[8]]) [4;5;6;7;8]
let my_set_all_union_test5 = equal_sets (set_all_union [["a"];["b";"a"];[];["c"]]) ["a";"b";"c"]
let my_set_all_union_test6 = equal_sets (set_all_union [[]]) []

(* Computed fixed point test cases *)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> 5) 10 = 5
let my_computed_fixed_point_test1 = computed_fixed_point (=) sqrt 100000. = 1.
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x * x/2) 1 = 0
let my_computed_fixed_point_test3 = computed_fixed_point (<) (fun x -> x/2) 2 = 2
let my_computed_fixed_point_test4 =
  ((computed_fixed_point (fun x y -> abs_float (x -. y) > 20.)
			 (fun x -> x *. 3.)
			 2.)
   = 18.)
let my_computed_fixed_point_test5 =
  computed_fixed_point (=) (fun x -> x *. 5.) 100. = infinity

(* An example grammar for a small subset of Awk.  *)

type my_nonterminals =
  | A | B | C | D | E

let my_rules = 
  [
    A, [N B;N C;N D];
    A, [T"A"; N D];
    B, [N E];
    C, [T"C"];
    D, [N B; N C];
    E, [T"E"]
  ]
let my_filter_reachable_test0 = filter_reachable (A, my_rules) = (A, my_rules)

let my_rules_swapped = 
  [
    D, [N B; N C];
    A, [T"A"; N D];
    C, [T"C"];
    A, [N B;N C;N D];
    E, [T"E"];
    B, [N E]
  ]
let my_filter_reachable_test1 = filter_reachable (A, my_rules_swapped) = (A, my_rules_swapped)

let my_filter_reachable_test2 = filter_reachable (D, my_rules) = 
  (D,
    [
      B, [N E];
      C, [T"C"];
      D, [N B; N C];
      E, [T"E"]
    ]
  )

let my_filter_reachable_test3 = filter_reachable (B, my_rules) = 
  (B,
    [B, [N E];
    E, [T"E"]]
  )
type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let my_filter_reachable_test4 =
  filter_reachable awksub_grammar = awksub_grammar

let my_filter_reachable_test5 =
  filter_reachable (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)

let my_filter_reachable_test6 =
  filter_reachable (Lvalue, awksub_rules) = (Lvalue, awksub_rules)

let my_filter_reachable_test7 =
  filter_reachable (Expr, List.tl (List.tl awksub_rules)) =
    (Expr,
     [Expr, [N Expr; N Binop; N Expr];
      Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"];
      Binop, [T "+"];
      Binop, [T "-"]])

let my_filter_reachable_test8 =
  filter_reachable (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    (Expr,
     [Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"]])

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let my_filter_reachable_test9 =
  filter_reachable giant_grammar = giant_grammar

let my_filter_reachable_test10 =
  filter_reachable (Sentence, List.tl (snd giant_grammar)) =
    (Sentence,
     [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])

let my_filter_reachable_test11 =
  filter_reachable (Quiet, snd giant_grammar) = (Quiet, [Quiet, []])