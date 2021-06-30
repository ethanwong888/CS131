(*Implementation of Functions*)

(*Problem 1 -- subset a b*)
let subset a b =
List.for_all(fun c -> List.exists (fun d -> c = d) b) a
(* check each element of a one-by-one and see if in b *)
(* the set you're invoking on is the outside one at the end *)


(*Problem 2 -- equal_sets a b*)
let equal_sets a b =
if(subset a b) && (subset b a) then true
else false


(*Problem 3 -- set_union a b*)
let set_union a b =
List.sort_uniq compare (List.append a b)


(*Problem 4 -- set_all_union a*)
let rec set_all_union a = match a with
  | [] -> []
  | y::remaining -> set_union y (set_all_union remaining)


(*Problem 5 -- self_member s*)
(*This is impossible because a set can not be a member of itself under the axiom of regularity. Every set can be a subset of itself,
       but that is another issue. For example, imagine if there was a one-element set: [8]. For this set to be a member of itself, that
       would mean the single element would have to be [8], not 8. Because 8 != [8], this means a set can not be a member of itself.
       There is no way to check this in oCaml because it doesn't even make sense from a amethematical perspective in the first place.*)


(*Problem 6 -- computed_fixed_point eq f x*)
let rec computed_fixed_point eq f x =
if(eq(f x) x) then x
else computed_fixed_point eq f (f x)



(*Problem 7 -- computed_fixed_point eq f x - note: this problem requires multiple parts, so it's a bit longer*)
type ('nonterminal, 'terminal) symbol = | N of 'nonterminal | T of 'terminal

(*First, find the non-terminal symbols and separate them into their own list*)
let rec clean_symbols_list lis = match lis with
  | [] -> []
  | y::remaining -> match y with
    | N nonterminal -> nonterminal::(clean_symbols_list remaining)
    | _ -> clean_symbols_list remaining

(*Next, separate list of non-terminal symbols according to the given rules*)
let rec clean_rules_list lis = match lis with
  | [] -> []
  | y::remaining -> match y with
    | (nonterminal, terminal) -> (set_union (clean_symbols_list terminal) (clean_rules_list remaining))

(*Next, separate list of non-terminal symbols that can be reached by nonterminal based on the grammar*)
let clean_grammar gram = match gram with
| (nonterminal, terminal) ->
nonterminal::(clean_rules_list (List.filter(fun y -> match y with
						    | (c, d) -> if c = nonterminal then true 
    else false) terminal))

(*Next, separate list of non-terminal symbols that can be reached by list based on the list of non-terminal symbols*)
let rec acquire_nonterminal lis dist = match lis with
  | [] -> []
    | y::remaining -> (set_union (clean_grammar (y, dist)) (acquire_nonterminal remaining dist))

(*Acquire a list of all the non-terminal symbols that can be reached *)
let rec acquire_nonterminal_full_list checked all dist =
  let current = ((fun a b -> List.filter(fun x -> not(List.exists (fun y -> x=y) b)) a) all checked) in
    match current with
    | [] -> all
        | _ -> acquire_nonterminal_full_list all (set_union (acquire_nonterminal current dist) all) dist

(*Compare the list to the grammar to get rid of the unreachable rules*)
let rec comparison gram lis = match gram with
  | (nonterminal, terminal) -> match terminal with
      | [] -> [];
    | y::remaining -> match y with
          | (c, d) -> if (subset [c] lis) 
        then (c, d)::(comparison (nonterminal, remaining) lis)
        else comparison (nonterminal, remaining) lis


(*Actual function, ties everything together*)
let filter_reachable g = match g with
| (nonterminal, terminal) -> let complete = acquire_nonterminal_full_list [nonterminal] (clean_grammar g) terminal in
  (nonterminal, comparison g complete)
