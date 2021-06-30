type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let acceptor path frag = match frag with
  | [] -> Some path
  | _ -> None

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


(*Helps with matching terminals*)
let terminalHelper fragment value x = match fragment with
  | [] -> None 
  | topFrag::botFrag -> if topFrag <> value 
    then None
    else x botFrag
    
(*Helps with recursion for checking*)
let rec matchHelper crit x prog check frag = match crit with
  | [] -> x check frag 
  | headCrit::tailCrit -> match headCrit with
    | T opt -> let oof = (matchHelper tailCrit x prog check) in
      terminalHelper frag opt oof
    | N opt -> 
      if frag = [] 
        then if crit <> [] 
          then None
          else x check []
        else let spoof = matchHelper tailCrit (x) prog in
          let big = nontermParser opt prog (prog opt) spoof frag check in
           big and nontermParser start prog rules x frag check = match rules with 
            | [] -> None
            | headRule::tailRule -> let mh = matchHelper headRule x prog ((start, headRule)::check) frag in
              let np = nontermParser start prog tailRule x frag check in
              match mh with
                | Some temp -> Some temp
                | None -> np
    
(*Return tuples of start expressions/symbols and their respective rules*)
let createTuple pair = 
  match pair with 
  | (sym, rule) -> fun create -> nontermParser sym rule (rule sym) acceptor create []

(*Creating the tree*)
let rec createTree tree = match tree with
  | head::tail -> let first = createBranches tail (snd head) in
    (match first with
      | (remaining, children) -> remaining, Node((fst head), children))
  | [] -> invalid_arg "path" and createBranches remaningPath rules = match rules with 
    | [] -> remaningPath, []
    | headRule::tailRule -> match headRule with
      | T element -> let green = Leaf element in 
        let br = createBranches remaningPath tailRule in
          (match br with 
            | (remaining, sibling) -> remaining, green::sibling)
      | N element -> let cr2 = createTree remaningPath in
        match cr2 with
          | (remaining, node) -> (match (createBranches remaining tailRule) with
            | (remaining_remain, sibling) -> remaining_remain, node::sibling)



let rec helper1 rules types = match rules with
| head::tail -> if(fst head) <> types 
  then helper1 tail types
  else (snd head)::helper1 tail types
| [] -> []


let rec helper2 sapling = match sapling with
| [] -> []
| head::tail -> match head with
  | Node (a, b) -> (helper2 b) @ (helper2 tail)
  | Leaf leaf -> leaf::(helper2 tail)


let rec helper3 grammar rules accept frag = match rules with
| [] -> None
| head::tail -> let temp = matched grammar head accept frag in
if temp = None 
  then helper3 grammar tail accept frag
  else temp and matched grammar r accept frag = match r with
    | [] -> accept frag 
    | rule1::rule2 -> match rule1 with
      | N pimp -> helper3 grammar (grammar pimp) (matched grammar rule2 accept) frag
      | T pimp -> match frag with
        | [] -> None
        | frag1::frag2 -> if frag1 <> pimp 
          then None
          else matched grammar rule2 accept frag2




          
(*Problem 1 - convert_grammar gram1*)
(*Convert a HW1 style grammar into a HW2 style grammar*)
(*Note: fst and snd operators retreive the first and second elements of a tuple*)
let convert_grammar graml = 
  let first = (fst graml) in 
    let second = (snd graml) in
      first, helper1 second

(*Problem 2 - parse_tree_leaves tree*)
(*Traverse a parse tree from left to right and yield a list of the leaves encountered*)
(*Note: @ is for list concatenation*)
let rec parse_tree_leaves tree = 
  (helper2 [tree])

(*Problem 3 - make_matcher gram*)
(*Return a matcher for the grammar gram*)
let make_matcher gram =
  let second = (snd gram) in 
    let first = (fst gram) in 
      helper3 second (second first)

(*Problem 4 - make_parser gram*)
(*Return a parser for the grammar gram*)
let make_parser gram frag = 
  let temp = createTuple gram frag in
    let path = temp in 
      match path with
      | None -> None
      | Some path -> Some (snd (createTree (List.rev path)))