type stuff = | S | NP | VP | NOUN | VERB | ADJ | ADV

let tempGrammar = (S, function
  | S -> [ [N NOUN; N VERB]; [N NP; N VP]]
  | NOUN -> [ [T "cups"]; [T "pencils"]; [T "plates"]; [T "cows"]; [T "bone"]; [T "speakers"]; [T "bottles"] ]
  | NP -> [ [N NOUN]; [N ADJ; N NOUN] ]
  | ADJ -> [ [T "ugly"]; [T "fly"]; [T "clean"]; [T "fat"]; [T "stupid"]; [T "skinny"]; [T "genius"] ]
  | VERB -> [ [T "squirm"]; [T "bark"]; [T "screech"]; [T "cry"]; [T "guzzle"]; [T "gulp"]; [T "inhale"]; [T "sigh"] ]
  | VP -> [ [N VERB]; [N VERB; N ADV] ]
  | ADV -> [ [T "loudly"]; [T "quickly"]; [T "softly"]; [T "incessantly"]; [T "impressively"]; [T "hastily"] ]
)
let acceptGenerous string = Some string


let make_matcher_test = 
  let test = make_matcher tempGrammar acceptGenerous ["fat"; "cows"; "guzzle"; "incessantly"] in
    test = Some ["incessantly"]

let make_parser_test = 
  let test = make_parser tempGrammar ["ugly"; "bottles"; "guzzle"; "impressively"] in 
    test 
    = Some (Node (S, 
      [Node (NP,
        [Node (ADJ,
          [Leaf "ugly"]);
        Node (NOUN,
          [Leaf "bottles"])]);
        Node (VP,
          [Node (VERB,
            [Leaf "guzzle"]);
          Node (ADV,
            [Leaf "impressively"])])]))