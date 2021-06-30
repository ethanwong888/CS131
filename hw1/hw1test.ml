(*My Test Cases*)

(*Problem 1 -- subset a b*)
let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [4;9;2]
let my_subset_test2 = subset [1;2;3] [5;3;4;1;2]
let my_subset_test3 = not(subset [4] [7;9;2])


				 (*Problem 2 -- equal_sets a b*)
				 let my_equal_sets_test0 = equal_sets [] []
				 let my_equal_sets_test1 = equal_sets [6;7;8;9] [6;7;9;8]
				 let my_equal_sets_test2 = not (equal_sets [1;2;3] [1;2;3;9])
									   let my_equal_sets_test3 = not (equal_sets [9;2;4] [4;8;1])


														     (*Problem 3 -- set_union a b*)
														     let my_set_union_test0 = equal_sets(set_union [] []) []
														     let my_set_union_test1 = equal_sets(set_union [4;4] [4;4]) [4;4]
																				   let my_set_union_test2 = equal_sets(set_union [4;5] [6;8]) [4;5;6;8]
																										 let my_set_union_test3 = equal_sets(set_union [8;8] [8;8]) [8]


																															       (*Problem 4 -- set_all_union a*)
																															       let my_set_all_union_test0 = equal_sets (set_all_union [[0];[1]]) [0;1]
																																						      let my_set_all_union_test1 = equal_sets (set_all_union [[0;1];[2;3]]) [0;1;2;3]
																																													     let my_set_all_union_test2 = equal_sets (set_all_union [[0;1;2];[3;4;5]]) [0; 1; 2; 3; 4; 5]


																																																				    (*Problem 5 -- self_member s*)
																																																				    (*No tests needed for this one as the function is not possible.*)


																																																				    (*Problem 6 -- computed_fixed_point eq f x*)
																																																				    let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x) 888 = 888
																																																				    let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x / 2) 899 = 0


																																																				    (*Problem 7 -- filter_reachable g*)
																																																				    type my_nonterminals = | ACHOO | BEE | COW | DIH
																																																				    let my_rules = [
																																																				    ACHOO, [N DIH];
																																																				    ACHOO, [T "monke"];
																																																				    BEE, [N COW];
																																																				    COW, [T "oop"];
																																																				    DIH, [T "gorilla"]
																																																				    ]

																																																				    let my_filter_reachable_test0 = filter_reachable (COW, my_rules) = (COW, [COW, [T "oop"]])
																																																				    let my_filter_reachable_test1 = filter_reachable (DIH, my_rules) = (DIH, [DIH, [T "gorilla"]])
																																																				    
