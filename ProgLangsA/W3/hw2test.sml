(* Homework2 Simple Test *)
(* Adam Karl *)
(* 17 June 2018 *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw2.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []
val test1a = all_except_option ("string", ["string", "mystring"]) = SOME ["mystring"]
val test1b = all_except_option ("string", ["mystring", "string"]) = SOME ["mystring"]
val test1c = all_except_option ("b", ["a","b","c"]) = SOME ["a","c"]
val test1d = all_except_option ("string", ["help"]) = NONE

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2a = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],
    ["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test2c = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],
	["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3a = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],
    ["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test3c = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],
	["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black
val test5a = card_color (Hearts, Num 10) = Red
val test5b = card_color (Spades, Ace) = Black
val test5c = card_color (Diamonds, Jack) = Red

val test6 = card_value (Clubs, Num 2) = 2
val test6a = card_value (Hearts, Num 9) = 9
val test6b = card_value (Spades, Ace) = 11
val test6c = card_value (Diamonds, Jack) = 10

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7a = remove_card ([(Hearts, Ace),(Diamonds, Jack)], (Hearts, Ace), IllegalMove)
 	= [(Diamonds, Jack)]
val test7b = remove_card ([(Clubs, Num 2),(Hearts, Ace),(Diamonds, Jack)], (Hearts, Ace), IllegalMove)
 	= [(Clubs, Num 2),(Diamonds, Jack)]

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8a = all_same_color [] = true
val test8b = all_same_color [(Hearts, Ace)] = true
val test8c = all_same_color [(Hearts, Ace), (Diamonds, Jack)] = true
val test8d = all_same_color [(Hearts, Ace), (Spades, Jack)] = false
val test8e = all_same_color [(Hearts, Ace), (Diamonds, Jack),(Hearts, Num 9),(Diamonds, King)] = true


val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test9a = sum_cards [(Clubs, Num 2),(Clubs, Ace)] = 13
val test9b = sum_cards [(Clubs, Num 2),(Clubs, Ace),(Diamonds, Jack)] = 23
val test9c = sum_cards [(Clubs, Num 2),(Clubs, Num 9),(Diamonds, King)] = 21

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10a = score ([(Hearts, Ace),(Clubs, Num 4)],10) = 15
val test10b = score ([(Hearts, Num 2),(Clubs, Num 4)],6) = 0

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)
