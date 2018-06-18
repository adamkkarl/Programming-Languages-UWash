(* Adam Karl *)
(* 17 June 2018  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* problem 1 *)
(* a *)
fun all_except_option (s, xs) =
    case xs of
        [] => NONE (* str not in list *)
      | x::xs' =>
            if same_string(s, x)
            then SOME xs'
            else (* haven't found string yet *)
                case all_except_option (s, xs') of
                    NONE => NONE (* string doesn't appear in rest of xs *)
                  | SOME rest => SOME (x::rest)

(* b *)
fun get_substitutions1 (lists, str) =
    case lists of
        [] => []
      | x::xs' =>
            (case all_except_option (str, x) of
                NONE => get_substitutions1 (xs', str)
              | SOME y => y @ get_substitutions1 (xs', str) )

(* c *)
fun get_substitutions2 (lists, str) =
    let fun get_substitutions_tail (mylists, acc) =
        case mylists of
            [] => acc
          | x :: xs' =>
                let val listOpt = all_except_option (str, x) in
                    case listOpt of
                        NONE => get_substitutions_tail (xs', acc)
                      | SOME y => get_substitutions_tail (xs', acc @ y)
                end
    in
        get_substitutions_tail (lists, [])
    end

(* d *)
fun similar_names (lists, {first=f, middle=m, last=la}) =
    let val first_names = f :: get_substitutions2(lists, f) in
        let
            fun all_possible_names (firsts) =
                case firsts of
                    [] => []
                  | x::xs' => {first=x, middle=m, last=la}::all_possible_names xs'
        in
            all_possible_names first_names
        end
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* problem 2 *)
(* a *)
fun card_color card =
    case card of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red

(* b *)
fun card_value card =
    case card of
        (_, Num n) => n
      | (_, Ace) => 11
      | _ => 10 (* face cards *)

(* c *)
fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | x::xs' => if x = c then xs' else x :: remove_card(xs', c, e)

(* d *)
fun all_same_color cs =
    case cs of
        [] => true
      | _::[] => true
      | c1::c2::[] => card_color c1 = card_color c2
      | c1::c2::rest => (card_color c1 = card_color c2)
            andalso all_same_color(c2::rest)

(* e *)
fun sum_cards cs =
    let fun sum_cards_tail (cs, acc) =
        case cs of
            [] => acc
          | x::xs' => sum_cards_tail (xs', acc + card_value x)
    in
        sum_cards_tail (cs, 0)
    end

(* f *)
fun score (cs, goal) =
    let val sum = sum_cards cs
    in
        let val prelim = if sum > goal then 3 * (sum - goal) else goal - sum
        in
            if all_same_color cs
            then prelim div 2
            else prelim
        end
    end

(* g *)
fun officiate (initial_deck, initial_moves, goal) =
    let fun play (deck, moves, hand) =
        if sum_cards hand > goal then score (hand, goal) else (* exceed goal = game over *)
        case moves of
            [] => score (hand, goal) (* no moves left = game over *)
          | my_move::rest_of_moves =>
                (case my_move of
                    Draw => (case deck of
                                [] => score(hand, goal) (* no cards left = game over *)
                              | c::rest_of_deck => play (rest_of_deck, rest_of_moves,
                                                c::hand) (* draw card *)
                            )
                  | Discard c => play (deck, rest_of_moves,
                                remove_card (hand, c, IllegalMove))
                )
    in play (initial_deck, initial_moves, []) end
