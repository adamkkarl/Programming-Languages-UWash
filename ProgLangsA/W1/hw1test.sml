(* Adam Karl 10 June 2018 *)

(* Homework1 Test *)
(* All the tests should evaluate to true. For example, the REPL should say:
val test1 = true : bool *)

use "hw1.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true
val test1a = is_older ((2,3,4),(1,2,3)) = false
val test1b = is_older ((1,2,3),(1,2,3)) = false
val test1c = is_older ((1,99,99),(2,99,99)) = true
val test1d = is_older ((99,10,99),(99,11,99)) = true
val test1e = is_older ((99,99,100),(99,99,101)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2a = number_in_month ([(2012,2,28),(2013,12,1)],1) = 0
val test2b = number_in_month ([(2012,2,28),(2013,12,1),(2000,99,99)],99) = 1
val test2c = number_in_month ([(2012,2,28),(2013,2,1)],2) = 2

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4a = dates_in_month ([(2012,2,28),(2013,2,1)],2) = [(2012,2,28),(2013,2,1)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6a = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val test6b = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test7a = date_to_string (2017, 8, 21) = "August 21, 2017"
val test7b = date_to_string (1995, 7, 26) = "July 26, 1995"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8a = number_before_reaching_sum (1, [1,2,3,4,5]) = 0
val test8b = number_before_reaching_sum (11, [1,2,3,4,5]) = 4

val test9 = what_month 70 = 3
val test9a = what_month 1 = 1
val test9b = what_month 365 = 12
val test9c = what_month 31 = 1
val test9d = what_month 32 = 2

val test10 = month_range (31, 34) = [1,2,2,2]
val test10a = month_range (30, 34) = [1,1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11a = oldest([]) = NONE
val test11b = oldest([(2011,4,28)]) = SOME (2011,4,28)
