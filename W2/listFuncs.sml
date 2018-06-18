(*recursively sum a list*)
fun sum_list (xs : int list) =
    if null xs
    then 0
    else hd xs + sum_list(tl xs)

fun list_product (xs : int list) =
    if null xs
    then 1
    else hd xs * list_product(tl xs)

fun countdown (x : int) = (*6 -> [6,5,4,3,2,1]*)
    if x=0
    then []
    else x :: countdown(x-1)

fun append (xs : int list, ys : int list) =
    if null xs
    then ys
    else hd xs :: append(tl xs, ys)

(*sum a list of int tuples*)
fun sum_pair_list (xs : (int * int) list) =
    if null xs
    then 0
    else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)

fun firsts (xs : (int * int) list) =
    if null xs
    then []
    else #1 (hd xs) :: firsts(tl xs)

val sum = sum_list [1,2,3,4,5,6]
val prod = list_product [1,2,3,4,5,6]
val count = countdown 6
val app = append([1,2,3],[4,5,6])
val sum_pairs = sum_pair_list [(1,2),(3,4),(5,6)]
val first = firsts [(1,4),(2,5),(3,6)]
