(* Adam Karl 10 June 2018 *)

(* true if the second date comes after the first date, false otherwise *)
fun is_older (a : int * int * int, b : int * int * int) =
    if #1 a < #1 b (* year *)
    then true
    else if #1 a > #1 b
    then false
    else if #2 a < #2 b (* month *)
    then true
    else if #2 a > #2 b
    then false
    else if #3 a < #3 b (* day *)
    then true
    else false

(* given a list of dates and a month, return the number of dates with the
specified month *)
fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month (tl dates, month)
    else 0 + number_in_month (tl dates, month)

(* takes a list of dates and a list of months and returns the number of
dates in the list of dates from any of the months given *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)

(* takes a list of dates and month and returns a list of all dates from the
month *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then hd dates :: dates_in_month (tl dates, month)
    else dates_in_month (tl dates, month)

(* takes list of dates adn list of months and returns a list with all dates in
any of the months in the list of months *)
fun dates_in_months (dates : (int * int * int ) list, months : int list) =
    if null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months(dates, tl months)

(* given a list of strings and n, return the nth string in the list *)
fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n-1)

(* take a date and return the string name
ex: [2012, 1, 20] returns January 20, 2013 *)
fun date_to_string (date : int * int * int) =
    let val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* given a sum and a list of nums, return the number of items in the list
that can be added before reaching the sum *)
fun number_before_reaching_sum (sum : int, nums : int list) =
    if null nums orelse hd nums >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - hd nums, tl nums)

(* given a number 1-365, return what month number the day falls in *)
fun what_month (day : int) =
    let val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, month_days) + 1
    end

(* given 2 days (1-365 each) pring the month of each day from day 1 to day 2 *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* Take a list of dates and return the oldest as SOME date
or NONE if the list is empty *)
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else if null (tl dates)
    then SOME (hd dates)
    else
        let val tail_ans = oldest(tl dates)
        in
            if isSome tail_ans
            then
                if is_older(hd dates, valOf tail_ans)
                then SOME (hd dates)
                else SOME (valOf tail_ans)
            else SOME (hd dates)
        end
