datatype my_type = TwoInts of int * int
                | Str of string
                | Pizza

(* mytype -> int *)
fun f (x) =
    case x of
        Pizza => 3
      | Str s => 8
      | TwoInts(i1,i2) => i1 + i2;

f Pizza;
f (Str "hi");
