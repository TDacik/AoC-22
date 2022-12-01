theory Day01
  imports Main HOL.String "Input01"
begin

fun int_of_char :: "char \<Rightarrow> nat" where
  "int_of_char (CHR ''1'') = 1"
| "int_of_char (CHR ''2'') = 2"
| "int_of_char (CHR ''3'') = 3"
| "int_of_char (CHR ''4'') = 4"
| "int_of_char (CHR ''5'') = 5"
| "int_of_char (CHR ''6'') = 6"
| "int_of_char (CHR ''7'') = 7"
| "int_of_char (CHR ''8'') = 8"
| "int_of_char (CHR ''9'') = 9"
| "int_of_char x = 0"

definition int_of_string_aux :: "char \<Rightarrow> nat \<times> nat \<Rightarrow> nat \<times> nat" where
  "int_of_string_aux ch acc = (1 + fst acc, snd acc + (int_of_char ch) * (power 10 (fst acc)))"

definition int_of_string :: "string \<Rightarrow> nat" where
  "int_of_string str = snd (List.foldr int_of_string_aux str (0, 0))"

fun take_while :: "'a list \<Rightarrow> ('a \<Rightarrow> bool) \<Rightarrow> 'a list" where
  "take_while [] _ = []"
| "take_while (x#xs) f = (if f x then x # take_while xs f else [])"

abbreviation "isnt_newline \<equiv> \<lambda>x. x \<noteq> CHR ''\<newline>''"

fun parse_input :: "string \<Rightarrow> nat list" where
  "parse_input [] = []"
| "parse_input xs = (
    let word = take_while xs isnt_newline in
    let len = List.length word + 1 in
    (int_of_string word) # parse_input (List.drop len xs)
)"

definition sum_aux :: "nat \<times> nat list \<Rightarrow> nat \<Rightarrow> nat \<times> nat list" where
  "sum_aux acc x = (if x = 0 then (0, fst acc # snd acc) else (fst acc + x, snd acc))"

definition sum :: "nat list \<Rightarrow> nat list" where
  "sum xs = snd (List.foldl sum_aux (0, []) xs)"

definition max :: "nat list \<Rightarrow> nat" where
  "max xs = List.foldl (\<lambda>x acc. if x > acc then x else acc) 0 xs"

definition main1 :: "string \<Rightarrow> nat" where
  "main1 str = max (sum (parse_input str))"

definition main2 :: "string \<Rightarrow> nat" where
  "main2 str = (
    let sums_x = sum (parse_input str) in 
    let x = max sums_x in
    let sums_y = List.remove1 x sums_x in    
    let y = max sums_y in
    let sums_z = List.remove1 y sums_y in
    let z = max sums_z in
    x + y + z
  )"

value "main1 input" (* 71124 *)
value "main2 input" (* 204639 *)

end