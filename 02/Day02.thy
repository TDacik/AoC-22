theory Day02
  imports Main HOL.String "Input02"
begin

fun take_while :: "'a list \<Rightarrow> ('a \<Rightarrow> bool) \<Rightarrow> 'a list" where
  "take_while [] _ = []"
| "take_while (x#xs) f = (if f x then x # take_while xs f else [])"

abbreviation "isnt_newline \<equiv> \<lambda>x. x \<noteq> CHR ''\<newline>''"

fun parse_line :: "string \<Rightarrow> char \<times> char" where
  "parse_line (a # _ # b # []) = (a, b)"
| "parse_line _ = undefined"

fun parse_input :: "string \<Rightarrow> (char \<times> char) list" where
  "parse_input [] = []"
| "parse_input xs = (
    let word = take_while xs isnt_newline in
    let len = List.length word + 1 in
    if word \<noteq> []
    then (parse_line) word # parse_input (List.drop len xs)
    else parse_input (List.drop len xs)
)"

(* Isabelle cannot handle functions with nine cases... *)

fun compute_rock :: "char \<Rightarrow> nat" where
  "compute_rock (CHR ''X'') = 4"
| "compute_rock (CHR ''Y'') = 8"
| "compute_rock (CHR ''Z'') = 3"
| "compute_rock _ = 0"

fun compute_paper :: "char \<Rightarrow> nat" where
  "compute_paper (CHR ''X'') = 1"
| "compute_paper (CHR ''Y'') = 5"
| "compute_paper (CHR ''Z'') = 9"
| "compute_paper _ = 0"

fun compute_scissors :: "char \<Rightarrow> nat" where
  "compute_scissors (CHR ''X'') = 7"
| "compute_scissors (CHR ''Y'') = 2"
| "compute_scissors (CHR ''Z'') = 6"
| "compute_scissors _ = 0"

fun compute_round :: "char \<times> char \<Rightarrow> nat" where
  "compute_round (CHR ''A'', x) = compute_rock x"
| "compute_round (CHR ''B'', x) = compute_paper x"
| "compute_round (CHR ''C'', x) = compute_scissors x"
| "compute_round _ = 0"

fun compute_rock2 :: "char \<Rightarrow> nat" where
  "compute_rock2 (CHR ''X'') = 3"
| "compute_rock2 (CHR ''Y'') = 4"
| "compute_rock2 (CHR ''Z'') = 8"
| "compute_rock2 _ = 0"

fun compute_paper2 :: "char \<Rightarrow> nat" where
  "compute_paper2 (CHR ''X'') = 1"
| "compute_paper2 (CHR ''Y'') = 5"
| "compute_paper2 (CHR ''Z'') = 9"
| "compute_paper2 _ = 0"

fun compute_scissors2 :: "char \<Rightarrow> nat" where
  "compute_scissors2 (CHR ''X'') = 2"
| "compute_scissors2 (CHR ''Y'') = 6"
| "compute_scissors2 (CHR ''Z'') = 7"
| "compute_scissors2 _ = 0"

fun compute_round2 :: "char \<times> char \<Rightarrow> nat" where
  "compute_round2 (CHR ''A'', x) = compute_rock2 x"
| "compute_round2 (CHR ''B'', x) = compute_paper2 x"
| "compute_round2 (CHR ''C'', x) = compute_scissors2 x"
| "compute_round2 _ = 0"

definition main1 :: "string \<Rightarrow> nat" where
  "main1 str = List.foldl (\<lambda>acc x. acc + compute_round x) 0 (parse_input str)"

definition main2 :: "string \<Rightarrow> nat" where
  "main2 str = List.foldl (\<lambda>acc x. acc + compute_round2 x) 0 (parse_input str)"

value "main1 input" (* 15523 *)
value "main2 input" (* 15702 *)

end
