--- Pick TERM and FREE TERM -- create stack of free terms wich will be applied
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
pop :: [a] -> a
pop (x:xs) = x

push :: a -> [a] -> [a]
push e l = [e] ++ l
--- zlicza nawiasy i bierze element nimi otoczony
cut_substring :: String -> Int -> Int  -> String
cut_substring [] open_s close_s  = if open_s == close_s then [] else " MISSING PARENTHESIS ERROR! " 
cut_substring ('(':cs) open_s close_s = if (open_s + 1) - close_s == 0 && ((open_s + 1) > 0) && (close_s > 0) then "(" else "(" ++ cut_substring cs (open_s + 1) close_s
cut_substring (')':cs) open_s close_s = if open_s - (close_s + 1) == 0 && (open_s > 0) && ((close_s + 1) > 0) then ")" else ")" ++ cut_substring cs open_s (close_s + 1)
cut_substring (c:cs) open_s close_s = [c] ++ cut_substring cs open_s close_s

take_term :: String -> String
take_term s = cut_substring s 0 0

take_free_term :: String -> String
take_free_term s = reverse $ cut_substring (reverse s) 0 0

-- equality :: String -> String -> Bool
-- equality [] [] = True
-- equality [] _ = False
-- equality _ [] = False
-- equality (c:cs) (d:ds) =  if c /= d then False else equality cs ds

check_if_term_start_with_lambda :: String -> Bool
check_if_term_start_with_lambda ('\\':_) = True
check_if_term_start_with_lambda (_:_) = False

check_if_to_process_term :: String -> Bool
check_if_to_process_term s = check_if_term_start_with_lambda $ (take_term $ delete_parenthesis s)

update_term_and_stack :: String -> [String] -> (String, [String])
update_term_and_stack s stack
  | check_if_to_process_term s == True = (s, stack)
  | otherwise =  update_term_and_stack (take_term $ delete_parenthesis s) (push (take_free_term $ delete_parenthesis s) stack)

--- UTILS
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------

change_char_to_string :: String -> Char -> String -> String
change_char_to_string [] var free_term = []
change_char_to_string (c:cs) var free_term
  | c == var = free_term ++ change_char_to_string cs var free_term
  | otherwise = [c] ++ change_char_to_string cs var free_term

count_parenthesis_until_lambda :: String -> Int
count_parenthesis_until_lambda ('\\':_) =  0
count_parenthesis_until_lambda ('(':cs)  =  1 + count_parenthesis_until_lambda cs

--- ALPHA CONVERSION
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------

alpha_conversion :: String -> String -> String
alpha_conversion s s_el =  change_vars_using_dict (create_alpha_dict (get_same_vars s s_el) (create_term_variables_list s s_el)) s_el

create_term_variables_list :: String -> String -> String
create_term_variables_list s s_el = get_unique_vars (s ++ s_el)

get_unique_vars :: String -> String
get_unique_vars s = unique_string (get_vars s)

get_vars :: String -> String
get_vars [] = []
get_vars (c:cs) = if c /= '(' && c /= ')' && c /= '.' && c /= '\\' then [c] ++ get_vars cs else get_vars cs

unique_string :: String -> String
unique_string [] = []
unique_string (x:xs) = x:unique_string (filter (\e -> e /= x) xs)

prepare_terms :: String -> String -> String
prepare_terms tv ftv = unique_string tv ++ ftv

get_same_vars :: String -> String -> String
get_same_vars t ft = pick_same_vars (get_unique_vars t) (get_unique_vars ft)

pick_same_vars :: String -> String -> String
pick_same_vars [] _ = []
pick_same_vars (c:cs) s2 = compare_char_with_string c s2 ++ pick_same_vars cs s2

compare_char_with_string :: Char -> String -> String
compare_char_with_string _ [] = []
compare_char_with_string c (s:ss) = if c == s then [c] else compare_char_with_string c ss

create_alpha_dict :: String ->  String -> [(Char, Char)]
create_alpha_dict [] _  = []
create_alpha_dict (sv:ssv) pt  = [(sv, get_new_var pt)]  ++ create_alpha_dict ssv (pt ++ [get_new_var pt])

get_new_var :: String -> Char
get_new_var pt = head $ check_diffrent_vars "abcdefghijklmnoprstwuqyz" pt

check_diffrent_vars :: String -> String -> String
check_diffrent_vars [] _ = []
check_diffrent_vars (c:cs) s2 = compare_char_with_string_diff c s2 ++ check_diffrent_vars cs s2

compare_char_with_string_diff :: Char -> String -> String
compare_char_with_string_diff c [] = [c]
compare_char_with_string_diff c (s:ss) = if c == s then [] else compare_char_with_string_diff c ss

change_vars_using_dict :: [(Char, Char)] -> String -> String
change_vars_using_dict [] s_el = s_el
change_vars_using_dict ((d, n):rest) s_el  = change_vars_using_dict rest (change_char_to_string s_el d [n])

--- BETA REDUCTION
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

beta_reduction :: String -> String -> String
beta_reduction t ft = change_char_to_string (clean_for_beta t) (get_variable t) ft

get_variable :: String -> Char
get_variable s = get_variable_char $ delete_parenthesis s

get_variable_char :: String -> Char
get_variable_char ('\\':c:_) = c

delete_fun_sig :: String -> String
delete_fun_sig ('\\':_:'.':cs) = cs

clean_for_beta :: String -> String
clean_for_beta s = delete_fun_sig $ delete_parenthesis s

delete_parenthesis :: String -> String
delete_parenthesis s = tail $ init s
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------

apply_alfa_beta :: (String, [String]) -> String
apply_alfa_beta (s, []) = s
apply_alfa_beta (s, l:ls) = apply_alfa_beta ( beta_reduction s (alpha_conversion s l), ls) -- pop stack


-- beta_reduction -- delete \\x. and delete parenthesises

main = do
         row <- getLine
         if row == (".")
           then return ()
         else do _ <- putStrLn $ change_char_to_string row 'm' "(\\y.(\\x.(y.xy))"
                 main
