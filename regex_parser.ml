type terminal = Star | Alternate | OpenPar | ClosePar | OpenSqPar | CloseSqPar | Dash | Letter | EndOfInput
type non_terminal = S | A | A' | C | C' | K | K' | F | W
type symbol = T of terminal | N of non_terminal 

let single_string s x = String.sub s x 1;;
let string_to_list s = List.init (String.length s) (single_string s);;

let rec lexer_list xs =
    match xs with 
    |x :: xs -> (
        match x with
        |"*" -> Star :: lexer_list xs
        |"|" -> Alternate :: lexer_list xs
        |"(" -> OpenPar :: lexer_list xs
        |")" -> ClosePar :: lexer_list xs
        |"[" -> OpenSqPar :: lexer_list xs
        |"]" -> CloseSqPar :: lexer_list xs
        |"-" -> Dash :: lexer_list xs
        |_ -> Letter :: lexer_list xs
    )
    |[] -> [EndOfInput]
    
let lexer input = lexer_list (string_to_list input);;

type exit = Success | Failure

let rec parser_machine xs ys =
    match xs, ys with
        |x::xs, y::ys -> (
            match y with
            |T(t) -> (
                if x = EndOfInput && t = EndOfInput then Success
                else if x = t then parser_machine xs ys
                else Failure
            )
            |N(n) -> (
                let zs = x :: xs in 
                match n with 
                |S -> parser_machine zs (N(A) :: T(EndOfInput) :: ys)
                |A -> parser_machine zs (N(C) :: N(A') :: ys)
                |A' -> (
                    if x = Alternate then parser_machine zs (T(Alternate) :: N(C) :: N(A') :: ys)
                    else parser_machine zs ys
                )
                |C -> parser_machine zs (N(K) :: N(C') :: ys)
                |C' -> ( 
                    if (x = OpenPar || x = Letter || x = OpenSqPar) then parser_machine zs (N(K) :: N(C') :: ys)
                    else parser_machine zs ys
                )
                |K -> parser_machine zs (N(F) :: N(K') :: ys)
                |K' -> (
                    if x = Star then parser_machine zs (T(Star) :: ys)
                    else parser_machine zs ys
                )
                |F -> (                    
                    if x = OpenPar then parser_machine zs (T(OpenPar) :: N(A) :: T(ClosePar) :: ys)
                    else if x = Letter then parser_machine zs (T(Letter) :: ys)
                    else (*if x = OpenSqPar*) (
                        try (
                            if List.nth xs 1 = Dash then 
                                parser_machine zs (T(OpenSqPar) :: T(Letter) :: T(Dash) :: T(Letter) :: T(CloseSqPar) :: ys)
                            else parser_machine zs (T(OpenSqPar) :: T(Letter) :: N(W) :: T(CloseSqPar) :: ys)
                        ) with _ -> Failure
                    )
                )
                |W -> (
                    if x = Letter then parser_machine zs (T(Letter) :: N(W) :: ys)
                    else parser_machine zs ys
                )
            )
        )
        |_ -> Failure

let parser input = parser_machine input [N(S)];; 

let run_compiler input = parser (lexer input);;

(* Lexer *)
lexer "ab(cd)";;
lexer "a[c-d]bc|(a|b*)*|[abc]e(gh)i*";;

(* Compiler *)
run_compiler "abc|(a|b*)*|de(gh)i*";;
run_compiler "(a|[a-d]*)*";;
run_compiler "[abd]cd|[ab]*";;

(* Invalid regular expressions *)
run_compiler "abc|(a|b*)*|de(ghi)|*";;
run_compiler "[abdcd|[ab]*";;
run_compiler "(a|[a--d]*)*";;