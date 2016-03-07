open Core.Std

let token test = Prim.check test
let verify test = token (fun ch -> if test ch then Some ch else None)

let multiple combinators =
  let f = List.fold_right combinators ~init:(fun str -> Prim.return str) ~f:(fun test acc ->
    (fun str -> Prim.(test >>= (fun ch -> acc (str ^ String.of_char ch))))
  ) in f ""

let repeat combinators =
  let f = List.fold_right combinators ~init:(fun str -> Prim.return str) ~f:(fun test acc ->
    (fun str -> Prim.(test >>= (fun ch -> acc (str ^ String.of_char ch))))
  ) in f ""

let char_ ch = verify (fun ch' -> ch = ch')
let one_of str = verify (String.contains str)

let digit     = verify Char.is_digit
let lowercase = verify Char.is_lowercase
let alphabet  = verify Char.is_alpha
let alphanum  = verify Char.is_alphanum
let space     = verify Char.is_whitespace

let digits      = Prim.(many1 digit >>| String.of_char_list >>| Int.of_string)
let lowercases  = Prim.(many1 lowercase >>| String.of_char_list)
let alphabets   = Prim.(many1 alphabet >>| String.of_char_list)
let alphanums   = Prim.(many1 alphanum >>| String.of_char_list)

let anyChar = verify (fun _ -> true)

let one_of str =
  List.fold (String.to_list str) ~init:Prim.mempty ~f:(fun acc ch -> Prim.(acc <|> char_ ch))

let string_ str =
  multiple (List.map (String.to_list str) ~f:char_)
