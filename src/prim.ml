open Core.Std

type ('a, 's) t = ('s -> ('a * 's) option)
include Monad.Make2(struct
  type ('a, 's) t = ('s -> ('a * 's) option)

  let bind x g =
    (fun state ->
      match x state with
      | None -> None
      | Some (v, state') -> (g v) state')

  let return x : ('a, 's) t = (fun state -> Some (x, state))
  let map = `Define_using_bind
end)

let of_option = function Some x -> return x | None -> (fun _ -> None)
let get : ('s, 's) t = (fun state -> Some (state, state))
let put state' : (unit, 's) t = (fun state -> Some ((), state'))

let check test =
  (fun state ->
    match State.read state with
    | None -> None
    | Some ch ->
        match test ch with
        | None -> None
        | Some res -> Some (res, State.next state))

let choice a b =
  (fun state ->
    match a state with
    | None -> b state
    | s -> s
  )

let (<|>) a b = choice a b

let many (combinator : ('a, 's) t) : ('a list, 's) t =
  let rec loop acc : ('a list, 's) t = (fun state ->
    match combinator state with
      | None -> Some (acc, state)
      | Some (v, state') -> loop (v :: acc) state') in
  loop [] >>| List.rev

let%monad many1 combinator =
  p <- combinator;
  ps <- many combinator;
  return (p :: ps)

let mempty = (fun state -> None)

let run parser_comb str =
  match parser_comb (State.init str) with
  | None -> None
  | Some (v, _) -> Some v
