open Core.Std

type t = { head : char option; next_ : t Lazy.t }
[@@deriving show]

let read (self : t) = self.head

let rec create chars =
  match chars with
  | [] -> { head = None; next_ = lazy (create []) }
  | x :: xs -> { head = Some x; next_ = lazy (create xs) }

let init str = create (String.to_list str)
let next (self : t) = Lazy.force self.next_

