let s = "
type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let list_length : list -> nat |>
  { [] => 0
  | [0] => 1
  | [0;0] => 2 } = ?"
