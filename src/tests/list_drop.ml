let s = "
type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let list_drop : list -> nat -> list |>
  { [] => ( 0 => []
          | 1 => []
          | 2 => [] )
  | [0] => ( 0 => [0]
           | 1 => [] )
  | [1] => ( 0 => [1]
           | 1 => [] )
  | [1; 0] => ( 0 => [1; 0]
              | 1 => [0]
              | 2 => [] )
  | [0; 1] => ( 0 => [0; 1]
              | 1 => [1]
              | 2 => [] )
  } = ?"
