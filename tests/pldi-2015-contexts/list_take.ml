#use "context.decls"

let list_take : nat -> list -> list |>
  { 0 => ( [] => []
         | [1] => []
         | [0;1] => []
         | [1;0;1] => [] )
  | 1 => ( []    => []
         | [1]   => [1]
         | [0;1] => [0]
         | [1;0;1] => [1] )
  | 2 => ( []    => []
         | [1]   => [1]
         | [0;1] => [0;1]
         | [1;0;1] => [1;0] )
  } = ?