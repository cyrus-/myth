let s = Bool_decls.s ^ "

let bool_bor : bool -> bool -> bool |>
  { True => True => True
  ; True => False => True
  ; False => True => True
  ; False => False => False } = ?" 
