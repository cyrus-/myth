let s = Bool_decls.s ^ "

let bool_xor : bool -> bool -> bool |>
  { True => True => False
  ; True => False => True
  ; False => True => True
  ; False => False => False } = ?"
