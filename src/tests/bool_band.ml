let s = Bool_decls.s ^ "

let bool_band : bool -> bool -> bool |>
  { True => True => True
  ; True => False => False
  ; False => True => False
  ; False => False => False } = ?"
