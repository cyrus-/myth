module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
open Js_of_ocaml_tyxml.Tyxml_js
open Consts
open Lang

let example_input = "
type nat =
  | O
  | S of nat

type list =
  | Nil
  | Cons of nat * list

let list_stutter : list -> list |>
  { [] => []
  | [0] => [0;0]
  | [1;0] => [1;1;0;0]
  } = ?
"

let parse_input (input : string) : prog = 
  input |> Lexing.from_string |> Parser.prog Lexer.token

let typecheck_prog (p: prog) : prog =
  let _ = Tc.tc_prog p in
  p

let process_preamble ((ds, p):prog) =
  let (s, g)      = Tc.tc_prog (ds, p) in
  let env         = Eval.gen_init_env ds in
  let (x, t, es)  = p in
  let vs          = Eval.gen_init_evidence env es in
  let tree        = Rtree.create_rtree s g env t vs !match_count in
  (s, g, env, x, t, es, vs, tree)

exception CannotSynthesize

let synthesize_prog (p: prog) : decl = 
  let (s, _, env, x, t, _, _, tree) = process_preamble p in
  begin match Synth.synthesize s env tree with
  | Some e -> Translate.to_top_level x t e
  | None -> raise CannotSynthesize
  end

type model = {
  input_prog: string
}

let init_model = {
  input_prog = example_input
}

let view model = 
  let input_prog = model.input_prog in 
  let output = input_prog |> parse_input |> typecheck_prog |> synthesize_prog |> Pp.pp_decl in 
  let open Html5 in 
  div ~a:[a_class ["output"]] [txt output]

let _ = 
  JSUtil.listen_to_t
    Dom_html.Event.domContentLoaded
    Dom_html.document
    (fun _ -> 
      let model = init_model in 
      let container = JSUtil.forceGetElementById("container") in 
      let ui = To_dom.of_div(view model) in 
      Dom.appendChild container ui;
      JSUtil.log("hi"))
