open Core_kernel
module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
open Js_of_ocaml_tyxml.Tyxml_js
open Consts
open Lang

let examples = [
  ("bool_band", Bool_band.s);
  ("bool_bor", Bool_bor.s);
  ("bool_impl", Bool_impl.s);
  ("bool_neg", Bool_neg.s);
  ("bool_xor", Bool_xor.s);

  ("list_append", List_append.s);
  ("list_compress", List_compress.s);
  ("list_concat", List_concat.s);
  ("list_drop", List_drop.s);
  ("list_even_parity", List_even_parity.s); 
  ("list_filter", List_filter.s);
  ("list_fold", List_fold.s);
  ("list_hd", List_hd.s);
  ("list_inc", List_inc.s);
  ("list_last", List_last.s);
  ("list_length", List_length.s);
  ("list_map", List_map.s);
  ("list_nth", List_nth.s);
  ("list_pairwise_swap", List_pairwise_swap.s);
  ("list_rev_append", List_rev_append.s);
  ("list_rev_fold", List_rev_fold.s);
  (* ("list_rev_snoc", List_rev_snoc.s); *) 
  ("list_rev_tailcall", List_rev_tailcall.s);
  ("list_snoc", List_snoc.s); 
  ("list_sort_sorted_insert", List_sort_sorted_insert.s);
  ("list_sorted_insert", List_sorted_insert.s);
  ("list_stutter", List_stutter.s);
  ("list_sum", List_sum.s);
  ("list_take", List_take.s);
  ("list_tl", List_tl.s); 
  
  ("nat_iseven", Nat_iseven.s);
  ("nat_max", Nat_max.s);
  ("nat_pred", Nat_pred.s);
  ("nat_sum", Nat_sum.s); 
  
  ("tree_binsert", Tree_binsert.s);
  ("tree_collect_leaves", Tree_collect_leaves.s);
  ("tree_count_leaves", Tree_count_leaves.s);
  ("tree_count_nodes", Tree_count_nodes.s);
  ("tree_inorder", Tree_inorder.s);
  ("tree_map", Tree_map.s);
  ("tree_nodes_at_level", Tree_nodes_at_level.s);
  ("tree_postorder", Tree_postorder.s);
  ("tree_preorder", Tree_preorder.s); 
]

type example_data = {
  name: string;
  input: string;
  output: string;
  time: float
}

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

let sum ns = 
  List.fold ns 
    ~init:0.0
    ~f:(fun total n -> total +. n)

let average ns = 
  sum ns /. float_of_int(List.length ns)

let time_fun f = 
  let start_time = Sys.time () in 
  let output = f () in 
  let end_time = Sys.time() in 
  let time = end_time -. start_time in 
  (output, time) 

let num_repetitions = 1
let benchmark f = 
  let data = 
    List.init 
      num_repetitions
      ~f:(fun _ -> time_fun f) in 
  let times = List.map data ~f:(fun (_, time) -> time) in 
  let time = average(times) in 
  match List.nth data 0 with None -> failwith ("num_repetitions must be > 0")
  | Some (output, _) -> (output, time)

let generate_data (name, input) : example_data = 
  JSUtil.log("test 1");
  let (output, time) = benchmark (fun () ->  
     input |> parse_input |> typecheck_prog |> synthesize_prog |> Pp.pp_decl
  ) in 
  {
    name = name;
    input = input;
    output = output;
    time = time
  }

let data = 
  List.map 
    examples
    ~f:generate_data

type model = example_data list

let init_model = data

let view model = 
  let open Html5 in 
  table ~a:[a_id "benchmarks"]  
    (List.map model ~f:(fun item -> 
      let the_name = item.name in 
      let the_time = item.time in 
      tr [td [txt the_name]; td [txt (string_of_float the_time)]]))

let _ = 
  JSUtil.listen_to_t
    Dom_html.Event.domContentLoaded
    Dom_html.document
    (fun _ -> 
      let model = init_model in 
      let container = JSUtil.forceGetElementById("container") in 
      let ui = To_dom.of_table(view model) in 
      Dom.appendChild container ui;
      JSUtil.log("hi"))
