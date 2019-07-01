module Js = Js_of_ocaml.Js
module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Ev = Dom_html.Event

let log x = Js_of_ocaml.Firebug.console##log(x)

let forceGetElementById id =
  let doc = Dom_html.document in 
  Js.Opt.get
    (doc##getElementById(Js.string(id)))
    (fun _ -> 
      log(id);
      assert(false))

let listen_to ev elem f =
  Dom_html.addEventListener elem ev (Dom_html.handler f) Js._false

let listen_to_t ev elem f =
  listen_to
    ev
    elem
    (fun evt -> 
      f(evt);
      Js._true)

