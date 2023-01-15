open Cohttp_lwt_unix

let default_port = 9990

let respond_string ~content_type ~status ~body ?headers =
  let headers = Cohttp.Header.add_opt headers "Content-Type" content_type in
  Server.respond_string ~headers ~status ~body
;;

let respond_file ~content_type ?headers s =
  let headers = Cohttp.Header.add_opt headers "Content-Type" content_type in
  Server.respond_file ~headers ~fname:s ()
;;

let html =
  {|
<!DOCTYPE html>
<html lang="en">
    <head>
      <script type="text/javascript" src="main.js"></script>
    </head>
    <body>
      <h1>OPT visualizations</h1>
      <div id="app"></div>
    </body>
</html>
|}
;;

let read_file filename = Stdlib.In_channel.with_open_text filename In_channel.input_all

let body spec =
  {|<body
     <div id="app"></div>
     <div id="vis"></div>
     <script>vegaEmbed("#vis",|}
  ^ spec
  ^ {|);</script>
   </body>|}
;;

let head =
  {|
    <head>
<script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
<script src="https://cdn.jsdelivr.net/npm/vega-lite@5"></script>
<script src="https://cdn.jsdelivr.net/npm/vega-embed@6"></script>
    </head> 
|}
;;

let gen_html spec = "<html>" ^ head ^ body spec ^ "</html>"
let csv_pattern = Base.String.Search_pattern.create ~case_sensitive:false "csv"

let server ~port =
  let callback _conn req _body =
    let uri = req |> Request.uri |> Uri.path in
    match uri with
    | "" | "/" | "/index.html" ->
      respond_string ~content_type:"text/html" ~status:`OK ~body:html ()
    | "/main.js" ->
      respond_string
        ~content_type:"application/javascript"
        ~status:`OK
        ~body:Embedded_files.main_dot_bc_dot_js
        ()
    | "/opt125m_out_range.html" ->
      let _ = Base.String.Search_pattern.matches csv_pattern uri in
      respond_string
        ~content_type:"text/html"
        ~status:`OK
        ~body:(read_file "recipe/opt125m_out_range.vg.json" |> gen_html)
        ()
    | _ ->
      if Base.String.Search_pattern.matches csv_pattern uri
      then respond_file ~content_type:"text/csv" (Base.String.drop_prefix uri 1)
      else respond_string ~content_type:"text/html" ~status:`Not_found ~body:"" ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
;;

let main ~port =
  Printf.printf "Running server at %d\n" port;
  flush stdout;
  Lwt_main.run @@ server ~port
;;

let () = main ~port:default_port
