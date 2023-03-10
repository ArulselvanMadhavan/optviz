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
  <script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
  <script src="https://cdn.jsdelivr.net/npm/vega-lite@5"></script>
  <script src="https://cdn.jsdelivr.net/npm/vega-embed@6"></script>
      <script type="text/javascript" src="main.js"></script>
<style>
form.vega-bindings {
    display: flex;
    position: absolute;
    top: 0px;
    flex-direction:column;
}
</style>
    </head>
    <body>
      <h1>OPT visualizations</h1>
      <div id="app"></div>
      <div id="viz"></div>
    </body>
</html>
|}
;;

let csv_pattern = Base.String.Search_pattern.create ~case_sensitive:false "csv"
let json_pattern = Base.String.Search_pattern.create ~case_sensitive:false "json"
let png_pattern = Base.String.Search_pattern.create ~case_sensitive:false "png"

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
    | _ ->
      print_string uri;
      if Base.String.Search_pattern.matches csv_pattern uri
      then respond_file ~content_type:"text/csv" (Base.String.drop_prefix uri 1)
      else if Base.String.Search_pattern.matches json_pattern uri
      then respond_file ~content_type:"application/json" (Base.String.drop_prefix uri 1)
      else if Base.String.Search_pattern.matches png_pattern uri
      then respond_file ~content_type:"application/png" (Base.String.drop_prefix uri 1)
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
