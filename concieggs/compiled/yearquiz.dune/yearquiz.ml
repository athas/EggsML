(* Helper program for the årquiz game. *)

open Core
open Async

let base_url = "https://da.wikipedia.org/w/api.php?action=parse&format=json&prop=parsetree&page="

let rec remove_link_artefacts string start =
  let rest () = String.sub string ~pos:start ~len:(String.length string - start) in
  match String.substr_index string ~pos:start ~pattern:"[[" with
  | None -> rest ()
  | Some a -> begin
      match String.substr_index string ~pos:a ~pattern:"]]" with
      | None -> rest ()
      | Some c -> begin
          match String.substr_index string ~pos:a ~pattern:"|" with
          | Some b when a < b && b < c ->
             String.sub string ~pos:start ~len:(a - start) ^ String.sub string ~pos:(b + 1) ~len:(c - (b + 1)) ^ remove_link_artefacts string c
          | _ ->
             String.sub string ~pos:start ~len:(c + 2 - start) ^ remove_link_artefacts string (c + 2)
        end
    end

let rec remove_ext_artefacts string start =
  match String.substr_index string ~pos:start ~pattern:"<ext" with
  | Some ext_start -> begin
      match String.substr_index string ~pos:ext_start ~pattern:"</ext>" with
      | Some ext_end ->
         String.sub string ~pos:start ~len:ext_start
         ^ remove_ext_artefacts string (ext_end + String.length "</ext>")
      | None ->
         String.sub string ~pos:start ~len:(String.length string - start)
    end
  | None ->
     String.sub string ~pos:start ~len:(String.length string - start)

let parse_events text header =
  match String.substr_index text ~pattern:header with
  | Some events_start ->
     let events_end =
       match String.substr_index text ~pos:events_start ~pattern:"\n\n<h level=\"2\"" with
       | Some p -> p
       | None -> String.substr_index_exn text ~pos:events_start ~pattern:"\n\n"
     in
     let rec find_events cur_start = begin
         match String.substr_index text ~pos:cur_start ~pattern:"\n* " with
         | Some sub_start when sub_start < events_end ->
            let sub_end = String.substr_index_exn text ~pos:(sub_start + 1) ~pattern:"\n" in
            let line = String.sub text ~pos:(sub_start + 3) ~len:(sub_end - sub_start - 3) in
            let line = remove_link_artefacts line 0 in
            let line = remove_ext_artefacts line 0 in
            let date_dash1 = "]] - " in
            let date_dash2 = "]] – " in
            let (date_dash, dash) =
              match (String.substr_index line ~pattern:date_dash1,
                     String.substr_index line ~pattern:date_dash2) with
              | (dash1, None) ->
                 (date_dash1, dash1)
              | (None, dash2) ->
                 (date_dash2, dash2)
              | (Some dash1, Some dash2) ->
                 if dash1 < dash2
                 then (date_dash1, Some dash1)
                 else (date_dash2, Some dash2)
            in
            let line =
              match dash with
              | Some p ->
                 String.sub line ~pos:(p + String.length date_dash)
                   ~len:(String.length line - (p + String.length date_dash))
              | None ->
                 line
            in
            let line = String.substr_replace_all (String.substr_replace_all (String.substr_replace_all (String.substr_replace_all line ~pattern:"[[" ~with_:"") ~pattern:"]]" ~with_:"") ~pattern:"'''" ~with_:"**") ~pattern:"''" ~with_:"*"
            in line :: find_events sub_end
         | _ ->
            []
       end
     in find_events events_start
  | _ ->
     []

let ok_sport s =
  String.is_substring s ~substring:"vinde"
  || String.is_substring s ~substring:"tabe"
  || String.is_substring s ~substring:"besejre"
  || String.is_substring s ~substring:"forsvare"
  || String.is_substring s ~substring:"holde"
  || String.is_substring s ~substring:"stifte"
  || String.is_substring s ~substring:"anlægge"
  || String.is_substring s ~substring:"sætte"

let ok_musik s =
  String.is_substring s ~substring:"vinde"
  || String.is_substring s ~substring:"tabe"
  || String.is_substring s ~substring:"hitte"
  || String.is_substring s ~substring:"danne"
  || String.is_substring s ~substring:"udgive"
  || String.is_substring s ~substring:"udsende"
  || String.is_substring s ~substring:"spille"

let parse text =
  let begivenheder = parse_events text "== Begivenheder ==" in
  let sport = parse_events text "== Sport ==" in
  let sport = List.filter ~f:ok_sport sport in
  let musik = parse_events text "== Musik ==" in
  let musik = List.filter ~f:ok_musik musik in
  begivenheder @ sport @ musik


let shuffle d =
  let module RandomPair =
    struct
      include Tuple.Comparable (Int) (String)
    end
  in
  let nd = List.map ~f:(fun c -> (Random.bits (), c)) d in
  let sond = List.sort ~compare:RandomPair.compare nd in
  List.map ~f:snd sond

let rec find_year_facts () =
  Random.self_init ();
  let year = 0 + Random.int 2020 in (* XXX: Better distribution *)
  let url = base_url ^ string_of_int year in
  Cohttp_async.Client.get (Uri.of_string url) >>= fun (_, body) ->
  Cohttp_async.Body.to_string body >>= fun json ->
  let open Yojson.Basic.Util in
  let text = Yojson.Basic.from_string json |> member "parse" |> member "parsetree" |> member "*" |> to_string in
  let text = String.substr_replace_all text ~pattern:(string_of_int year) ~with_:"XXXX" in
  let lines = parse text in
  if List.length lines = 0 || (List.length lines = 1 && String.length (List.nth_exn lines 0) = 0)
  then find_year_facts ()
  else let lines = shuffle lines in
       let lines = List.init (min 3 (List.length lines)) ~f:(List.nth_exn lines) in
       printf "%d %d\n" year (List.length lines);
       List.iteri ~f:(printf "%d. %s\n") lines;
       Deferred.unit

(* This seems kind of overcomplicated. *)
let () =
   don't_wait_for (find_year_facts () >>| fun _ -> shutdown 0);
   never_returns (Scheduler.go ())
