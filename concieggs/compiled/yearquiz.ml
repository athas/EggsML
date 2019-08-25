(* Helper program for the årquiz game. *)

let base_url = "https://da.wikipedia.org/w/api.php?action=parse&format=json&prop=parsetree&page="

(* Why is this not in some standard library? *)
let read_all fd =
  let chunk = 1024 in
  let rec read buffer size offset =
    let used = input fd buffer offset chunk
    in if used <= 0 then Bytes.extend buffer 0 (-chunk)
       else read (Bytes.extend buffer 0 used) (size + used) (offset + used)
  in Bytes.to_string (read (Bytes.create chunk) chunk 0)

let rec find_substring string start sub =
  if start + String.length sub <= String.length string
  then let sub' = String.sub string start (String.length sub) in
       if String.equal sub' sub
       then start
       else find_substring string (start + 1) sub
  else -1

(* This is just plain inefficient. *)
let rec replace string start old repl =
  if start + String.length old <= String.length string
  then let sub = String.sub string start (String.length old) in
       if String.equal sub old
       then repl ^ replace string (start + String.length old) old repl
       else String.sub string start 1 ^ replace string (start + 1) old repl
  else String.sub string start (String.length string - start)

let rec remove_link_artefacts string start =
  let rest () = String.sub string start (String.length string - start) in
  let a = find_substring string start "[[" in
  if a != -1
  then let c = find_substring string a "]]" in
       if c != -1
       then let b = find_substring string a "|" in
            if b != -1 && a < b && b < c
            then String.sub string start (a - start) ^ String.sub string (b + 1) (c - (b + 1)) ^ remove_link_artefacts string c
            else String.sub string start (c + 2 - start) ^ remove_link_artefacts string (c + 2)
       else rest ()
  else rest ()

let rec remove_ext_artefacts string start =
  let ext_start = find_substring string start "<ext" in
  let ext_end = if ext_start != -1
                then find_substring string ext_start "</ext>"
                else -1 in
  if ext_end != -1
  then String.sub string start ext_start
       ^ remove_ext_artefacts string (ext_end + String.length "</ext>")
  else String.sub string start (String.length string - start)

let parse_events text header =
  let events_start = find_substring text 0 header in
  if events_start != -1
  then let events_end = find_substring text events_start "\n\n<h level=\"2\"" in
       let events_end = if events_end == -1 then find_substring text events_start "\n\n" else events_end in
       let rec find_events cur_start =
         let sub_start = find_substring text cur_start "\n* " in
         if sub_start != -1 && sub_start < events_end
         then let sub_end = find_substring text (sub_start + 1) "\n" in
              let line = String.sub text (sub_start + 3) (sub_end - sub_start - 3) in
              let line = remove_link_artefacts line 0 in
              let line = remove_ext_artefacts line 0 in
              let date_dash1 = "]] - " in
              let date_dash2 = "]] – " in
              let dash1 = find_substring line 0 date_dash1 in
              let dash2 = find_substring line 0 date_dash2 in
              let (date_dash, dash) = if dash2 == -1
                                      then (date_dash1, dash1)
                                      else if dash1 == -1
                                      then (date_dash2, dash2)
                                      else if dash1 < dash2
                                      then (date_dash1, dash1)
                                      else (date_dash2, dash2) in
              let line = if dash != -1
                         then String.sub line (dash + String.length date_dash)
                                (String.length line - (dash + String.length date_dash))
                         else line in
              let line = replace (replace (replace (replace line 0 "[[" "") 0 "]]" "") 0 "'''" "**") 0 "''" "*"
              in line :: find_events sub_end
         else []
       in find_events events_start
  else []

let ok_sport s =
  find_substring s 0 "vinde" != -1
  || find_substring s 0 "tabe" != -1
  || find_substring s 0 "besejre" != -1
  || find_substring s 0 "forsvare" != -1
  || find_substring s 0 "holde" != -1
  || find_substring s 0 "stifte" != -1
  || find_substring s 0 "anlægge" != -1
  || find_substring s 0 "sætte" != -1

let parse text =
  let begivenheder = parse_events text "== Begivenheder ==" in
  let sport = parse_events text "== Sport ==" in
  let sport = List.filter ok_sport sport in
  begivenheder @ sport

(* https://stackoverflow.com/a/15095713 *)
let shuffle d =
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond

let rec find_year_facts () =
  Random.self_init ();
  let year = 1908 in (* 1700 + Random.int 200 in (* XXX: Better distribution *) *)
  let url = base_url ^ string_of_int year in
  let text_in = Unix.open_process_in ("python3 -c 'import urllib.request; import json; import html; print(html.unescape(json.load(urllib.request.urlopen(\"" ^ url ^ "\"))[\"parse\"][\"parsetree\"][\"*\"]))'") in
  let text = read_all text_in in
  close_in text_in;
  let lines = parse text in
  if List.length lines == 0
  then find_year_facts ()
  else let lines = shuffle lines in
       let lines = List.init (min 3 (List.length lines)) (List.nth lines)
       in Printf.printf "%d %d\n" year (List.length lines);
          List.mapi (Printf.printf "%d. %s\n") lines


let main () = find_year_facts ()
;;

main ()
