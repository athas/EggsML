(* Hip Hop navnegenerator - http://heltnormalt.dk/truthfacts/2013/10/04 *)

(* Indlæs tilfældighedsgenerator *)
val gen = Random.newgen ()

local
  (* Giver et tilfældigt tal i intervallet [n,m) *)
  fun rand (n,m) = Random.range (n,m) gen

  (* Tager et tilfældigt element fra en liste *)
  fun randNth [] = raise Empty
    | randNth xs = List.nth(xs, rand (0, length xs))

  (* Funktioner til at lave præfikser + vælge funktion til tilfældigt mellemnavn *)
  fun big   () = ("Big",   randNth [ aedelmetal_valuta ])
  and lil   () = ("Lil",   randNth [ aedelmetal_valuta, cannabis_slang, tal_bogstav ])
  and young () = ("Young", randNth [ aedelmetal_valuta, skydevaaben  ])
  and fat   () = ("Fat",   randNth [ cannabis_slang, skydevaaben, tal_bogstav ])
  and mc    () = ("MC",    randNth [ skydevaaben, tal_bogstav ])
  and dj    () = ("DJ",    randNth [ skydevaaben, tal_bogstav ])
  and asap  () = ("ASAP",  randNth [ tal_bogstav ])

  (* Funktion til tilfældigt mellemnavn efterfulgt af tilfældigt efternavn *)
  and aedelmetal_valuta () = (randNth [ "Dollar", "Yen", "Gee", "Money", "Ca$h",
                                        "Gold", "Silver", "Platinum", "Bismuth" ],
                              randNth [ "Boy", "Master" ])
  and cannabis_slang    () = (randNth [ "Ganja", "Smokey", "Dope", "Weed" ],
                              randNth [ "Man", "Thug", "King" ])
  and skydevaaben       () = (randNth [ "Magnum", ".50", "High-Caliber", "Glock",
                                        "Machine Gun", "AK47", "Sniper" ],
                              randNth [ "Master", "Rock", "Rocky" ])
  and tal_bogstav       () = (randNth [ "Zero", "Number One", "IInd", "IIIrd", "Five",
                                        "Seven", "Nine", "A", "11th", "'D-12'",
                                        "Lucky 13", "F'in", "U-238" ],
                              randNth [ "Boy", "Thug", "Master", "King" ])

  (* Vigtigt: Udskift alle s/S med z/Z eller $ *)
  fun vigtigt s = String.map (fn #"s"  => randNth [#"z", #"$"]
                               | #"S"  => randNth [#"Z", #"$"]
                               | other => other) s
in
  (* Lav et tilfældigt navn. Sæt nogle gange "A.K.A." på *)
  fun navn () =
      let val (prefixS, middleF) = (randNth [big, lil, young, fat, mc, dj, asap]) ()
          val (middleS, suffixS) = middleF ()
          fun continue false = ""
            | continue true  = " A.K.A. " ^ navn ()
      in String.concat [ vigtigt prefixS, " ",
                         vigtigt middleS, " ",
                         vigtigt suffixS,
                         continue (randNth [true, false]) ]
      end
end

val _ = print (navn ())
