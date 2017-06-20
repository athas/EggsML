val grammar = Grammar [
  >> ("start", [[S "greet", S "delimiter", T " ", S "recruit message"]]),

  >> ("delimiter", [[T "!"],
                    [T ":"]]),

  >> ("greet", [[S "name"],
                [S "greet prefix", T " ", S "name"]]),
  >> ("greet prefix", [[T "Du"],
                       [T "Hør"],
                       [T "Hva'"]]),

  >> ("name", [[S "name prefix", S "name suffix"]]),
  >> ("name prefix", [[T "Internet"],
                      [T "Spindel"],
                      [T "Verdens"],
                      [T "Web"]]),
  >> ("name suffix", [[T "bruger"],
                      [T "borger"],
                      [T "person"],
                      [T "personlighed"],
                      [T "kendis"],
                      [T "fidus"]]),

  >> ("recruit message", [[T "Kan jeg ", S "friste", T " med en ", S "lækker", T " <EGGS>?"],
                          [T "Kan du ", S "friste", T "s med til <EGGS>?"]]),

  >> ("friste", [[T "friste"],
                 [T "lokke"]]),

  >> ("lækker", [[T "lækker"],
                 [T "skøn"],
                 [T "uovertruffen"],
                 [T "middelmådig"],
                 [T "gennemsnitlig"],
                 [T "fin"]])
];
