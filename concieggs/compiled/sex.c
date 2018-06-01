/*sex_da.c*/

/* Original author unknown.  Presumably this is public domain by now.
 * If you are the original author or know the original author, please
 * contact <freebsd@spatula.net>
 *
 * Orphan code cleaned up a bit by Nick Johnson <freebsd@spatula.net>
 * Completely rewrote how word wrapping works and added -w flag.
 *
 * Translated to danish by Troels Henriksen <athas@sigkill.dk>
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

static char     *faster[] = {
  "\"Lad spillet begynde\"",             "\"Åh gud!\"",
  "\"Nej, ikke det!\"",                        "\"Endelig!\"",
  "\"Det er det forkerte hul!\"",                  "\"Er det det hele?\"",
  "\"Fandens, strømerne!\"",             "\"Jeg havde aldrig drømt om at det kunne være\"",
  "\"Hvis jeg gør det vil du ikke respektere mig!\"",   "\"Nu!\"",
  "\"Sesam, luk dig op!\"",                     "\"EMR!\"",
  "\"Igen!\"",                           "\"Hurtigere!\"",
  "\"Hårdere!\"",                          "\"Hjælp!\"",
  "\"Tag mig hårdere!\"",                  "\"Er den inde endnu?\"",
  "\"Du er ikke min far!\"",            "\"Hr. læge, er det nu propert?\"",
  "\"Nej, nej, gør det ved guldfisken!\"",         "\"Hellige batmobil, Batman!\"",
  "\"Han er død, han er død!\"",            "\"Tag mig, Robert!\"",
  "\"Jeg er liberal!\"",                "\"Stik fire fingre ind!\"",
  "\"Sikke en elsker!\"",                    "\"Sig noget beskidt til mig, svin!\"",
  "\"Loftet trænger til maling,\"",      "\"Sug hårdere!\"",
  "\"Dyrene vil høre os!\"",           "\"Ikke offentligt!\"",
  "\"Hvad hvis Spectrum hører os?\"",   "\"Tag nu det flag, for pokker!\"",
  "\"Mit /dev/input har plads til det hele!\"",  "\"Ohøj, hund!\"",
  "\"Splitte mine bramsejl!\"", "\"Hov stop, jeg tror jeg flækker,\"",
  "\"Skinke!\"",
};

static char     *said[] = {
  "brølte",              "gøede",           "kvækkede",
  "snerrede",            "stønnede",        "udklemte",
  "brummede",            "grinte",          "udspyede",
  "sukkede",             "ejakulerede",     "hostede",
  "stammede",            "peb",             "hvinede",
  "pev",                 "sagde",           "argumenterede",
  "græd",                "skreg",           "råbte",
  "skinkede",
};

static char     *the[] = {
  "den",
};

static char     *fadj[] = {
  "sovsede",              "eftersøgte",           "uheldige",
  "lystige",              "niårige",              "tyrenakkede",
  "bisexuelle",           "fantastiske",          "søde",
  "nymfomanske",          "bred-bækkenede",       "fregnede",
  "45-årige",             "hvidhårede",           "fede",
  "hellige",              "blinde",               "skæggede",
  "blåøjede",             "liderlige",            "venlige",
  "klaverspillende",      "øreslikkende",         "menstruerende",
  "røvslikkende",         "lesbiske",             "behårede",
  "skinkende",
};


static char     *female[] = {
  "sæk",                  "dagplejemor",          "kvinde",
  "grevinde",             "transvestit",          "nymfoman",
  "jomfru",               "læderfreak",           "drag queen",
  "afpillede nonne",      "bisexuelle dværg",     "cheerleader",
  "kontorassistent",      "sexuelle afviger",     "regeringsagent",
  "småpige",              "ceremonipingvin",      "femme fatale",
  "chefdatter",           "byggearbejder",        "pølsemisbruger",
  "sekretær",             "folketingsassistent",  "bedstemor",
  "pingvin",              "tyske hyrde",          "stewardesse",
  "tjenerinde",           "prostituerede",        "slikmutter",
  "truckertøs",           "mor til Spectrum",     "trunte",
  "sæddunk",              "feltmadras",           "dronning",
  "prinsesse",            "malkepige",            "skønjomfru",
  "sygeplejeske",         "matematiklærerinde",   "frøken",
  "skolepige",            "sjuft til Lindbo",     "skinke",
};

static char     *asthe[] = {
  "da den", "idet den", "mens den", "kort før den"
};

static char     *madjec[] = {
  "aggressive",           "savlende",             "umættelige",
  "gale",                 "sataniske",            "korpulente",
  "næsepillende",         "hundebelurerende",     "dryppende",
  "bredskuldrede",        "oralfikserede",        "veludrustede",
  "ufatteligt veludrustede",    "godt udseende",  "hjernedøde",
  "ufatteligt stive",     "trebenede",            "dampende",
  "kvindeudklædte",       "stive",                "sexgale",
  "vanvittige",           "måtteslikkende",       "vilde",
  "virile",               "ekstreme",             "cykelsædesniffende",
  "svenske",              "sortsmuskede",         "skinkende",
};

static char     *male[] = {
  "redneck",              "kamel",                "spytslikker",
  "ærkegreve",            "dværg",                "medarbejder",
  "store dansker",        "hingst",               "slambert",
  "elektriske ål",        "paranoide",            "ulækre gamle mand",
  "homofile tigger",      "friar",                "advokat",
  "hårrodsfetishist",     "pæne præst",           "kyllingeelsker",
  "homoflamingo",         "eks-cølibate",         "stofsniffer",
  "kjolemand",            "konstruktionsarbejder","frisør",
  "tandlæge",             "dommer",               "socialmedarbejder",
  "viking",               "hest",                 "tv-reparatør",
  "pizzaudbringer",       "pave",                 "Fader",
  "læge",                 "gynækelog",            "Batman",
  "skinke",               "landsforræder",        "svin",
};

static char     *diddled[] = {
  "snød",                 "fortærrede",           "græmsede",
  "mundede",              "tungede",              "tævede",
  "indstilte",            "misbrugte",            "forurenede",
  "gødede",               "gennemborede",         "omsorgsfuldt voldtog",
  "hammerede",            "bad",                  "tungeslaskede",
  "sugede",               "kneppede",             "gned",
  "hævnskneppede",        "onanerede med",        "sluprede",
  "ferniserede",          "kalibrerede",          "pulede",
  "indstilte",            "blitzkrieg'ede",       "desavou'erede",
  "skinkede",
};

char *her[] = {
  "hendes",
};

static char     *titadj[] = {
  "alabaste",             "lyserøde",             "cremede",
  "rødmossede",           "fugtige",              "dunkende",
  "saftige",              "hivende",              "spændte",
  "elefantiske",          "sukkulente",           "skælvende",
  "rosa",                 "kugleformede",         "opsvulmede",
  "dinglende",            "blodige",              "skævvredne",
  "dryppende",            "osende",               "faste",
  "hængende",             "muskuløse",            "kødfyldte",
  "faste",                "spidse",               "skinkede",
};

static char     *knockers[] = {
  "glober",               "meloner",              "højdedrag",
  "skud",                 "gajoler",              "lunger",
  "forhøjninger",         "skatte",               "brød",
  "patter",               "vestibule",            "armhuler",
  "bryster",              "jader",                "albuer",
  "øjne",                 "lungehylstre",         "kasser",
  "udposninger",          "forlys",               "fastpladelagre",
  "støddæmpere",          "knæ",                  "spejlæg",
  "baller",               "stødpuder",            "øreflipper",
  "skinker",              "intuitive brugerflade", 
};

char *and[] = {
  "og", "hvorefter han"
};

static char     *thrust[] = {
  "kastede",              "stødte",               "klemte",
  "hamrede",              "førte",                "listede",
  "gled",                 "knaldede",             "maste",
  "proppede",             "dyppede",              "avancerede",
  "vædrede",              "indførte",             "skubbede",
  "fyldte",               "slæbte",               "overførte",
  "rev",                  "tvang",                "puffede",
  "indtog",               "allokerede",           "implementerede",
  "skinkede",
};

static char     *his[] = {
  "sin",
};

static char     *dongadj[] = {
  "bristende",            "lagte",                "glitrende",
  "svulmende",            "velsignede",           "lilla",
  "sviende",              "ophævede",             "rigide",
  "hærgende",             "vortede",              "dampende",
  "rygende",              "snabellignende",       "frådende",
  "sprøjtende",           "svinende",             "forlorne",
  "årede",                "forædte",              "hestelignende",
  "pulserende",           "enorme",               "afgrunds-kløvende",
  "slangelignende",       "kurvede",              "stålindfattede",
  "glasbesatte",          "knudrede",             "kirurgisk modificerede",
  "metalforstærkede",     "ømme",                 "hastigt skrumpende",
  "hævede",               "minimale",             "ranglede",
  "skinkende",
};

static char     *dong[] = {
  "indtrænger",           "gren",                 "stub",
  "lem",                  "kødpølse",             "majestæt",
  "bovspryd",             "gummiged" ,            "damphammer",
  "ladestok",             "torsk",                "joystick",
  "vejviser",             "slambert",             "motor",
  "motivation",           "kødroulade",           "stempel",
  "svupper",              "redskab",              "mandom",
  "slikkepind",           "nyreprikker",          "lysestage",
  "bandit",               "arm",                  "testikel",
  "kugle",                "finger",               "fod",
  "tunge",                "tissemand",            "enøjede monster",
  "kødfløjte",            "midterste ben",        "diller",
  "gearstang",            "penis",                "holdkæftbolsje",
  "manuele styrepind",    "anaconda",             "python",
  "dolk",                 "kleine Führer",        "skinke",
  "flagstang",            "vatterpas",            "ynglekæp",
  "messingstang",         "budbringer",           "Ethernet-kabel",
  "scepter",              "sædpumpe",             "parerstang",
  "lynafleder",           "hobbyobjekt",          "træningsapparat",
};

static char     *intoher[] = {
  "ind i hendes", "efter hendes"
};

static char     *twatadj[] = {
  "pulserende",           "sultende",             "glubrende",
  "bankende",             "vidtåbne",             "savlende",
  "indbydende",           "mættede",              "smaskende",
  "spindelvævsbesatte",   "desperate",            "slubrende",
  "glinsende",            "dryppende",            "arrede",
  "porøse",               "milde",                "lyserøde",
  "støvede",              "stramme",              "ildelugtende",
  "fugtige",              "slappe",               "stinkende",
  "tandløse",             "velslidte",            "hjulspors-afmærkede",
  "halvspiste",           "sammentrukne",         "velbesøgte",
  "taknemmelige",         "rådnende",             "tandbesatte",
  "skinkende",            "fantastiske",
};

static char     *twat[] = {
  "sump.",                "honningkrukke.",       "syltetøjsglas.",
  "smørkasse.",           "pelsburger.",          "kirsebærstærte.",
  "sprække.",             "revne.",               "hul.",
  "cockpit.",             "hule.",                "fordybning.",
  "helligste hellige.",   "skæggede østers.",     "kontinentalafgrund.",
  "paradisiske dal.",     "røde flodbund.",       "lysthus.",
  "babyfabrik.",          "palads.",              "røv.",
  "rosenskud.",           "svælg.",               "øjenåbning.",
  "svedkanal.",           "indre øre.",           "kropsåbning.",
  "operationsar.",        "åbne sår.",            "navle.",
  "mund.",                "næse.",                "fisse.",
  "/dev/null.",           "portal til helvede.",  "mellemgulvsmund.",
  "Cocio-dåse.",          "begivenhedshorizont.", "fællesnævner.",
  "skinke.",              "bagdør.",              "svingdør.",
  "vandrehal.",           "hundredårskrigsstarter.",   "VGA-port.",
  "nødudgang.",
};

struct table {
  char    **item;
  short   len;
};

typedef struct table    TABLE;
#define SZ(a)           sizeof(a) / sizeof(char *)

TABLE   list[] = {
  {faster,         SZ(faster)},     {said,           SZ(said)},
  {the,            SZ(the)},        {fadj,           SZ(fadj)},
  {female,         SZ(female)},     {asthe,          SZ(asthe)},
  {madjec,         SZ(madjec)},     {male,           SZ(male)},
  {diddled,        SZ(diddled)},    {her,            SZ(her)},
  {titadj,         SZ(titadj)},     {knockers,       SZ(knockers)},
  {and,            SZ(and)},        {thrust,         SZ(thrust)},
  {his,            SZ(his)},        {dongadj,        SZ(dongadj)},
  {dong,           SZ(dong)},       {intoher,        SZ(intoher)},
  {twatadj,        SZ(twatadj)},    {twat,           SZ(twat)},
  {(char **)NULL,  (short)0},
};

#define LLINE   50
static short    lwidth;
static short    wraplen;

int main(int argc, char **argv)
{
  register TABLE  *ttp;
  register char   *cp;
  int     getpid();
  long     now;
  char buffer[2048];
  int pos, lastword;

  wraplen = 0;
  if (argc > 1) {
    if (!strcmp(argv[1],"-w")) {
      if (argc == 3) {
        wraplen = atoi(argv[2]);
      } else {
        wraplen = LLINE;
      }
    }
  }

  now = time(&now) / random();
  srandom(getpid() + (int)((now >> 16) + now + time(&now)));

  pos = lastword = 0;
  for (ttp = list;ttp->item;++ttp,++lwidth) {
    for (cp = ttp->len > 1 ? ttp->item[random() % ttp->len] :
           *ttp->item;*cp;++cp,++lwidth) {
      buffer[pos] = *cp;
      if ((wraplen > 0) && (lwidth >= wraplen)) {
        buffer[lastword] = '\n';
        lwidth = pos - lastword;
      }
      if (isspace(*cp)) {
        lastword = pos;
      } 
      pos++;
    }
    buffer[pos] = ' ';
    lastword = pos++;
  }
  buffer[pos] = '\0';

  puts(buffer);

  return(0);
}
