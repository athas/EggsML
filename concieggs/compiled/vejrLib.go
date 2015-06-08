package vejrLib

import (
	"math"
	"time"
	"strconv"
)

const (
	fejlVedVejrBeskrivelse = "Jeg kender ikke denne vejrbeskrivelse!"
	DIKULON    = 12.561210 //længdegrad for DIKUs Kantine
	DIKULAT    = 55.702082 //breddegrad for DIKUs Kantine
	MEAN_EARTH_RADIUS = 6371 //enhed er i kilometer!
	OPLOESNING = 22.5 //Bruges til vindretningsangivelse
)


/* Tjekker om en grad (vinkel) passer til et bestemt verdenshjørne */
func projicerVindretning(koordinat float64, hovedretning, oploesning float64) bool {
	return math.Abs(koordinat - hovedretning) <= oploesning/2
}


/* Fortæller, om målingen er mere end 60 minutter gammel */
func ErMaalingenGammel (timeForUpdate int) string {
	age := (int(time.Now().UTC().Unix()) - int(timeForUpdate))/60
	if (age > 60) {
		return "Målingen er " + strconv.Itoa(age) + " minutter gammel. "
}
	return ""
}


/* Fortolker vejrkode og returnerer en vejrbeskrivelse */
func Vejrbeskrivelse(kode int) string {
	// Vejrbeskrivelser er hentet fra openweathermap.org/weather-conditions
vejrBeskrivelse := map[int]string{
    200: "tordenvejr med let regn",
    201: "tordenvejr med regn",
    202: "tordenvejr med kraftig regn",
    210: "let tordenvejr",
    211: "tordenvejr",
    212: "kraftigt tordenvejr",
    221: "omskifteligt tordenvejr",
    230: "tordenvejr med let støvregn",
    231: "tordenvejr med støvregn",
    232: "tordenvejr med kraftig støvregn",
    300: "let støvregn",
    301: "støvregn",
    302: "kraftig støvregn",
    310: "let støvregn/regn",
    311: "støvregn/regn",
    312: "kraftig støvregn/regn",
    313: "regnbyger og støvregn",
    314: "kraftige regnbyger og støvregn",
    321: "byger med støvregn",
    500: "let regn",
    501: "regn",
    502: "kraftig regn",
    503: "skybrud",
    504: "kraftigt skybrud",
    511: "isslag",
    520: "lette regnbyger",
    521: "regnbygevejr",
    522: "kraftige regnbyger",
    531: "omskifteligt med byger og regn",
    600: "let sne",
    601: "sne",
    602: "kraftig sne",
    611: "slud",
    612: "sludbyger",
    615: "let regn og sne",
    616: "regn og sne",
    620: "lette snebyger",
    621: "snebyger",
    622: "kraftige snebyger",
    701: "dis",
    711: "røg",
    721: "dis/tåge",
    731: "sand- og støvvind",
    741: "tåge",
    751: "sand i luften",
    761: "støv i luften",
    762: "vulkansk aske",
    771: "kastevinde",
    781: "tornadoer",
    800: "klar himmel",
    801: "næsten klar himmel",
    802: "spredte skyer",
    803: "næsten overskyet",
    804: "overskyet",
    900: "tornadoer",
    901: "tropisk storm",
    902: "orkan",
    903: "ekstrem kulde",
    904: "ekstrem varme",
    905: "ekstreme vindforhold",
    906: "haglvejr",
    951: "stille eller næsten stille", //herfra og ned er Beaufort-skala. Ændr dem ikke!
    952: "svag vind",
    953: "let vind",
    954: "jævn vind",
    955: "frisk vind",
    956: "hård vind",
    957: "stiv kuling",
    958: "hård kuling",
    959: "stormende kuling",
    960: "storm",
    961: "stærk storm",
    962: "orkan",
}	

beskrivelse := vejrBeskrivelse[kode]
	if (beskrivelse == "") {
		return fejlVedVejrBeskrivelse	
	}	
return beskrivelse
}



/* Returner Beaufort vindskala beskrivelse (f.eks. hård vind/stiv kuling) fra funktionsargumentet, som er vindhastighed i m/s */
func Beaufort(windSpeed float64) string {

	/* Disse to definitioner er definitionen på Beauforts skala. Enhed for vind: m/s. Arrays kan ikke placeres i const-felt. */
	beaufortHastighed := [12]float64{
		.3,
		.6,
		3.4,
		5.5,
		8.0,
		10.8,
		13.9,
		17.2,
		20.8,
		24.5,
		28.5,
		32.7 }
	beaufortBeskrivelse := [13]string{
		"Stille", //Er vindhastigheden under .3 m/s, betegnes vindfholdet som "stille".
		"Næsten stille",
		"Svag vind",
		"Let vind",
		"Jævn vind",
		"Frisk vind",
		"Hård vind",
		"Stiv kuling",
		"Hård kuling",
		"Stormende kuling",
		"Storm",
		"Stærk storm",
		"Orkan" }

	for i := range beaufortHastighed {
		if (beaufortHastighed[i] > windSpeed) {
			return beaufortBeskrivelse[i]
		}
	}
	return beaufortBeskrivelse[cap(beaufortBeskrivelse)-1] //orkan!!
}


func fraGradTilRad(grad float64) float64 {
	return math.Pi/180*grad //pi radianer per 180	
}


/* Udregner afstanden fra målestationen til DIKUs Kantine */
func Afstand(longitude, latitude float64) int {
	lonRad := fraGradTilRad(longitude)
	latRad := fraGradTilRad(latitude)
	dikuLonRad := fraGradTilRad(DIKULON)
	dikuLatRad := fraGradTilRad(DIKULAT)
	storCirkelVinkel := math.Acos(math.Sin(dikuLatRad)*math.Sin(latRad)+math.Cos(dikuLatRad)*math.Cos(latRad)*math.Cos(math.Abs(lonRad-dikuLonRad))) //se evt. wiki/Great-circle_distance
	return int(MEAN_EARTH_RADIUS*storCirkelVinkel)
}


/* Hvorfra blæser det? */
func WindDirectionString(windDirection float64) string {
	verdenshjoerner := [17]string{"nord","nordnordøst","nordøst","østnordøst","øst","østsydøst",
		"sydøst","sydsydøst","syd","sydsydvest","sydvest","vestsydvest","vest",
		"vestnordvest","nordvest","nordnordvest","nord"}
	if (len(verdenshjoerner) != 360 / OPLOESNING + 1) {
		return "Der er sket en fejl og nogen bør skamme sig!"
	}
	for i := 0; i < len(verdenshjoerner); i++ {
		if (projicerVindretning(windDirection, float64(i)*OPLOESNING, OPLOESNING)) {
			return verdenshjoerner[i]
		}
	}
	return "noget er gået galt"
}
