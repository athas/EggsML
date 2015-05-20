/* Henter vejrdata i JSON-format fra Yahoos API. Fortolker denne. Se format og eksempel på JSON nedlastning nederst i denne kode. */

package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math"
	"net/http"
	"net/url"
	"os"
	"strings"
	"text/template"
	"time"
)


const (
	OPLOESNING = 45 //Bruges til vindretningsangivelse
	APIKEY     = "4b42ecc61fd13e0a0cb7006d583fb3e0"
	DIKULON    = 12.561210 //længdegrad for DIKUs Kantine
	DIKULAT    = 55.702082 //breddegrad for DIKUs Kantine
	MEAN_EARTH_RADIUS = 6371 //enhed er i kilometer!
	kantinevejrBeskrivelse = "Kantinen er inden døre, og der er derfor nok tørvejr og stuetemperatur. Men det ved du nok bedre end jeg!"
	fejlVedVejrBeskrivelse = "Jeg kender ikke denne vejrbeskrivelse!"
)


type JsonAPI struct {
	Coord struct {
		Lon float64
		Lat float64
	}
	Sys struct {
		Message float64
		Country string
		Sunrise int
		Sunset  int
	}
	Weather []struct {
		Id          int
		Main        string
		Description string
		Icon        string
	}
	Base string
	Main struct {
		Temp       float64
		Temp_min   float64
		Temp_max   float64
		Pressure   float64
		Sea_level  float64
		Grnd_level float64
		Humidity   int
	}
	Wind struct {
		Speed float64
		Deg   float64
	}
	Clouds struct {
		All int
	}
	Dt   int
	Id   int
	Name string
	Cod  int
}

/* Tjekker om en grad (vinkel) passer til et bestemt verdenshjørne */
func projicerVindretning(koordinat int, hovedretning, oploesning int) bool {
	return math.Abs(float64(koordinat)-float64(hovedretning)) <= float64(oploesning)
}

/* Fortolker vejrkode og returnerer en vejrbeskrivelse */
func vejrbeskrivelse(kode int) string {
	// Vejrbeskrivelser er hentet fra openweathermap.org/weather-conditions
vejrBeskrivelse := map[string]int{
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
func beaufort(windSpeed float64) string {

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
func afstand(longitude, latitude float64) int {
	lonRad := fraGradTilRad(longitude)
	latRad := fraGradTilRad(latitude)
	dikuLonRad := fraGradTilRad(DIKULON)
	dikuLatRad := fraGradTilRad(DIKULAT)
	storCirkelVinkel := math.Acos(math.Sin(dikuLatRad)*math.Sin(latRad)+math.Cos(dikuLatRad)*math.Cos(latRad)*math.Cos(math.Abs(lonRad-dikuLonRad))) //se evt. wiki/Great-circle_distance
	return int(MEAN_EARTH_RADIUS*storCirkelVinkel)
}


	/* Hvorfra blæser det? */
func windDirectionString(windDirection int) string {
	switch {
	case (projicerVindretning(windDirection, 0, OPLOESNING)):
		return "nord"
	case (projicerVindretning(windDirection, 360, OPLOESNING)):
		return  "nord"
	case (projicerVindretning(windDirection, 45, OPLOESNING)):
		return  "nordøst"
	case (projicerVindretning(windDirection, 90, OPLOESNING)):
		return  "øst"
	case (projicerVindretning(windDirection, 135, OPLOESNING)):
		return  "sydøst"
	case (projicerVindretning(windDirection, 180, OPLOESNING)):
		return  "syd"
	case (projicerVindretning(windDirection, 225, OPLOESNING)):
		return  "sydvest"
	case (projicerVindretning(windDirection, 270, OPLOESNING)):
		return  "vest"
	case (projicerVindretning(windDirection, 315, OPLOESNING)):
		return  "nordvest"
	}
	return "noget er gået galt"
}


func main() {
	city := "København"
	country := "Danmark"
	if len(os.Args) > 1 {
		/* Hvad er vejret i Kantinen? */
		if (os.Args[1] == "Kantinen") {
			fmt.Println(kantinevejrBeskrivelse)
			return
		}

		args := append(os.Args[:0], os.Args[1:]...)
		argsStr := strings.Join(args, " ")
		ss := strings.Split(argsStr, ",")
		if len(ss) == 1 {
			city = ss[0]
		}
		if len(ss) >= 2 {
			city = strings.Trim(ss[0], " ")
			country = strings.Trim(ss[1], " ")
		}
	}

	resp, err := http.Get(fmt.Sprintf("http://api.openweathermap.org/data/2.5/weather?q=%s,%s&lang=da&units=metric&APPID=%s", url.QueryEscape(city), url.QueryEscape(country), APIKEY))
	if err != nil {
		fmt.Println("Den by findes vist ikke.")
		return
	}
	defer resp.Body.Close()
	body, _ := ioutil.ReadAll(resp.Body)

	var dat JsonAPI
	err = json.Unmarshal(body, &dat)
	if err != nil {
		fmt.Println("Den by findes vist ikke.")
		return
	}


	/* Hent relevant vinddata fra JSON-struktur */
	degrees := dat.Main.Temp
	windSpeed := dat.Wind.Speed
	windBeaufortName := beaufort(dat.Wind.Speed)
	windDirection := dat.Wind.Deg
	description := dat.Weather[0].Description

	/* Hent coordinater for målestation og udregn afstand til Kantinen */
	lon := dat.Coord.Lon
	lat := dat.Coord.Lat
	afstand := afstand(lon, lat)

	/* Tid for opdatering */
	timeForUpdate := dat.Dt
	age := (int(time.Now().UTC().Unix()) - int(timeForUpdate))/60 //dividering med 60 angiver minutter
	
	

	windDirectionstr := windDirectionString(int(windDirection))

	t, _ := template.New("vejr").Parse(`Vejret i {{.City}}, {{.Country}}: {{.Description}}, med en temperatur på {{.Degrees}}°. {{.WindBeaufortName}}, {{.WindSpeed}} m/s, fra {{.WindDirection}}. Målestationens afstand til Kantinen er ca. {{.Afstand}} km. Målingen er (vist nok) {{.Age}} minutter gammel.`)
	out := bytes.NewBufferString("")
	t.Execute(out, struct {
		City              string
		Country           string
		Description       string
		Degrees           string
		WindBeaufortName  string
		WindSpeed         string
		WindDirection     string
		Afstand           int
		Age               int
	}{
		city,
		country,
		description,
		fmt.Sprintf("%.1f", degrees),
		fmt.Sprint(windBeaufortName),
		fmt.Sprintf("%.1f", windSpeed),
		windDirectionstr,
		afstand,
		age,
	})

	fmt.Println(out.String())
}

