package main

import(
	"fmt"
	"net/http"
	"io/ioutil"
	"net/url"
	"encoding/json"
	"math"
	"os"
	"strings"
	"text/template"
	"bytes"
)

const (
	APIKEY     = "4b42ecc61fd13e0a0cb7006d583fb3e0"
	OPLOESNING = 22.5
	fejlVedVejrBeskrivelse	= "Jeg kender ikke denne vejrbeskrivelse."
)

/*
type JsonAPI struct {
	Cod string // should be int or string.
	Message float64
	City struct {
		Id int
		Name string
		Coord struct {
			Lon float64
			Lat float64
		}
		Country string
		Population int
		Sys struct{
			Population int
		}
	}
	Cnt int
	List []struct {
		Dt int64
		Temp struct {
			Day float64
			Min float64
			Max float64
			Night float64
			Eve float64
			Morn float64
		}
		Pressure float64
		Humidity int
		Weather []struct {
			Id int
			Main string
			Description string
			Icon string
		}
		Speed float64
		Deg   float64
		Clouds int
		//Rain   float64 /* ER IKKE ALTID MED!! */


/* Fortolker vejrkode og returnerer en vejrbeskrivelse */
func vejrbeskrivelse(kode int) string {
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



/* Hvorfra blæser det? */
func windDirectionString(windDirection float64) string {
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


/* Tjekker om en grad (vinkel) passer til et bestemt verdenshjørne */
func projicerVindretning(koordinat float64, hovedretning, oploesning float64) bool {
	return math.Abs(koordinat - hovedretning) <= oploesning/2
}



func main() {
	city := "Copenhagen"
	country := "Denmark"

	if (len(os.Args) < 3) {
		fmt.Println("Brug: vejrudsigt <i morgen|i overmorgen|om tre-fem dage> [, by[, land]]")
		return
	}

	args := append(os.Args[:0], os.Args[1:]...) //what does this do??
	argsStr := strings.Join(args, " ")
	ss := strings.Split(argsStr, ",")
	tidspunkt := ""
	if len(ss) == 1 {
		tidspunkt = ss[0]
	}
	if len(ss) == 2 {
		tidspunkt = strings.Trim(ss[0], " ")
		city = strings.Trim(ss[1], " ")
	}
	if len(ss) == 3 {
		tidspunkt = strings.Trim(ss[0], " ")
		city = strings.Trim(ss[1], " ")
		country = strings.Trim(ss[2], " ")
	}


	advance := -1
	if (tidspunkt == "i morgen") {
		advance = 1
	}
	if (tidspunkt == "i overmorgen") {
		advance = 2
	}
	if (advance == -1) {
		fmt.Println("Gyldige tidspunkter er \"i morgen\" eller \"i overmorgen\"")
		return
	}

	resp, err := http.Get(fmt.Sprintf("http://api.openweathermap.org/data/2.5/forecast/daily?q=%s,%s&lang=da&mode=json&units=metric&cnt=5&APPID=%s", url.QueryEscape(city), url.QueryEscape(country), APIKEY))

	if err != nil {
		fmt.Println("1 Den by findes ikke eller også er der noget andet, der er gået galt!")
		return
	}

	defer resp.Body.Close()
	body, _ := ioutil.ReadAll(resp.Body)

	/* En dynamisk struktur er umiddelbart nødvendig, da API'en ikke altid returnerer den samme struktur (rain feltet indgår ikke altid) */
	type Datif map[string]interface{}
	var dat Datif
	json.Unmarshal(body,&dat)

	if err != nil {
		fmt.Println("2 Den by findes ikke eller også er der noget andet, der er gået galt!")
		//return
	}


	/* Hent relevant vinddata fra JSON-struktur */
	city = (dat["city"].(map[string]interface{}))["name"].(string)
	country =  (dat["city"].(map[string]interface{}))["country"].(string)
	listn := dat["list"].([]interface{})
	list := listn[advance].(map[string]interface{})

	temp := list["temp"].(map[string]interface{})
	degrees := temp["day"].(float64)

	weathern := list["weather"].([]interface{})
	weather := weathern[0].(map[string]interface{})
	kode := weather["id"].(float64) // SHOULD BE INT!! But only works for float!
	beskrivelse := vejrbeskrivelse(int(kode))

	windSpeed := list["speed"].(float64)
	windBeaufortName := beaufort(windSpeed)
	windDirection := list["deg"].(float64)
	windDirectionStr := windDirectionString(windDirection)


	t, _ := template.New("vejr").Parse(`Vejret i {{.City}}, {{.Country}}, {{.Tidspunkt}}: {{.Beskrivelse}} med en forventet temperatur på {{.Degrees}}°C. {{.WindBeaufortName}}, {{.WindSpeed}} m/s, fra {{.WindDirection}}.`)
	out := bytes.NewBufferString("")
	t.Execute(out, struct {
		City              string
		Country           string
		Tidspunkt         string
		Beskrivelse       string
		Degrees           int
		WindBeaufortName  string
		WindSpeed         string
		WindDirection     string
	}{
		city,
		country,
		tidspunkt, //sat af funktionskald
		beskrivelse,
		int(degrees),
		fmt.Sprint(windBeaufortName),
		fmt.Sprintf("%.1f", windSpeed),
		windDirectionStr,
	})

	fmt.Println(out.String())



}
