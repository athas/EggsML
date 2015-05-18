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
	OPLOESNING = 45
	APIKEY     = "4b42ecc61fd13e0a0cb7006d583fb3e0"
	DIKULON    = 12.561210 //længdegrad for DIKUs Kantine
	DIKULAT    = 55.702082 //breddegrad for DIKUs Kantine
	MEAN_EARTH_RADIUS = 6371 //enhed er i kilometer!
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

func projicerVindretning(koordinat float64, hovedretning, oploesning int) bool {
	return math.Abs(koordinat-float64(hovedretning)) <= float64(oploesning)
}

/* Returner beaufort vindskala beskrivelse (f.eks. hård vind/stiv kuling) fra funktionsargumentet, som er vindhastighed i m/s */
func beaufort(windSpeed float64) string {

	/* Disse kunne ikke placeres i const.-feltet, da der ikke tillades arrays der */
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
		"Stille",
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


/* Udregner afstanden fra målestationen til DIKUs Kantine */
func afstand(longitude, latitude float64) int {
	lonRad := math.Pi/180*longitude
	latRad := math.Pi/180*latitude
	dikuLonRad := math.Pi/180*DIKULON
	dikuLatRad := math.Pi/180*DIKULAT
	storCirkelVinkel := math.Acos(math.Sin(dikuLatRad)*math.Sin(latRad)+math.Cos(dikuLatRad)*math.Cos(latRad)*math.Cos(math.Abs(lonRad-dikuLonRad))) //se evt. wiki/Great-circle_distance
	return int(MEAN_EARTH_RADIUS*storCirkelVinkel)
}


func main() {
	city := "København"
	country := "Danmark"
	if len(os.Args) > 1 {
		/* Hvad er vejret i Kantinen? */
		if (os.Args[1] == "Kantinen") {
			fmt.Println("Kantinen er inden døre, og der er derfor nok tørvejr og stuetemperatur. Men det ved du nok bedre end jeg!")
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
	//condition := dat.Weather[0].Main
	description := dat.Weather[0].Description


	/* Hent coordinater for målestation og udregn afstand til Kantinen */
	lon := dat.Coord.Lon
	lat := dat.Coord.Lat
	afstand := afstand(lon, lat)


	/* Tid for opdatering */
	timeForUpdate := dat.Dt
	age := (int(time.Now().UTC().Unix()) - int(timeForUpdate))/60 //dividere med 60 angiver minutter


	/* Hvorfra blæser det? */
	var windDirectionstr string
	switch {
	case (projicerVindretning(windDirection, 0, OPLOESNING)):
		windDirectionstr = "nord"
	case (projicerVindretning(windDirection, 360, OPLOESNING)):
		windDirectionstr = "nord"
	case (projicerVindretning(windDirection, 45, OPLOESNING)):
		windDirectionstr = "nordøst"
	case (projicerVindretning(windDirection, 90, OPLOESNING)):
		windDirectionstr = "øst"
	case (projicerVindretning(windDirection, 135, OPLOESNING)):
		windDirectionstr = "sydøst"
	case (projicerVindretning(windDirection, 180, OPLOESNING)):
		windDirectionstr = "syd"
	case (projicerVindretning(windDirection, 225, OPLOESNING)):
		windDirectionstr = "sydvest"
	case (projicerVindretning(windDirection, 270, OPLOESNING)):
		windDirectionstr = "vest"
	case (projicerVindretning(windDirection, 315, OPLOESNING)):
		windDirectionstr = "nordvest"
	}

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

