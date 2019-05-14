/* Henter vejrdata i JSON-format fra Yahoos API. Fortolker denne. Se format og eksempel på JSON nedlastning nederst i denne kode. */

package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"strings"
	"text/template"
	"vejrLib"
)

// prut
const (
	APIKEY                 = "4b42ecc61fd13e0a0cb7006d583fb3e0"
	kantinevejrBeskrivelse = "Kantinen er inden døre, og der er derfor nok tørvejr og stuetemperatur. Men det ved du nok bedre end jeg!"
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

func main() {
	city := "København"
	country := "DK"
	velkomst := false;
	if len(os.Args) > 1 {
		if os.Args[1] == "Velkomstbesked"{
			velkomst = true
		} else {
			// Hvad er vejret i Kantinen?
			if os.Args[1] == "Kantinen" {
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
	}

	resp, err := http.Get(fmt.Sprintf("http://api.openweathermap.org/data/2.5/weather?q=%s,%s&lang=da&units=metric&APPID=%s", url.QueryEscape(city), url.QueryEscape(country), APIKEY))
	if err != nil {
		// prøv lige uden land
		resp, err = http.Get(fmt.Sprintf("http://api.openweathermap.org/data/2.5/weather?q=%s&lang=da&units=metric&APPID=%s", url.QueryEscape(city), APIKEY))
		if err != nil {
			fmt.Println("Den by findes ikke eller også er der noget andet, der er gået galt!")
			return
		}
	}
	defer resp.Body.Close()
	body, _ := ioutil.ReadAll(resp.Body)

	var dat JsonAPI
	err = json.Unmarshal(body, &dat)
	if err != nil {
		fmt.Println("Den by findes ikke eller også er der noget andet, der er gået galt!")
		return
	}

	/* Hent relevant vinddata fra JSON-struktur */
	degrees := dat.Main.Temp
	windSpeed := dat.Wind.Speed
	windBeaufortName := vejrLib.Beaufort(dat.Wind.Speed)
	windDirection := dat.Wind.Deg

	/* Hent vejrbeskrivelse, fortolk og fordansk denne */
	description := dat.Weather[0].Id
	beskrivelse := vejrLib.Vejrbeskrivelse(description)

	/* Hent coordinater for målestation og udregn afstand til Kantinen */
	lon := dat.Coord.Lon
	lat := dat.Coord.Lat
	afstandStr := vejrLib.AfstandStr(lon, lat)

	/* Tid for opdatering */
	timeForUpdate := dat.Dt
	ageStr := vejrLib.ErMaalingenGammel(timeForUpdate)

	windDirectionstr := vejrLib.WindDirectionString(windDirection)

	realCountry := vejrLib.CountryFromCode(dat.Sys.Country)

	var t *template.Template
	if (velkomst){
		if (description == 800 || description == 801) {
			t, _ = template.New("Vejr").Parse("Sikke et dejligt vejr vi har her i Kongens København! Det er jo {{.Beskrivelse}} og {{.Degrees}}°C, og der blæser en {{.WindBeaufortName}} fra {{.WindDirection}}.")
		} else {
			t, _ = template.New("Vejr").Parse(`Vidste du at vejret her i København er {{.Beskrivelse}} med en temperatur på {{.Degrees}}°C? Der blæser en {{.WindBeaufortName}} fra {{.WindDirection}}.`)
		}
	} else {
		t, _ = template.New("vejr").Parse(`Vejret i {{.City}}, {{.Country}}: {{.Beskrivelse}} med en temperatur på {{.Degrees}}°C. {{.WindBeaufortName}}, {{.WindSpeed}} m/s, fra {{.WindDirection}}. {{.Afstand}} {{.Age}}`)
	}
	out := bytes.NewBufferString("")
	t.Execute(out, struct {
		City             string
		Country          string
		Beskrivelse      string
		Degrees          string
		WindBeaufortName string
		WindSpeed        string
		WindDirection    string
		Afstand          string
		//Position          string
		Age string
	}{
		city,
		realCountry,
		beskrivelse,
		fmt.Sprintf("%.1f", degrees),
		fmt.Sprint(windBeaufortName),
		fmt.Sprintf("%.1f", windSpeed),
		windDirectionstr,
		afstandStr,
		//fmt.Sprintf("længdegrad: %.3f°, breddegrad: %.3f°.", lon, lat),
		ageStr,
	})

	fmt.Println(out.String())
}
