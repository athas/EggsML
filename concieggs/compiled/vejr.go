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
)

const (
	OPLOESNING = 45
	APIKEY     = "4b42ecc61fd13e0a0cb7006d583fb3e0"
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

func main() {
	city := "København"
	country := "Danmark"
	if len(os.Args) > 1 {
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
	windDirection := dat.Wind.Deg
	//condition := dat.Weather[0].Main
	description := dat.Weather[0].Description

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

	t, _ := template.New("vejr").Parse(`Vejret i {{.City}}, {{.Country}}: {{.Description}}, med en temperatur på {{.Degrees}}° og en blæst med {{.WindSpeed}} m/s fra {{.WindDirection}}.`)
	out := bytes.NewBufferString("")
	t.Execute(out, struct {
		City          string
		Country       string
		Description   string
		Degrees       string
		WindSpeed     string
		WindDirection string
	}{
		city,
		country,
		description,
		fmt.Sprintf("%.1f", degrees),
		fmt.Sprintf("%.1f", windSpeed),
		windDirectionstr,
	})

	fmt.Println(out.String())
}

