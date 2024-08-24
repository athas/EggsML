/* Henter vejrdata i JSON-format fra Yahoos API. Fortolker denne. Se format og eksempel på JSON nedlastning nederst i denne kode. */

package main

import (
	"bytes"
	"encoding/csv"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"text/template"
	"unicode"
	"unicode/utf8"
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

func readLonLat(filename string, city string, country string) (lon float64, lat float64, ok bool, err error) {
	b, err := ioutil.ReadFile(filename)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return 0.0, 0.0, false, nil
		}
		return 0.0, 0.0, false, err
	}

	toFind := fmt.Sprintf("%s,%s", city, country)

	r := csv.NewReader(bytes.NewReader(b))
	r.Comma = '\t'

	for {
		record, err := r.Read()
		if err == io.EOF {
			break
		}
		if err != nil {
			return 0.0, 0.0, false, err
		}

		if record[0] == toFind {
			lon, err = strconv.ParseFloat(record[1], 64)
			if err != nil {
				return 0.0, 0.0, false, err
			}
			lat, err = strconv.ParseFloat(record[2], 64)
			if err != nil {
				return 0.0, 0.0, false, err
			}
			return lon, lat, true, nil
		}
	}

	return 0.0, 0.0, false, nil
}

func appendLonLat(filename string, city string, country string, lon float64, lat float64) error {
	f, err := os.OpenFile(filename, os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0600)
	if err != nil {
		return err
	}
	defer f.Close()

	if _, err := f.WriteString(fmt.Sprintf("%s,%s\t%.2f\t%.2f\n", city, country, lon, lat)); err != nil {
		return err
	}

	return nil
}

// nu med bivirkninger!
func makeCall(url string, response interface{}) error {
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	body, _ := ioutil.ReadAll(resp.Body)

	if err = json.Unmarshal(body, response); err != nil {
		return err
	}

	return nil
}

func readUserSetting(user string) (city string, country string, found bool) {
	cmd := exec.Command("dbUserRead", user, "vejrsted")
	o, err := cmd.Output()
	if err != nil {
		return city, country, false
	}
	s := strings.Split(string(o), ",")
	if len(s) == 0 {
		return city, country, false
	}
	if len(s) == 1 {
		return s[0], "DK", true
	}
	return s[0], s[1], true
}

func writeUserSetting(user string, setting string) (err error) {
	cmd := exec.Command("dbUserWrite", user, "vejrsted")
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return err
	}

	go func() {
		defer stdin.Close()
		io.WriteString(stdin, setting)
	}()

	return cmd.Run()
}

func capitalise(s string) string {
	r, size := utf8.DecodeRuneInString(s)
	if r == utf8.RuneError {
		// øh, bare gør det som det passer dem!
		return s
	}
	return string(unicode.ToUpper(r)) + s[size:]
}

func main() {
	user := os.Getenv("EGGS_USER")
	var city, country string
	var found bool
	city, country, found = readUserSetting(user)
	if !found || city == "" || country == "" {
		city = "København"
		country = "DK"
	}
	velkomst := false
	args := os.Args
	if len(args) > 1 {
		country = "DK" // antag Danmark
		if args[1] == "Velkomstbesked" {
			velkomst = true
		} else {
			if args[1] == "gem" {
				args = append(args[:0], args[2:]...)
				if err := writeUserSetting(user, strings.Join(args, " ")); err != nil {
					fmt.Println("Den kunne jeg ikke lige gemme, men jeg kan vise dig vejret!")
				}
			} else if args[1] == "Kantinen" {
				// Hvad er vejret i Kantinen?
				fmt.Println(kantinevejrBeskrivelse)
				return
			} else {
				args = append(args[:0], args[1:]...)
			}
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

	dbDir := os.Getenv("CONCIEGGS_DB_DIR")
	var storeFile string
	var lon, lat float64
	foundLoc := false
	if len(dbDir) > 0 {
		storeFile = fmt.Sprintf("%s/store/vejrsteder", dbDir)

		var ok bool
		lon, lat, ok, _ = readLonLat(storeFile, city, country)
		if ok {
			foundLoc = true
		}
	}

	if !foundLoc {
		var locations []struct {
			Name       string
			LocalNames struct {
				Da string
			} `json:"local_names"`
			Lat     float64
			Lon     float64
			Country string
			State   string
		}

		// lad os finde hvor vi er
		if len(country) > 0 {
			if err := makeCall(fmt.Sprintf("http://api.openweathermap.org/geo/1.0/direct?q=%s,%s&limit=1&appid=%s", url.QueryEscape(city), url.QueryEscape(country), APIKEY), &locations); err != nil {
				fmt.Println("Den by findes ikke eller også er der noget andet, der er gået galt!")
				return
			}
		}
		if len(locations) == 0 {
			// prøv lige uden land
			if err := makeCall(fmt.Sprintf("http://api.openweathermap.org/geo/1.0/direct?q=%s&limit=1&appid=%s", url.QueryEscape(city), APIKEY), &locations); err != nil {
				fmt.Println("Den by findes ikke eller også er der noget andet, der er gået galt!")
				return
			}
		}
		if len(locations) == 0 {
			// øv bøv
			fmt.Println("Den by findes ikke eller også er der noget andet, der er gået galt!")
			return
		}

		// vælg det første sted
		loc := locations[0]
		country = loc.Country

		lon, lat = loc.Lon, loc.Lat
	}

	var dat JsonAPI
	if err := makeCall(fmt.Sprintf("http://api.openweathermap.org/data/2.5/weather?lat=%.2f&lon=%.2f&lang=da&units=metric&APPID=%s", lat, lon, APIKEY), &dat); err != nil {
		fmt.Println("Den by findes ikke eller også er der noget andet, der er gået galt!")
		return
	}

	if !foundLoc && len(storeFile) > 0 {
		if err := appendLonLat(storeFile, city, country, lon, lat); err != nil {
			// ligemeget hvis den ikke blev gemt
		}
	}

	/* Hent relevant vinddata fra JSON-struktur */
	degrees := dat.Main.Temp
	windSpeed := dat.Wind.Speed
	windBeaufortName := capitalise(vejrLib.Beaufort(dat.Wind.Speed))
	windDirection := dat.Wind.Deg
	humidity := dat.Main.Humidity

	/* Hent vejrbeskrivelse, fortolk og fordansk denne */
	description := dat.Weather[0].Id
	beskrivelse := vejrLib.Vejrbeskrivelse(description)

	/* Hent coordinater for målestation og udregn afstand til Kantinen */
	lon = dat.Coord.Lon
	lat = dat.Coord.Lat
	afstandStr := vejrLib.AfstandStr(lon, lat)

	/* Tid for opdatering */
	timeForUpdate := dat.Dt
	ageStr := vejrLib.ErMaalingenGammel(timeForUpdate)

	windDirectionstr := vejrLib.WindDirectionString(windDirection)

	realCountry := vejrLib.CountryFromCode(dat.Sys.Country)

	var t *template.Template
	if velkomst {
		if humidity >= 75 && degrees > 25 {
			t, _ = template.New("Vejr").Parse("Jeg er ved at dø af varme!")
		} else if description == 800 || description == 801 { // klar himmel
			t, _ = template.New("Vejr").Parse("Synes du ikke også, vejret er dejligt?")
		} else if description <= 622 && description >= 200 { // nedbør
			t, _ = template.New("Vejr").Parse("Synes du ikke også, vejret er træls?")
		} else if description == 804 { // overskyet
			t, _ = template.New("Vejr").Parse("Det er lidt trist vejr, vi har, ikke?")
		} else {
			t, _ = template.New("Vejr").Parse(`Vidste du at vejret her i København er {{.Beskrivelse}} med en temperatur på {{.Degrees}}°C og en luftfugtighed på {{.Humidity}}%? Der blæser en {{.WindBeaufortName}} fra {{.WindDirection}}.`)
		}
	} else {
		t, _ = template.New("vejr").Parse(`Vejret i {{.City}}, {{.Country}}: {{.Beskrivelse}} med en temperatur på {{.Degrees}}°C og luftfugtighed på {{.Humidity}}%. {{.WindBeaufortName}}, {{.WindSpeed}} m/s, fra {{.WindDirection}}. {{.Afstand}} {{.Age}}`)
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
		Humidity         int
		Age              string
	}{
		city,
		realCountry,
		beskrivelse,
		fmt.Sprintf("%.1f", degrees),
		fmt.Sprint(windBeaufortName),
		fmt.Sprintf("%.1f", windSpeed),
		windDirectionstr,
		afstandStr,
		humidity,
		ageStr,
	})

	fmt.Println(out.String())
}
