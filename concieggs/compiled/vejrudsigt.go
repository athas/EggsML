package main

import(
    "fmt"
    "net/http"
    "io/ioutil"
    "net/url"
    "encoding/json"
    //"math"
    "os"
    "strings"
    //"text/template"
    //"bytes"
)

const (
    APIKEY     = "4b42ecc61fd13e0a0cb7006d583fb3e0"
    OPLOESNING = 22.5
    fejlVedVejrBeskrivelse    = "Jeg kender ikke denne vejrbeskrivelse."
    APIURL = "http://api.openweathermap.org/data/2.5/forecast?q=%s,%s&lang=da&mode=json&units=metric&APPID=%s"
)

/* Følgende kommentar viser strukturen af den hentede data */
/*
type JsonAPI struct {
    Cnt int
    List []struct {
        Dt_txt string
        Dt float64
        Main struct {
            Temp float64
            TempKf float64
            TempMin float64
            TempMax float64
            Pressure float64
            SeaLevel float64
            GroundLevel float64
            Humidity int
        }
        Weather []struct {
            Id int
            Main string
            Description string
            Icon string
        }
        Clouds struct {
            All int
        }
        Wind struct {
            Speed float64
            Deg float64
        }
        // Rain is not always present!
        Rain struct {
            threeH float64
        }
        Sys struct {
            Pod string
        }
    }
}
*/

func getAdvance(arg2 string) {
    advance := -1
    if (tidspunkt == "i dag") {
        advance = 0
    }
    if (tidspunkt == "i morgen") {
        advance = 1
    }
    if (tidspunkt == "i overmorgen") {
        advance = 2
    }
    return advance
}


func main() {
    city := "København"
    country := "Danmark"

    if (len(os.Args) < 3) {
        fmt.Println("Brug: vejrudsigt <i dag|i morgen|i overmorgen> [, by[, land]]")
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

    advance := getAdvance(ss[0])
    if (advance == -1) {
        fmt.Println("Gyldige tidspunkter er \"i morgen\" eller \"i overmorgen\"")
        return
    }

    resp, err := http.Get(fmt.Sprintf(APIURL, url.QueryEscape(city), url.QueryEscape(country), APIKEY))
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

    fmt.Println(dat)


    /* Hent relevant vinddata fra JSON-struktur */
    // city = (dat["city"].(map[string]interface{}))["name"].(string)
    // country =  (dat["city"].(map[string]interface{}))["country"].(string)
    /listn := dat["List"].([]interface{})
    /* How do you weather data for the current day?? Some date library must be used. */
    /* The following call should only take weather forecasts for today if "i dag" is 0th argument */
    list := listn[advance].(map[string]interface{}) //entry no. 0-2 in

    // temp := list["temp"].(map[string]interface{})
    // degrees := temp["day"].(float64)

    // weathern := list["weather"].([]interface{})
    // weather := weathern[0].(map[string]interface{})
    // kode := weather["id"].(float64) // SHOULD BE INT!! But only works for float!
    // beskrivelse := vejrbeskrivelse(int(kode))

    // windSpeed := list["speed"].(float64)
    // windBeaufortName := beaufort(windSpeed)
    // windDirection := list["deg"].(float64)
    // windDirectionStr := windDirectionString(windDirection)


    // t, _ := template.New("vejr").Parse(`Vejret i {{.City}}, {{.Country}}, {{.Tidspunkt}}: {{.Beskrivelse}} med en forventet temperatur på {{.Degrees}}°C. {{.WindBeaufortName}}, {{.WindSpeed}} m/s, fra {{.WindDirection}}.`)
    // out := bytes.NewBufferString("")
    // t.Execute(out, struct {
    //     City              string
    //     Country           string
    //     Tidspunkt         string
    //     Beskrivelse       string
    //     Degrees           int
    //     WindBeaufortName  string
    //     WindSpeed         string
    //     WindDirection     string
    // }{
    //     city,
    //     country,
    //     tidspunkt, //sat af funktionskald
    //     beskrivelse,
    //     int(degrees),
    //     fmt.Sprint(windBeaufortName),
    //     fmt.Sprintf("%.1f", windSpeed),
    //     windDirectionStr,
    // })

    // fmt.Println(out.String())



}
