/* Henter vejrdata i JSON-format fra Yahoos API. Fortolker denne. Se format og eksempel på JSON nedlastning nederst i denne kode. */

package main

import (
	"fmt"
	"encoding/json"
	"net/http"
	"io/ioutil"
	"strconv"
	"math"
)

const (
	OPLOESNING = 45
)



func projicerVindretning(koordinat, hovedretning, oploesning int) bool {
	if (math.Abs(float64(koordinat)-float64(hovedretning)) <= float64(oploesning)) {
	return true
	}
	return false
}


func main() {
	resp, _ := http.Get("https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20%3D%20554890&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys")
	defer resp.Body.Close()
	body, _ := ioutil.ReadAll(resp.Body)
	
	
	var dat map[string]interface{}
	json.Unmarshal(body,&dat)

	/* Hent relevant vinddata fra JSON-struktur */
	query := dat["query"].(map[string]interface{})
	results := query["results"].(map[string]interface{})
	channel := results["channel"].(map[string]interface{})
	wind := channel["wind"].(map[string]interface{})
	windSpeedstr := wind["speed"].(string)
	windDirectionstr := wind["direction"].(string)
	item := channel["item"].(map[string]interface{})
	pubDate := item["pubDate"].(string) //tid for observation

	/* Hent temperaturdata */
	condition := item["condition"].(map[string]interface{})
	tempFstr := condition["temp"].(string)
	tempF,_ := strconv.Atoi(tempFstr)
	tempC := float64(tempF-32.0)*5.0/9.0

	/* Hent vejrforhold -- skal fra eng til dk!! */
	//text := condition["text"].(map[string]interface{})


	/* Manipuler vindstørrelser og converter til m/s */
	windSpeedmph, _ := strconv.Atoi(windSpeedstr)
	windSpeedms := 0.447*float64(windSpeedmph)

	/* Hvorfra blæser det? */
	windDirection, _ := strconv.Atoi(windDirectionstr)
	switch {
	case (projicerVindretning(windDirection, 0, OPLOESNING)):
		windDirectionstr = "nord"
	case (projicerVindretning(windDirection, 360, OPLOESNING )):
		windDirectionstr = "nord"
	case (projicerVindretning(windDirection, 45, OPLOESNING )):
		windDirectionstr = "nordøst"
	case (projicerVindretning(windDirection, 90, OPLOESNING )):
		windDirectionstr = "øst"
	case (projicerVindretning(windDirection, 135, OPLOESNING )):
		windDirectionstr = "sydøst"
	case (projicerVindretning(windDirection, 180, OPLOESNING )):
		windDirectionstr = "syd"
	case (projicerVindretning(windDirection, 225, OPLOESNING )):
		windDirectionstr = "sydvest"
	case (projicerVindretning(windDirection, 270, OPLOESNING )):
		windDirectionstr = "vest"
	case (projicerVindretning(windDirection, 315, OPLOESNING )):
		windDirectionstr = "nordvest"
	}

	fmt.Printf("Vejrdata fra: %s.", pubDate)
	fmt.Println("")
	fmt.Println("Temperaturen er", int(tempC), " grader celcius.")
	fmt.Printf("Vindhastigheden er: %.1f sekundmeter. ", windSpeedms)
	fmt.Printf("Det blæser fra %s.", windDirectionstr)
	fmt.Println("")
}

/* Structur på JSON data. Dem, der hedder omit fungerer ligesom alle andre felter og kan bruges med samme syntaks som de andre felter. Se eksemplet under dette, hvis noget er uklart. */
/*type omit *struct{}


type guid struct {
	isPermalink omit `json:"isPermaLink,omitempty"`
	content omit `json:"content,omitempty"`
}


type forecast struct {
	code omit `json:"code,omitempty"`
	date string `json:"date"`
	day string  `json:"day"`
	high string `json:"high"`
	low string `json:"low"`
	text string `json:"text"`
}


// type forecast struct {
// 	forecast1 [5]vejrudsigtEnDag
// }


type condition struct {
	code omit `json:"code,omitempty"`
	date string `json:"date"`
	temp string `json:"temp"`
	text string `json:"text"`
}


type item struct {
	title omit `json:"title,omitempty"`
	lat omit `json:"lat,omitempty"`
	long omit `json:"long,omitempty"`
	link omit `json:"link,omitempty"`
	pubDate string `json:"pubDate"`
	condition condition `json:"condition"`
	description string `json:"description"`
	forecast [5]forecast `json:"forecast"`
	guid guid `json:"guid"`
}


type image struct {
	title omit `json:"title,omitempty"`
	width omit `json:"width,omitempty"`
	link omit `json:"link,omitempty"`
	url omit `json:"url,omitempty"`
}


type astronomy struct {
	sunrise string `json:"sunrise"`
	sunset string `json:"sunset"`
}


type atmosphere struct {
	humidity string `json:"humidity"`
	pressure string `json:"pressure"`
	rising string `json:"rising"`
	visibility string `json:"visibility"`
}


type wind struct {
	chill omit `json:"chill,omitempty"`
	direction string `json:"direction"`
	speed string `json:"speed"`
}

type units struct {
	distance omit `json:"distance,omitempty"`
	pressure omit `json:"pressure,omitempty"`
	speed omit `json:"speed,omitempty"`
	temperature omit `json:"temperature,omitempty"`
}


type location struct {
	city omit `json:"city,omitempty"`
	country omit `json:"country,omitempty"`
	region omit `json:"region,omitempty"`
}


type channel struct {
	title omit `json:"title,omitempty"`
	link omit `json:"link,omitempty"`
	description omit `json:"description,omitempty"`
	language omit `json:"language,omitempty"`
	lastBuildDate omit `json:"lastBuildDate,omitempty"`
	ttl omit `json:"ttl,omitempty"`
	location location `json:"location,omitempty"`
	units omit `json:"units,omitempty"`
	wind wind `json:"wind"`
	atmosphere `json:"atmosphere"`
	astronomy `json:"astronomy"`
	image `json:"image"`
	item `json:"item"`
}

type results struct {
	channel channel `json:"channel"`
}

type query struct {
	count omit `json:"count,omitempty"`
	created omit `json:"created,omitempty"`
	lang string `json:"lang"`
	results results `json:"results"`
}
*/




/* Eksempel på hentet data */
/*
{"query":
	{"count":1,"created":"2015-05-13T14:30:28Z","lang":"en-US",
		"results":
		{"channel":
			{"title":"Yahoo! Weather - Copenhagen, DK","link":"http://us.rd.yahoo.com/dailynews/rss/weather/Copenhagen__DK/*http://weather.yahoo.com/forecast/DAXX0009_f.html","description":"Yahoo! Weather for Copenhagen, DK","language":"en-us","lastBuildDate":"Wed, 13 May 2015 3:50 pm CEST","ttl":"60",
				"location":
				{"city":"Copenhagen","country":"Denmark","region":""},
				"units":
				{"distance":"mi","pressure":"in","speed":"mph","temperature":"F"},
				"wind":
				{"chill":"54","direction":"270","speed":"22"},
				"atmosphere":
				{"humidity":"62","pressure":"29.83","rising":"0","visibility":"6.21"},
				"astronomy":{"sunrise":"5:01 am","sunset":"9:08 pm"},
				"image":
				{"title":"Yahoo! Weather","width":"142","height":"18","link":"http://weather.yahoo.com","url":"http://l.yimg.com/a/i/brand/purplelogo//uh/us/news-wea.gif"},
				"item":
				{"title":"Conditions for Copenhagen, DK at 3:50 pm CEST","lat":"55.67","long":"12.55","link":"http://us.rd.yahoo.com/dailynews/rss/weather/Copenhagen__DK/*http://weather.yahoo.com/forecast/DAXX0009_f.html","pubDate":"Wed, 13 May 2015 3:50 pm CEST",
					"condition":
					{"code":"28","date":"Wed, 13 May 2015 3:50 pm CEST","temp":"54","text":"Mostly Cloudy"},
					"description":
					"\n<img src=\"http://l.yimg.com/a/i/us/we/52/28.gif\"/><br />\n<b>Current Conditions:</b><br />\nMostly Cloudy, 54 F<BR />\n<BR /><b>Forecast:</b><BR />\nWed - Clouds Early/Clearing Late. High: 56 Low: 45<br />\nThu - Partly Cloudy. High: 60 Low: 46<br />\nFri - Sunny. High: 58 Low: 44<br />\nSat - Mostly Cloudy. High: 56 Low: 44<br />\nSun - PM Light Rain/Wind. High: 56 Low: 44<br />\n<br />\n<a href=\"http://us.rd.yahoo.com/dailynews/rss/weather/Copenhagen__DK/*http://weather.yahoo.com/forecast/DAXX0009_f.html\">Full Forecast at Yahoo! Weather</a><BR/><BR/>\n(provided by <a href=\"http://www.weather.com\" >The Weather Channel</a>)<br/>\n",
					"forecast":
					[{"code":"29","date":"13 May 2015","day":"Wed","high":"56","low":"45","text":"Clouds Early/Clearing Late"},
					 {"code":"30","date":"14 May 2015","day":"Thu","high":"60","low":"46","text":"Partly Cloudy"},
					 {"code":"32","date":"15 May 2015","day":"Fri","high":"58","low":"44","text":"Sunny"},
					 {"code":"28","date":"16 May 2015","day":"Sat","high":"56","low":"44","text":"Mostly Cloudy"},
					 {"code":"11","date":"17 May 2015","day":"Sun","high":"56","low":"44","text":"PM Light Rain/Wind"}]
					,"guid":
					{"isPermaLink":"false","content":"DAXX0009_2015_05_17_7_00_CEST"}}}}}}


*/
