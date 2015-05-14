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

// const (
// 	North := 1
// 	North-by-east:= 2
// 	North-northeast:= 3    
// 	Northeast-by-north:= 4
// 	Northeast:= 5
// 	Northeast-by-east:= 6
// 	East-northeast:= 7
// 	East-by-north:= 8   
// 	East:= 9    
// 	East-by-south:= 10
// 	East-southeast:= 11   
// 	Southeast-by-east := 12
// 	Southeast:= 13
// 	Southeast-by-south:= 14
// 	South-southeast := 15
// 	South-by-east := 16
// 	South := 17  
// 	South-by-west := 18
// 	South-southwest  := 19  
// 	Southwest-by-south := 20
// 	Southwest := 21
// 	Southwest-by-west := 22
// 	West-southwest := 23
// 	West-by-south := 24 
// 	West := 25 
// 	West-by-north:= 26
// 	West-northwest := 27   
// 	Northwest-by-west := 28 
// 	Northwest := 29
// 	Northwest-by-north := 30
// 	North-northwest := 31
// 	North-by-west := 32

// )


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

	fmt.Println("Vejrdata fra: ", pubDate)
	fmt.Println("Temperaturen er", int(tempC), " grader celcius.")
	fmt.Printf("Vindhastigheden er: %.1f sekundmeter. ", windSpeedms)
	fmt.Println("Det blæser fra " + windDirectionstr)
	//fmt.Println("Vejrforholdene beskrives som:")
}
