package main

import (
	"os"
	"time"
	"strings"
	"io/ioutil"
	"net/http"
	"encoding/json"
	"math/rand"
	"fmt"
)

func searchUrl(args []string) string {
	return "http://oeis.org/search?fmt=json&q=" + strings.Join(args, ",")
}

func main() {
	rand.Seed(time.Now().Unix())

	resp, err := http.Get(searchUrl(os.Args[1:]))
	if err != nil {
		fmt.Println("OEIS ER NEDE!!!  RING TIL 113!")
		return
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("JEG FIK NOGET DÅRLIG DATA TILBAGE!!!")
		return
	}
	var res map[string]interface{}
	err = json.Unmarshal(body, &res)
	if err != nil {
		fmt.Println("HVAD SUSAN SKER DER LIGE FOR DEN JSON???")
		return
	}
	results := res["results"]
	if results == nil {
		fmt.Println("Eurikke, en ny talrække!  Den findes i hvert fald ikke i databasen.")
		return
	}
	results1 := results.([]interface{})
	result := results1[rand.Intn(len(results1))].(map[string]interface{})
	url := fmt.Sprintf("http://oeis.org/A%06d", int(result["number"].(float64)))
	fmt.Printf("\00302Det er jo den fra %s — %s\nSe bare her: %s", url, result["name"], result["data"])
}
