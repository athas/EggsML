package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"math/rand"
	"net/http"
	"regexp"
	"strconv"
	"time"
)

const (
	ebolaUrl = "https://en.wikipedia.org/wiki/List_of_Ebola_outbreaks"
)

func main() {
	resp, err := http.Get(ebolaUrl)
	if err != nil {
		fmt.Println("Katastrofe!  Nogen har mistet kontakten til Wikipedia!")
		return
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("Øv.  Jeg fik noget lort tilbage.")
		return
	}
	reg, _ := regexp.Compile("(?s)<tr>.*?<td>.*?</td>.*?<td>.*?</td>.*?<td>.*?</td>.*?<td>.*?([0-9,]+).*?</td>.*?<td>.*?([0-9,]+).*?</td>.*?<td>.*?</td>.*?<td>.*?</td>.*?</tr>")
	if reg == nil {
		fmt.Println("Nå.  Der er ingen tal.")
		return
	}
	res := reg.FindAllSubmatch(body, -1)
	if len(res) < 1 {
		fmt.Println("Nå.  Der var ingen tal.")
		return
	}
	var cases, deaths int
	for _, e := range res {
		tcases, err := strconv.Atoi(bytes.NewBuffer(bytes.Replace(e[1], []byte(","), []byte(""), -1)).String())
		if err != nil {
			fmt.Println("Katastrofe!  Tallene er i udu.")
			return
		}
		tdeaths, err := strconv.Atoi(bytes.NewBuffer(bytes.Replace(e[2], []byte(","), []byte(""), -1)).String())
		if err != nil {
			fmt.Println("Katastrofe!  Tallene er i udu.")
			return
		}
		cases += tcases
		deaths += tdeaths
	}
	fmt.Printf("Ebola har indtil nu smittet %d og slået %d ihjel.\n", cases, deaths)
	countries := []string{
		"Sudan",
		"Zaire",
		"Gabon",
		"Uganda",
		"Congo",
		"Det Andet Congo",
		"Guinea",
		"Liberia",
		"Sierra Leone",
		"Nigeria",
		"Mali",
		"Amerikas Forenede Stater",
		"Senegal",
		"Spanien",
		"Storbritanniens og Nordirlands Forenede Kongerige",
		"Italien",
	}
	rand.Seed(time.Now().Unix())
	fmt.Printf("Du kan hjælpe ved at tage til %s og røre ved nogle mennesker.\n", countries[rand.Intn(len(countries))])
}

