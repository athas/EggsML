package main

import (
	"net/http"
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"bytes"
)

const (
	ebolaUrl = "http://en.wikipedia.org/w/index.php?title=Ebola_virus_epidemic_in_West_Africa&action=raw"
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
	reg, _ := regexp.Compile("\\{\\{noflag\\}\\}.?Total: ([0-9,]+) / ([0-9,]+)")
	if reg == nil {
		fmt.Println("Nå.  Der er ingen tal.")
		return
	}
	res := reg.FindAllSubmatch(body, -1)
	cases, err := strconv.Atoi(bytes.NewBuffer(bytes.Replace(res[0][1], []byte(","), []byte(""), -1)).String())
	if err != nil {
		fmt.Println("Katastrofe!  Tallene er i udu.")
		return
	}
	deaths, err := strconv.Atoi(bytes.NewBuffer(bytes.Replace(res[0][2], []byte(","), []byte(""), -1)).String())
	if err != nil {
		fmt.Println("Katastrofe!  Tallene er i udu.")
		return
	}
	fmt.Printf("Ebola har indtil nu smittet %d og slået %d ihjel siden december 2013.\n", cases, deaths)
	fmt.Println("Du kan hjælpe ved at tage til Guinea, Liberia eller Sierra Leone og røre ved nogle mennesker.")
}
