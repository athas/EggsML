package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().Unix())
	remarks := []string{
		"Præcist",
		"Genau",
		"Mais oui",
		"Exactly",
		"Jeg kunne ikke sige det bedre selv",
	}
	fmt.Printf("%s.", remarks[rand.Intn(len(remarks))])
}

