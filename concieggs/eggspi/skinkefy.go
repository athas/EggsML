package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"
)

const (
	skinkeFactor = 25
	skinkeLength = 4
	scRegular    = iota + 1
	scCapitalised
	scFullUpper
)

type skinkeList []string

var Skinker skinkeList
var IkkeSkinker skinkeList

func main() {
	rand.Seed(time.Now().Unix())
	read := bufio.NewReader(os.Stdin)
	str, _ := read.ReadString('\n')
	r, _ := regexp.Compile("([\\p{L}]+)")
	out := strings.Trim(r.ReplaceAllStringFunc(str, skinkefy), "\n ")
	fmt.Println(out)
}

func oneSkinke(condition int, plural bool, definitive bool) string {
	var word string
	switch {
	case plural:
		word = "skinker"
	case definitive:
		word = "skinken"
	default:
		word = "skinke"
	}
	switch condition {
	case scFullUpper:
		word = strings.ToUpper(word)
	case scCapitalised:
		// Get first rune
		r, s := utf8.DecodeRuneInString(word)
		rs := strconv.QuoteRune(unicode.ToUpper(r))
		word = rs[1:len(rs)-1] + word[s:]
	}
	return word
}

func skinkefy(word string) string {
	camelCaseCheckR, _ := regexp.Compile("[\\p{Ll}][\\p{Lu}][\\p{L}]+")
	camelCaseR, _ := regexp.Compile("([\\p{Lu}][\\p{Ll}]+|[\\p{Lu}]+)")
	if camelCaseCheckR.MatchString(word) {
		word = camelCaseR.ReplaceAllStringFunc(word, skinkefy)
	} else {
		if (rand.Intn(100) <= skinkeFactor &&
			!IkkeSkinker.Contains(word) &&
			len(word) >= skinkeLength) ||
			Skinker.Contains(word) {
			Skinker.Append(word)
			var skinkeCondition int
			first, _ := utf8.DecodeRuneInString(word)
			switch {
			case strings.ToUpper(word) == word:
				skinkeCondition = scFullUpper
			case unicode.ToUpper(first) == first:
				skinkeCondition = scCapitalised
			default:
				skinkeCondition = scRegular
			}
			end, size := utf8.DecodeLastRuneInString(word)
			end2, _ := utf8.DecodeLastRuneInString(word[:len(word)-size])
			end = unicode.ToUpper(end)
			end2 = unicode.ToUpper(end2)
			word = oneSkinke(skinkeCondition, end == 'R', end2 == 'E' && (end == 'T' || end == 'N'))
		} else {
			IkkeSkinker.Append(word)
		}
	}
	return word
}

func (sl *skinkeList) Contains(str string) bool {
	str = strings.ToUpper(str)
	for _, s := range *sl {
		if s == str {
			return true
		}
	}
	return false
}

func (sl *skinkeList) Append(str string) {
	str = strings.ToUpper(str)
	*sl = append(*sl, str)
}
