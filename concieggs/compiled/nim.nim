#!/bin/sh ":"; exec nim compile --run "$0" "$@"

from strutils import splitWhitespace, strip, join, parseInt, parseUInt
from os import commandLineParams, getEnv
from osproc import execProcess
from random import randomize, random
import sequtils

randomize()

proc getState(): seq[string] =
  execProcess("cat nimdb 2>/dev/null").splitWhitespace()

var currentState = getState()

proc setState(s: seq[string]) =
  discard execProcess("echo '" & s.join(" ") & "' > nimdb")

proc getFirstPlayer(s: seq[string]): string =
  s[1]

proc getSecondPlayer(s: seq[string]): string =
  s[2]

proc printState(s: seq[string]) =
  echo "Spillet ser således ud: ", s[3..len(s)-1].join(" ").splitWhitespace().join(" "), " og det er ", getFirstPlayer(s), "s tur. Brug `nim N BIN` for at tage N pinde fra bunke BIN"

proc isFresh(s: seq[string]): bool =
  s == @[]

proc isPlaying(s: seq[string]): bool =
  s.len() > 3 and s[0] == "spiller"

proc gameOver(s: seq[string]): bool =
  s.len() == 4 and s[3] == ""

proc isWaiting(s: seq[string]): bool =
  s.len() == 2 and s[0] == "venter"

proc status() =
  if isFresh(currentState):
    echo "Ingen spiller nim lige nu. Du kan jo starte et spil?"
  elif isWaiting(currentState):
    echo getFirstPlayer(currentState), " venter tålmodigt på en modspiller. Skal det være dig?"
  elif isPlaying(currentState):
    echo getFirstPlayer(currentState), " og ", getSecondPlayer(currentState), " er i gang med et spil."
    printState(currentState)

proc getBin(s: seq[string], n: int): int =
  parseInt(s[n+3])

proc setBin(s: var seq[string], n: int, val: int) =
  if val <= 0:
    s[n+3] = ""
  else:
    s[n+3] = $val

proc numBins(s: seq[string]): int =
  len(s) - 3

var currentUser = getEnv("EGGS_USER").strip()

if currentUser == "":
  echo "Noget gik galt..."
  quit()

# echo "currentState: ", $currentState
# echo "Got args: ", commandLineParams(), " and user: ", currentUser

var args = commandLineParams()

proc newGame() =
  if isFresh(currentState):
    setState(@["venter", currentUser])
    echo "Den er fjong, men du mangler altså en at spille imod"
  elif isWaiting(currentState):
    var firstPlayer = getFirstPlayer(currentState)
    echo "Super! Du skal spille imod ", firstPlayer

    var numBins = random(8)+2
    var state = @["spiller", firstPlayer, currentUser, newSeqWith(numBins, random(8)+2).join(" ")]

    setState(state)

    printState(state)
    echo firstPlayer, ": Du skal starte."
  else:
    var nextPlayer = getFirstPlayer(currentState)
    echo "Der er altså allerede nogen der spiller. Du kan jo skubbe til ", nextPlayer, " for at få lidt gang i spillet"

proc help() =
  echo "Brug `nim status` for at få en oversigt over spillets status, brug `nim spil` eller bare `nim` for at komme i gang."

if len(args) == 0:
  newGame()
else:
  case args[0]
  of "status": status()
  of "spil": newGame()
  of "hjælp": help()
  else:
    if isPlaying(currentState):
      if getFirstPlayer(currentState) == currentUser:
        var numPins = parseInt(args[0])
        if numPins < 1:
          echo "Den går altså ikke fister, du skal fjerne 1 eller flere pinde."
          quit()

        var bin = parseInt(args[1])
        if bin < 0 or bin > numBins(currentState):
          echo "Den bunke findes ikke."
          quit()

        var newNumber = getBin(currentState, bin) - numPins
        setBin(currentState, bin, newNumber)

        if gameOver(currentState):
          echo "Du vandt, ", currentUser, "!! ", getSecondPlayer(currentState), " er den store fede taber."
          setState(@[])
          quit()

        var tmp = currentState[2]
        currentState[2] = currentState[1]
        currentState[1] = tmp
        setState(currentState)

        echo "Den er fjong."
        printState(currentState)

      else:
        echo "Hvad har du gang i? Det er ", getFirstPlayer(currentState), "s tur..."

    else:
      echo "Du skal først starte et spil. Prøv med bare `nim`..."
