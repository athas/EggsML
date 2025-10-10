# 🎰🎰 CONCIEGGS CASINO 🎰🎰

Et rigtigt casino med concieggs-poletter!

## Kommandoer

### Casino hovedkommandoer

- `casino hjælp` - Vis hjælp
- `casino hent` - Hent dine daglige poletter (kun én gang om dagen)
- `casino balance` - Se hvor mange poletter du har
- `casino roulette [...]` - Spil roulette

### Roulette

- `casino roulette sats <beløb> <hvad>` - Sats poletter på roulette
- `casino roulette spin` - Drej rouletten nu (ellers drejes den automatisk efter 30 sekunder)
- `casino roulette status` - Se status for aktuelle spil
- `casino roulette hjælp` - Vis hjælp for roulette

Du kan også bruge `roulette` direkte som en kommando.

## Poletter (Tokens)

- Hver spiller får 100 poletter om dagen
- Du kan kun hente poletter én gang om dagen
- Hvis concieggs er irriteret på dig (høj annoyance), får du færre poletter
- Poletter akkumuleres, så du kan spare op

## Roulette regler

### Hvad kan du satse på?

- **Enkelt nummer (0-36)**: `casino roulette sats 10 17`
- **Rød eller sort**: `casino roulette sats 10 rød` eller `casino roulette sats 10 sort`
- **Lige eller ulige**: `casino roulette sats 10 lige` eller `casino roulette sats 10 ulige`
- **Lav (1-18)**: `casino roulette sats 10 1-18`
- **Høj (19-36)**: `casino roulette sats 10 19-36`

### Udbetalinger

- **Enkelt nummer**: 35:1 (du vinder 35 gange dit indsats + får dit indsats tilbage)
- **Rød/sort, lige/ulige, lav/høj**: 1:1 (du vinder samme som dit indsats + får dit indsats tilbage)

### Multiplayer

- Flere spillere kan satse i samme runde
- Når den første spiller satser, starter en 30-sekunders timer
- Andre spillere kan satse inden tiden løber ud
- Du kan bruge `casino roulette spin` for at dreje rouletten med det samme

## Eksempel spil

```bash
# Hent dine daglige poletter
casino hent

# Check din balance
casino balance

# Start en roulette runde
casino roulette sats 20 rød

# (Andre spillere kan nu satse...)

# Drej rouletten
casino roulette spin
```

## Ansvarligt spil

Husk at du kan tilmelde dig https://www.spillemyndigheden.dk/rofus hvis du vil undgå at komme til at spille dine penge væk. På nuværende tidspunkt er en lignende ordning for concieggs-poletterne dog ikke planlagt.

## Implementering

Casinoet bruger JSON-baseret tilstandsstyring i databasen:
- `casino` fil indeholder token balancer og sidste claim tidspunkter
- `roulette_game` fil indeholder aktiv roulette spil tilstand

Implementeret i Python for nem multiplayer-håndtering. Kan senere portes til rash når ergonomien forbedres.
