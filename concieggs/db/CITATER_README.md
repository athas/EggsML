# Citat-systemet

Dette er concieggs' system til at håndtere citater fra kendte personer.

## Kommandoer

### `citat`
Henter et tilfældigt citat fra databasen og præsenterer det med en passende begrundelse.

**Brug:**
```
citat          - henter et tilfældigt citat fra databasen
citat --ai     - bruger ChatGPT til at generere et passende citat
```

**Eksempel:**
```
> citat
runner: "Jeg har aldrig betalt skat med glæde." - Mogens Glistrup
runner: Dette citat vil uden tvivl bringe klarhed til debatten.
```

### `tilføj-citat`
Tilføjer et nyt citat til databasen.

**Brug:**
```
tilføj-citat "citat tekst" "Person Navn"
```

**Eksempel:**
```
> tilføj-citat "At være eller ikke at være" "William Shakespeare"
runner: Citatet er tilføjet til databasen. Nu er der 21 citater i alt.
```

### `scrape-citater`
Scraper citater fra sjove-citater.dk og tilføjer dem til databasen.

**Brug:**
```
scrape-citater [person-url]
```

**Eksempel:**
```
> scrape-citater mogens-glistrup
runner: Scraping færdig. Der er nu 35 citater i databasen.
```

## Citatdatabasen

Citater gemmes i `concieggs/db/citater` med formatet:
```
citat tekst|Person Navn
```

Du kan manuelt redigere denne fil for at tilføje eller fjerne citater.

## Tilføj flere citater

Der er flere måder at tilføje citater på:

1. **Manuelt via kommandoen:**
   ```
   tilføj-citat "Dit citat her" "Forfatter"
   ```

2. **Rediger filen direkte:**
   ```
   echo "Nyt citat her|Forfatter" >> concieggs/db/citater
   ```

3. **Scrape fra websider:**
   ```
   scrape-citater person-url-slug
   ```

4. **Brug AI til at generere:**
   ```
   citat --ai
   ```

## Format

Alle citater skal følge formatet: `citat|person`
- Citatet må ikke indeholde pipe-tegnet (|)
- Personnavnet kommer efter pipe-tegnet
- Hver linje er ét citat
