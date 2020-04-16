        IDENTIFICATION DIVISION.
        PROGRAM-ID. HYGGE.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 HYGGE-AREA.
          05 TIME-RESULT   PIC 9(12) VALUE ZERO.
          05 RANDOM-RESULT PIC 9     VALUE ZERO.
        01 SEED-TIME.
          05 HOURS    PIC 99.
          05 MINUTES  PIC 99.
          05 SECONDS  PIC 99.
          05 MS       PIC 99.
        
        PROCEDURE DIVISION.
        
      * **************************************************** *
      * Først skal vi lige udregne tid så vi kan få ægte     *
      * vilkårlige tal!  DET ER VIGTIGT!                     *
      * **************************************************** *
        
        MOVE FUNCTION CURRENT-DATE (9:8) TO SEED-TIME.
        COMPUTE TIME-RESULT = (SECONDS + HOURS + MINUTES + MS)
        COMPUTE RANDOM-RESULT = FUNCTION RANDOM (TIME-RESULT) * 1000
        
      * **************************************************** *
      * Ellers ved vi jo slet ikke om der skal hygges!       *
      * RANDOM-RESULT er en værdi fra 0-9, så der er         *
      * desværre god sandsynlighed for ingen hygge!          *
      * **************************************************** *
        
        IF RANDOM-RESULT = 0
          DISPLAY "Så skal der hygges!".
        IF RANDOM-RESULT = 1
          DISPLAY "Jeg henter et par øl.".
        IF RANDOM-RESULT = 2
          DISPLAY "/me tager et brætspil frem.".
        IF RANDOM-RESULT = 3
          DISPLAY "Gad vide om jeg ikke har en gammel LP"
          " der stadig virker.".
        IF RANDOM-RESULT = 4
          DISPLAY "Er der egentligt en hyggegrænse?".
        IF RANDOM-RESULT > 4
          DISPLAY "Åh, jeg tror ikke jeg orker at hygge.".
        
        STOP RUN.
