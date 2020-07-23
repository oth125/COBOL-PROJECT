       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVS.
      ***** This MODULE Workshop 3.1b - Create new COBOL program
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FAV-REC.
          05 ARTIST-NAME             PIC X(30).
          05 NUMBER-OF-MUSICIAN      PIC 9(02).
          05 MUSICAL-GENRE           PIC X(12).
          05 COST.
             10 CD-COST              PIC 9(3)V99.
             10 SHIPPING-COST        PIC 9(2)V99.
             10 TAX                  PIC 9(2)V99.
          05 BAND-IS-STILL-TOGETHER  PIC X(1).
          05 TOTAL-COST              PIC 9(4)V99.
          05 TOTAL-COST2             PIC 9(4).99.
          05 TOTAL-COST3             PIC 99999.9999.
          05 TOTAL-COST4             PIC +99999.9999.
          05 TOTAL-COST5             PIC -99999.9999.
       PROCEDURE DIVISION.
           MOVE "Millard" TO ARTIST-NAME.
           MOVE "The second" TO MUSICAL-GENRE.
           MOVE 19 TO NUMBER-OF-MUSICIAN.
           MOVE 200.5 TO CD-COST.
           MOVE 50.5 TO SHIPPING-COST.
           MOVE .55 TO TAX.
           MOVE 'T' TO BAND-IS-STILL-TOGETHER.
           COMPUTE TOTAL-COST =
              (CD-COST + SHIPPING-COST + TAX).
           DISPLAY "Name: " ARTIST-NAME.
           DISPLAY "NUMBER-OF-MUSICIAN : " NUMBER-OF-MUSICIAN.
           DISPLAY "MUSICAL-GENRE: " MUSICAL-GENRE.
           IF BAND-IS-STILL-TOGETHER = 'T'
              DISPLAY "BAND-IS-STILL-TOGETHER: True "
           ELSE
              DISPLAY "BAND-IS-STILL-TOGETHER: False "
           END-IF.
           COMPUTE TOTAL-COST2 =(CD-COST + SHIPPING-COST + TAX).
           COMPUTE TOTAL-COST3 =(CD-COST + SHIPPING-COST + TAX).
           COMPUTE TOTAL-COST4 =(CD-COST + SHIPPING-COST + TAX).
           COMPUTE TOTAL-COST5 = -1 *(CD-COST + SHIPPING-COST + TAX).
           DISPLAY "TOTAL-COST: " TOTAL-COST.
           DISPLAY "TOTAL-COST2: " TOTAL-COST2.
           DISPLAY "TOTAL-COST3: " TOTAL-COST3.
           DISPLAY "TOTAL-COST4: " TOTAL-COST4.
           DISPLAY "TOTAL-COST5: " TOTAL-COST5.
           GOBACK.
