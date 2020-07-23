       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO2.
      * Comment: This program Displays a number of text strings
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       PROCEDURE DIVISION.
           PERFORM 5 TIMES
             DISPLAY "Name: Martin Mullins"
           END-PERFORM
           DISPLAY "Address: 61 Brigham Tavern Lane, Coven"
           DISPLAY "Today's Date: 50/10/2020".
           DISPLAY "Hours Worked: 23".
           DISPLAY "Gross Pay: 00437".
           GOBACK.