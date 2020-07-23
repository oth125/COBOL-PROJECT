      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    B37.
       AUTHOR.        ABEND-B37.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT B37-FILE
           ASSIGN TO UT-S-B37
             ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  B37-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS B37-REC.
       01  B37-REC  PIC X(80).
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           OPEN OUTPUT B37-FILE.
           MOVE HIGH-VALUES TO B37-REC.
           PERFORM 100000 TIMES
                WRITE B37-REC END-PERFORM.
           CLOSE B37-FILE.
           GOBACK.