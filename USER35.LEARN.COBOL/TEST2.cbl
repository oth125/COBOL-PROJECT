       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCOB2.
      * Comment: This program Displays a number of text strings
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 ACCT-VAL-A         PIC S9(15)V9(02) COMP-3 .

       77 ACCT-VAL-B-01      PIC S9(16)V9(02) VALUE 0.
       77 ACCT-VAL-B-02      PIC S9(13)V9(05) VALUE 0 .
       77 ACCT-VAL-C         PIC S9(16)V9(02) VALUE 0 .
       77 ACCT-RESULT        PIC S9(15)V9(02) VALUE 0 .
       PROCEDURE DIVISION.

           MOVE 2500.87 TO ACCT-VAL-A
           MOVE 12285 TO ACCT-VAL-B-01
           MOVE 12285 TO ACCT-VAL-B-02
           MOVE 4387.5 TO ACCT-VAL-C
           COMPUTE ACCT-RESULT  ROUNDED =
             (ACCT-VAL-A / ( ACCT-VAL-B-01 +  ACCT-VAL-C) * 100 ).
             DISPLAY ACCT-RESULT
           COMPUTE ACCT-RESULT  ROUNDED =
             (ACCT-VAL-A / ( ACCT-VAL-B-02 +  ACCT-VAL-C) * 100 ).
             DISPLAY ACCT-RESULT
           GOBACK.