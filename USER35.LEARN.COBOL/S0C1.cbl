      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    S0C1.
       AUTHOR.        ABEND-S0C1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL
           ASSIGN TO UT-S-PAYCHECK
             ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PAYROLL-REC.
       01  PAYROLL-REC  PIC X(80).
       WORKING-STORAGE SECTION.
       01   MONTH-IN                PIC S9(02)   COMP.
           88 VALID-MONTH VALUES ARE 1 THRU 12.
       01   WS-USER-ABEND-CODE      PIC S9(04)   COMP.
       PROCEDURE DIVISION.
      *--- Cause ABEND
           READ PAYROLL.
           OPEN INPUT PAYROLL.