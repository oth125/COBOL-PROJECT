       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TABLES00.
      *
      * ***************************************************
      * **************************************************
       INSTALLATION.  IBM.
       DATE-WRITTEN.  01-01-2009.
       DATE-COMPILED. 01-01-2009.
       SECURITY.   NONE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   IBM.
       OBJECT-COMPUTER.   IBM.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE   ASSIGN TO UT-S-EMPFILE
                  ORGANIZATION IS SEQUENTIAL.
           SELECT PHONES-REPORT ASSIGN TO UT-S-PHONEOUT
                  ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.
       01  EMPLOYEE-RECORD.
           05  EMPLOYEE-NAME           PIC X(20).
           05 EMPLOYEE-PHONE-TAB OCCURS 3 TIMES.
                   10  PHONE           PIC X(13).
           05  FILLER                  PIC X(21).
       FD  PHONES-REPORT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.
       01  REPORT-LINE-OUT             PIC X(80).
       WORKING-STORAGE SECTION.
       01  SWITCHES-IN-PROGRAM.
           05  SW-END-OF-DATA          PIC X VALUE 'N'.
               88  END-OF-DATA               VALUE 'Y'.
       PROCEDURE DIVISION.
       000-TOP-LEVEL.
           PERFORM 100-INITIALIZATION.
           PERFORM 200-PROCESS-RECORDS UNTIL END-OF-DATA.
           PERFORM 300-WRAP-UP.
           GOBACK.
       100-INITIALIZATION.
           OPEN INPUT  EMPLOYEE-FILE.
           OPEN OUTPUT PHONES-REPORT.
           PERFORM 230-READ-A-RECORD.
       200-PROCESS-RECORDS.
           MOVE EMPLOYEE-RECORD TO REPORT-LINE-OUT.
           DISPLAY 'EMPLOYEE-NAME: 'EMPLOYEE-NAME.
           DISPLAY '      CELL-PHONE: 'PHONE(1).
           DISPLAY '      LAND-LINE: ' PHONE(2).
           DISPLAY '      WORK-PHONE: ' PHONE(3).
           DISPLAY ' '.
           WRITE REPORT-LINE-OUT.
           PERFORM 230-READ-A-RECORD.
       230-READ-A-RECORD.
           READ EMPLOYEE-FILE AT END MOVE 'Y' TO SW-END-OF-DATA.
       300-WRAP-UP.
           CLOSE PHONES-REPORT  EMPLOYEE-FILE.