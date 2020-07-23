       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROL0A.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-SPACE             PIC X(1) VALUE SPACE.
       01  PAYSTUB-V20.
           05 REPORT-DATE                       PIC XX/XX/XXXX.
           05 FULLNAME.
                10 WS-FNAME                     PIC X(14).
                10 WS-LNAME                     PIC X(16).
           05 WS-ADDRESS.
                10 WS-CITY                      PIC X(20).
                10 WS-STREET.
                    15  WS-STREET-NBR           PIC 9(03).
                    15  WS-STREET-NAME          PIC X(20).
                10 WS-STATE     PIC X(02).
                10 ZIP.
                     15 WS-ZIP-FIRST-5          PIC X(05).
                     15 FILLER                  PIC X(01) VALUE '-'.
                     15 WS-ZIP-PLUS-4           PIC X(04).
           05 WS-PAYROLL.
                10 WS-EMP-TYPE                  PIC X(01).
                    88  FULL-TIME       VALUE 'F'.
                    88  PART-TIME       VALUE 'P'.
                10 WS-FULL-TIME.
                    15 WS-FULL-TIME-SALARY       PIC 9(6)V99 VALUE
                               ZEROS.
                    15 WS-FULL-TIME-BONUS           PIC V99.
                10  WS-HOURLY.
                    15  WS-HOURS-WORKED             PIC 9(02).
                    15  WS-RATE                     PIC 9(3).
                    15  WS-OT-COMP                  PIC V99.
                    15  WS-40-DIFF                  PIC 99.
                    15  WS-PART-TIME-SALARY         PIC 9(6)V99.
                    15  FILLER                      PIC X(40).
      *
       PROCEDURE DIVISION.
           PERFORM ASSIGNMENT-PARAGRAPH.
           PERFORM DISPLAY-DATA-PARAGRAPH.
           GOBACK.
       ASSIGNMENT-PARAGRAPH.
           MOVE 'F' TO WS-EMP-TYPE.
           MOVE  "Millard Fillmore" TO FULLNAME.
           MOVE  61 TO WS-STREET-NBR.
           MOVE 'BRIGHAM TAVERN LANE' TO WS-STREET-NAME.
           MOVE  FUNCTION CURRENT-DATE TO REPORT-DATE.
           MOVE 'NC' TO WS-STATE.
           MOVE '90210' TO WS-ZIP-FIRST-5.
           MOVE '1111'  TO WS-ZIP-PLUS-4.
           PERFORM CONDITIONAL-SELECTION.
       CONDITIONAL-SELECTION.
           IF  FULL-TIME
               PERFORM FULL-TIME-PARA
           ELSE IF PART-TIME
                PERFORM PART-TIME-PARA
           ELSE
                DISPLAY 'BAD DATA'
           END-IF.
       FULL-TIME-PARA.
           MOVE 50000 TO WS-FULL-TIME-SALARY.
           MOVE .2      TO WS-FULL-TIME-BONUS.
           COMPUTE WS-FULL-TIME-SALARY =
                WS-FULL-TIME-SALARY * (WS-FULL-TIME-BONUS).
       PART-TIME-PARA.
           MOVE 45  TO WS-HOURS-WORKED.
           MOVE 15  TO WS-RATE.
           MOVE .2  TO WS-OT-COMP.
           COMPUTE WS-40-DIFF = WS-HOURS-WORKED - 40.
           COMPUTE WS-PART-TIME-SALARY =
                (WS-HOURS-WORKED * WS-RATE).
           COMPUTE WS-PART-TIME-SALARY = WS-PART-TIME-SALARY +
                   (WS-40-DIFF * WS-RATE) * (1 + WS-OT-COMP).
       DISPLAY-DATA-PARAGRAPH.
           DISPLAY "FULL-NAME: " FULLNAME.
           DISPLAY "ADDRESS: " WS-ADDRESS.
           DISPLAY "PAY-STUB: " WS-PAYROLL.