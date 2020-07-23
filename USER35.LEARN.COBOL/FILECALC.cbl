       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILECALC.
      * This program reads a file of input values into INVALS-WS
      * The operation read into the W-S structure drives the arithmetic
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVALS ASSIGN TO INVALS ORGANIZATION IS SEQUENTIAL.
           SELECT REPORTFILE ASSIGN TO PRTLINE.
       DATA DIVISION.
       FILE SECTION.
       FD  INVALS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INVALS-REC.
       01  INVALS-REC  PIC X(80).
        FD REPORTFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REPORT-REC.
       01 REPORT-REC .
           05 VAL1-RPT     PIC X(10) VALUE SPACES .
           05 OPERATION-CODE    PIC X(20) VALUE SPACES .
           05 VAL2-RPT   PIC X(10) VALUE SPACES .
           05 EQUAL-TEXT    PIC X(3) VALUE ' = ' .
           05 RESULT-RPT   PIC  9(5).99  VALUE ZEROS .
           05 FILLER   PIC X(20) VALUE SPACES  .
       WORKING-STORAGE SECTION.
      * End of File switch
       01  INVALS-EOF               PIC X(1) VALUE SPACE.
            88   EOF-REACHED        VALUE  'Y'.
       01  INVALS-WS.
           05  OPERATION  PIC X(1) .
               88  ADD-OPERATION            VALUE  'A' 'a'.
               88  SUBTRACT-OPERATION       VALUE  'S'  's'.
               88  MULTIPLY-OPERATION       VALUE  'M' 'm'.
               88  SQUARE-ROOT-OPERATION    VALUE  'R'  'r'.
               88  DIVIDE-OPERATION         VALUE  'D' 'd'.
           05  INVALS-1             PIC 99V99.
           05  INVALS-2             PIC 99.
           05  INVALS-2X REDEFINES INVALS-2  PIC X(2).
           05  TEXT1                PIC X(10) VALUE ' RESULT : ' .
           05  INVALS-RZLT          PIC 99999.99.
       01 HEADER-1.
          05 FILLER    PIC X(30)   VALUE SPACES.
          05 FILLER    PIC X(30)   VALUE  ' CALCULATION REPORT '.
       01  INVALS1-EDITED     PIC 99.99.
       01  INVALS-1X REDEFINES INVALS1-EDITED  PIC X(5).
       PROCEDURE DIVISION.
           PERFORM 000-Housekeeping.
           PERFORM 100-Main UNTIL EOF-REACHED.
           PERFORM 900-CLOSE-FILES.
           GOBACK.
       000-Housekeeping.
           INITIALIZE INVALS-WS.
           PERFORM 300-OPEN-FILES.
           WRITE REPORT-REC FROM HEADER-1.
           MOVE SPACES TO REPORT-REC.
           WRITE REPORT-REC AFTER ADVANCING 1 LINES.
      *    Priming Read
           PERFORM 400-Read-INVALS.
       100-Main.
           PERFORM 320-PROCESS-RECORDS
           PERFORM 350-REWRITE-RECORD
           PERFORM 400-Read-INVALS .
       300-OPEN-FILES.
           OPEN I-O INVALS.
           OPEN OUTPUT REPORTFILE.
       320-PROCESS-RECORDS.
           IF ADD-OPERATION PERFORM 500-ADD
           ELSE IF SUBTRACT-OPERATION PERFORM 600-SUBTRACT
           ELSE IF MULTIPLY-OPERATION PERFORM 700-MULTIPLY
           ELSE IF SQUARE-ROOT-OPERATION PERFORM 750-SQUARE-ROOT
           ELSE IF DIVIDE-OPERATION PERFORM 800-DIVIDE
           END-IF.
      *    DISPLAY INVALS-1.
      *    DISPLAY INVALS-2.
      *    DISPLAY INVALS-RESULT.
       350-REWRITE-RECORD.
      *    Although updating with calculated field is redundancy
             STRING    INVALS-1X
               DELIMITED BY SIZE INTO VAL1-RPT .
              STRING  INVALS-2X
                 DELIMITED BY SIZE INTO  VAL2-RPT.
            IF SQUARE-ROOT-OPERATION
                MOVE '0.5' TO  VAL2-RPT
            ELSE IF    SUBTRACT-OPERATION OR  DIVIDE-OPERATION
      *  just for Report formt Switching values due to  subtracting x from y
                 MOVE INVALS-2X  TO  VAL1-RPT
                 MOVE  INVALS-1X  TO  VAL2-RPT
              END-IF.
            MOVE    INVALS-RZLT   TO RESULT-RPT .
            MOVE    ' RESULT : '  TO TEXT1 .
            WRITE REPORT-REC
            WRITE REPORT-REC FROM INVALS-WS.
            REWRITE INVALS-REC FROM INVALS-WS.
       400-Read-INVALS.
           READ INVALS INTO INVALS-WS  AT END MOVE "Y" TO INVALS-EOF
           DISPLAY  INVALS-WS .
           MOVE ZEROS TO INVALS-RZLT  .
           DISPLAY  INVALS-WS .
           MOVE FUNCTION UPPER-CASE(OPERATION) TO OPERATION.
           INITIALIZE REPORT-REC.
           COMPUTE INVALS1-EDITED =  INVALS-1  .
       500-ADD.
           ADD  INVALS-1 , INVALS-2 GIVING INVALS-RZLT.
           MOVE   ' ADDED TO ' TO OPERATION-CODE  .
       600-SUBTRACT.
           SUBTRACT INVALS-2 FROM  INVALS-1 GIVING INVALS-RZLT  .
           MOVE   ' SUBTRACTED FROM ' TO OPERATION-CODE  .
       700-MULTIPLY.
           MULTIPLY  INVALS-1  BY INVALS-2 GIVING INVALS-RZLT  .
           MOVE   ' MULTIPLIED BY ' TO OPERATION-CODE  .
       750-SQUARE-ROOT.
           COMPUTE INVALS-RZLT   =    FUNCTION SQRT( INVALS-1 ) .
           MOVE   ' Exponenated to ' TO OPERATION-CODE  .
       800-DIVIDE.
           DIVIDE INVALS-2 BY INVALS-1    GIVING INVALS-RZLT  .
           MOVE   ' Devided by ' TO OPERATION-CODE  .
       900-CLOSE-FILES.
           CLOSE INVALS.
           CLOSE REPORTFILE.

