       IDENTIFICATION DIVISION.
       PROGRAM-ID. USERPASS.
      ***** This MODULE Workshop 8 - Create new COBOL program
      *****  THAT CHECK FOR USER CREDENTIALS (NAME/PASSWORD)
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PASS-REC  ASSIGN TO PASSFILE
                     FILE STATUS IS PASSFILE-F-STATUS.
           SELECT OUT2    ASSIGN TO PRTLINE.
       DATA DIVISION.
       FILE SECTION.
       FD  PASS-REC
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PASSFILE-REC.
       01 PASSFILE-REC.
          05 PFILE-USER                PIC X(10) .
          05 PFILE-PASS                PIC X(10).
       FD  OUT2
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS OUTFILE-REC.
       01 OUTFILE-REC.
          05 FILLER            PIC X(80).
       WORKING-STORAGE SECTION.
       01 HEADER-1.
          05 FILLER                     PIC X(30)   VALUE SPACES.
          05 FILLER PIC X(30) VALUE ' PROCESSING CREDENTIALS '.
          05 FILLER                     PIC X(20)   VALUE SPACES.
       01 TRAILLER-2.
          05 FILLER PIC X(20)       VALUE 'ENTERED UserName:'.
          05 USER-NAME      PIC X(10)  VALUE  SPACES  .
          05 FILLER PIC X(20)       VALUE 'ENTERED Password:'.
          05 PASS-WORD     PIC X(10)  VALUE  SPACES  .
          05 MESSAGE-RZLT        PIC X(30)   .
       01 TRAILLER-3.
          05 FILLER PIC X(30) VALUE 'FROM PASSWORDS FILE UserName:'.
          05 USER-NAME-OUT      PIC X(10)  VALUE  SPACES  .
          05 FILLER PIC X(15)       VALUE 'READ Password:'.
          05 PASS-WORD-OUT     PIC X(10)  VALUE  SPACES  .

       77 LASTREC     PIC X.
             88 NO-MORE-RECORDS VALUE 'Y'.
       77 PASSFILE-F-STATUS PIC XX .
       77 TRIAL-KTR    PIC 9(1) VALUE 0 .
       PROCEDURE DIVISION.
           OPEN OUTPUT OUT2.
           WRITE OUTFILE-REC FROM HEADER-1.
           MOVE SPACES TO OUTFILE-REC.
           WRITE OUTFILE-REC AFTER ADVANCING 1 LINES.
           PERFORM UNTIL TRIAL-KTR = 3 OR USER-NAME IS NUMERIC
               OPEN INPUT PASS-REC
      *        Prime Read
               PERFORM ACCEPT-User-Name
               PERFORM READ-RECORD
               PERFORM UNTIL LASTREC = 'Y' OR NO-MORE-RECORDS
                       PERFORM Check-User-Name
                       PERFORM WRITE-CHK-RZLT-RECORD
                       PERFORM READ-RECORD
               END-PERFORM
               CLOSE PASS-REC
               MOVE SPACES TO LASTREC
               COMPUTE TRIAL-KTR = TRIAL-KTR + 1
            END-PERFORM
           CLOSE OUT2.
           STOP RUN.
       ACCEPT-User-Name.
           MOVE SPACES TO USER-NAME, PASS-WORD  , MESSAGE-RZLT.
           DISPLAY 'ENTER USER NAME OR NUMBER TO EXIT'
           ACCEPT USER-NAME .
           DISPLAY 'ENTER THE PASSWORD  '
           ACCEPT PASS-WORD .
           MOVE  ' JUST ENTERED ' TO MESSAGE-RZLT
           DISPLAY TRAILLER-2.
           WRITE OUTFILE-REC  FROM TRAILLER-2 .
       READ-RECORD.
           READ PASS-REC
      *         AT END   MOVE 'Y' TO LASTREC
           AT END
              PERFORM END-OF-FILE
      *    NOT AT END         PERFORM PROCESS-RECORDS
           END-READ.

       Check-User-Name.
            MOVE PFILE-USER TO  USER-NAME-OUT
            MOVE PFILE-PASS TO  PASS-WORD-OUT
            WRITE OUTFILE-REC  FROM TRAILLER-3 .
            EVALUATE TRUE ALSO TRUE
              WHEN USER-NAME NOT = PFILE-USER  ALSO TRUE
                 MOVE  'Wrong ID'  TO MESSAGE-RZLT
              WHEN USER-NAME = PFILE-USER ALSO PASS-WORD NOT= PFILE-PASS
                 MOVE  'Bad Password'  TO MESSAGE-RZLT
              WHEN USER-NAME = PFILE-USER  ALSO PASS-WORD = PFILE-PASS
                 MOVE  'Welcome to COBOL'  TO MESSAGE-RZLT
      *          SET EXIT CONTER
                 MOVE 'Y' TO LASTREC
                 COMPUTE TRIAL-KTR = 2
              WHEN OTHER
                 MOVE  'Other condition'  TO MESSAGE-RZLT
            END-EVALUATE .

       WRITE-CHK-RZLT-RECORD.
            DISPLAY MESSAGE-RZLT
            WRITE OUTFILE-REC  FROM TRAILLER-2 .
       END-OF-FILE.
           MOVE 'Y' TO LASTREC.





