      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBL0001
       AUTHOR.        HOW DOES THIS PROGRAM WORK?
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTREC.
      * //ACCTREC  DD DSN=DDS0001.LEARN.ACCT.DATA,DISP=SHR in COBUCLD
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC.
           05  ACCT-NO-O      PIC X(8).
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
           05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.
           05  LAST-NAME-O    PIC X(20).
           05  FIRST-NAME-O   PIC X(15).
           05  COMMENTS-O     PIC X(50).
       01  PRINT-HEADING-REC.
           05  ACCT-NO-O-H      PIC X(8).
           05  ACCT-LIMIT-O-H   PIC X(13).
           05  ACCT-BALANCE-O-H PIC X(13).
           05  LAST-NAME-O-H    PIC X(20).
           05  FIRST-NAME-O-H   PIC X(15).
           05  COMMENTS-O-H     PIC X(50).
      *
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-NO            PIC X(8).
           05  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
           05  ACCT-BALANCE       PIC S9(7)V99 COMP-3.
           05  LAST-NAME          PIC X(20).
           05  FIRST-NAME         PIC X(15).
           05  CLIENT-ADDR.
               10  STREET-ADDR    PIC X(25).
               10  CITY-COUNTY    PIC X(20).
               10  USA-STATE      PIC X(15).
           05  RESERVED           PIC X(7).
           05  COMMENTS           PIC X(50).
      *
       WORKING-STORAGE SECTION.
       01 FLAGS.
         05 LASTREC           PIC X VALUE SPACE.
         05 HEADING-PRINT     PIC X VALUE SPACE.
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
      *
       READ-NEXT-RECORD.
            PERFORM READ-RECORD
            PERFORM UNTIL LASTREC = 'Y'
                PERFORM WRITE-RECORD
                PERFORM READ-RECORD
            END-PERFORM
           .
      *
       CLOSE-STOP.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.
           STOP RUN.
      *
       READ-RECORD.
           READ ACCT-REC
           AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       WRITE-RECORD.
           IF  HEADING-PRINT = ' '  PERFORM HEADING-PRINTING.
           MOVE ACCT-NO      TO  ACCT-NO-O.
           MOVE ACCT-LIMIT   TO  ACCT-LIMIT-O.
           MOVE ACCT-BALANCE TO  ACCT-BALANCE-O.
           MOVE LAST-NAME    TO  LAST-NAME-O.
           MOVE FIRST-NAME   TO  FIRST-NAME-O.
           MOVE COMMENTS     TO  COMMENTS-O.
           WRITE PRINT-REC.
       HEADING-PRINTING.
           MOVE 'ACCT-NO'      TO  ACCT-NO-O-H.
           MOVE 'ACCT-LIMIT '  TO  ACCT-LIMIT-O-H.
           MOVE 'ACCT-BALANCE' TO  ACCT-BALANCE-O-H.
           MOVE 'LAST-NAME'    TO  LAST-NAME-O-H.
           MOVE 'FIRST-NAME'   TO  FIRST-NAME-O-H.
           MOVE 'COMMENTS'     TO  COMMENTS-O-H.
           MOVE    'Y' TO HEADING-PRINT .
           WRITE PRINT-HEADING-REC.
      *