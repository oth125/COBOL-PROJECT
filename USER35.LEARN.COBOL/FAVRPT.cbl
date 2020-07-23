       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRPT.
      ***** This MODULE Workshop 5.3.1b - Create new COBOL program
      *****  THAT USES A FILE READ
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FAVIN1  ASSIGN TO FAVIN
                          FILE STATUS IS FAVIN-F-STATUS.
           SELECT OUT2    ASSIGN TO FAVRPT.
       DATA DIVISION.
       FILE SECTION.
       FD  FAVIN1
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FAVIN-REC.
       01 FAVIN-REC.
          05 ARTIST-NAME                PIC X(30).
          05 NUMBER-OF-MUSICIAN         PIC 9(02).
          05 MUSICAL-GENRE              PIC X(12).
          05 COST.
             10 CD-COST                 PIC 9(3)V99.
             10 SHIPPING-COST           PIC 9(2)V99.
             10 TAX                     PIC 9(2)V99.
          05 BAND-IS-STILL-TOGETHER     PIC X(1).
       FD  OUT2
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FAVOUT-REC.
       01 FAVOUT-REC.
          05 ARTIST-NAME-OUT            PIC X(30).
          05 NUMBER-OF-MUSICIAN-OUT     PIC 9(02).
          05 MUSICAL-GENRE-OUT          PIC X(12).
          05 COST-OUT.
             10 CD-COST-OUT             PIC 9(3)V99.
             10 SHIPPING-COST-OUT       PIC 9(2)V99.
             10 TAX-OUT                 PIC 9(2)V99.
          05 BAND-IS-STILL-TOGETHER-OUT PIC X(1).
          05 COST-IS                    PIC X(9)    VALUE ' Cost is:'.
          05 COMPUTED-COST-OUT          PIC 9(5).99.

       WORKING-STORAGE SECTION.
       01 HEADER-1.
          05 FILLER                     PIC X(30)   VALUE SPACES.
          05 FILLER                     PIC X(30)   VALUE
                                         'Musical Bands Report -FAVRPT'.
          05 FILLER                     PIC X(20)   VALUE SPACES.
       01 DEBUG-REC.
          05 FILLER                     PIC X(5)    VALUE 'DEBUG'
                                                              JUSTIFIED
                                                                  RIGHT.
          05 FILLER                     PIC X(13)   VALUE
                                                        ' file status:'.
          05 FAVIN-F-STATUS             PIC X(2).
          05 FILLER                     PIC X(4)    VALUE ' LR:'.
          05 LASTREC                    PIC X       VALUE SPACES.
             88 STELL-THERE-REC                     VALUE ' '.
             88 NO-MORE-RECORDS                     VALUE 'Y'.
          05   DBG-MESSAGE-ALL.
              10 DBG-MESSAGE.
                  15 DBG-MSG1                   PIC X(15).
                  15 DBG-MSG2                   PIC X(15).
              10 DBG-MSG3                   PIC X(10).
              10 DBG-MSG4                   PIC X(10).
              10 DBG-MSG5                   PIC X(5).
       01 COST-DEBUG.
              10 CD-COST-D                  PIC 9(3).99.
              10 FILLER                     PIC X(3)    VALUE ' + '.
              10 SHIPPING-COST-D            PIC 9(2).99.
              10 FILLER                     PIC X(3)    VALUE ' + '.
              10 TAX-D                      PIC 9(2).99.
       77 COMPUTED-COST                     PIC 9(5).99.
       PROCEDURE DIVISION.
           OPEN INPUT FAVIN1.
           OPEN OUTPUT OUT2.
           WRITE FAVOUT-REC FROM HEADER-1.
           MOVE SPACES TO FAVOUT-REC.
           WRITE FAVOUT-REC AFTER ADVANCING 1 LINES.
      *    Prime Read
           PERFORM READ-RECORD.
           PERFORM UNTIL LASTREC = 'Y' OR NO-MORE-RECORDS
                   PERFORM PROCESS-RECORDS
                   PERFORM WRITE-RECORD
                   PERFORM READ-RECORD
           END-PERFORM
           PERFORM CLOSE-FILES
           STOP RUN.
       READ-RECORD.
           READ FAVIN1
      *         AT END   MOVE 'Y' TO LASTREC
           AT END
              PERFORM END-OF-FILE
      *    NOT AT END         PERFORM PROCESS-RECORDS
           END-READ.
       PROCESS-RECORDS.
           COMPUTE COMPUTED-COST =(CD-COST + SHIPPING-COST + TAX).
           MOVE CD-COST TO CD-COST-D .
           MOVE SHIPPING-COST TO SHIPPING-COST-D .
           MOVE TAX TO TAX-D .
           MOVE SPACES TO DBG-MESSAGE-ALL.
      *    MOVE ' P-RCD ' TO DBG-MSG1 .
      *    WRITE FAVOUT-REC FROM DEBUG-REC.
      *    Just to print debug data
      *    MOVE COST-DEBUG TO DBG-MESSAGE .
      *    WRITE FAVOUT-REC FROM DEBUG-REC.
       WRITE-RECORD.
      *    MOVE SPACES TO DBG-MESSAGE-ALL.
      *    MOVE '  W-RCD ' TO DBG-MSG1 .
      *    WRITE FAVOUT-REC FROM DEBUG-REC.
           MOVE FAVIN-REC TO FAVOUT-REC.
           MOVE COMPUTED-COST TO COMPUTED-COST-OUT.
           MOVE ' Cost is:' TO   COST-IS    .
           WRITE FAVOUT-REC.
       CLOSE-FILES.
      *    MOVE SPACES TO DBG-MESSAGE-ALL.
      *    MOVE ' CLOS-FIL ' TO DBG-MSG1 .
      *    WRITE FAVOUT-REC FROM DEBUG-REC.
           CLOSE FAVIN1.
           CLOSE OUT2.
       END-OF-FILE.
           MOVE 'Y' TO LASTREC.
      *    MOVE SPACES TO DBG-MESSAGE-ALL.
      *    MOVE ' E-O-FILE ' TO DBG-MSG1 .
      *    WRITE FAVOUT-REC FROM DEBUG-REC.



