000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.   TABLES07.
003200*
000300* ************************************************************
      * THIS IS A WORKING EXAMPLE PROGRAM FOR THE EXERCISE IN
      *  MODULE 17 - OF A TWO-DIM TABLE LOAD & SEARCH OPERATION
      *    CURRENTLY INCOMPLETE: 7/4/2020
      * ************************************************************
003300 INSTALLATION.  IBM.
003400 DATE-WRITTEN.  01-01-2009.
003500 DATE-COMPILED. 01-01-2009.
003600 SECURITY.   NONE.
003700 ENVIRONMENT DIVISION.
003800 CONFIGURATION SECTION.
003900 SOURCE-COMPUTER.   IBM.
004000 OBJECT-COMPUTER.   IBM.
004100 INPUT-OUTPUT SECTION.
004200 FILE-CONTROL.
004300     SELECT STUDENT-FILE   ASSIGN TO UT-S-STDNTCRS
004300            ORGANIZATION IS SEQUENTIAL.
004400     SELECT CREDITS-REPORT ASSIGN TO UT-S-STCRSRPT
004300            ORGANIZATION IS SEQUENTIAL.
004500 DATA DIVISION.
004600 FILE SECTION.
004700 FD  STUDENT-FILE
           RECORDING MODE IS F
004800     LABEL RECORDS ARE STANDARD.
004900 01  STUDENT-RECORD.
005000     05  STUDENT-NAME            PIC X(20).
           05  STUDENT-COURSES.
               10 STUDENT-COURSE-TAB OCCURS 5 TIMES.
                   15  COURSE-NBR      PIC X(7).
                   15  COURSE-GRADE    PIC X(1).
           05  FILLER                  PIC X(20).
006700 FD  CREDITS-REPORT
           RECORDING MODE IS F
006800     LABEL RECORDS ARE STANDARD.
006900 01  REPORT-LINE-OUT             PIC X(80).
007000 WORKING-STORAGE SECTION.
007100 01  SWITCHES-IN-PROGRAM.
007200     05  SW-END-OF-DATA            PIC X VALUE 'N'.
007300         88  END-OF-DATA               VALUE 'Y'.
007200     05  SW-MUS-FOUND              PIC X VALUE 'N'.
007300         88  MUS-FOUND                 VALUE 'Y'.
007200     05  SW-STUDENT-FOUND          PIC X VALUE 'N'.
007300         88  STUDENT-FOUND             VALUE 'Y'.
       01  OUT-FILE.
           05 STUDENT-OUT                  PIC X(80).
           05 COURSE-OUT                   PIC X(80).

       01  SUBSCRIPTS-AND-COUNTERS.
           05  CTR-STUDENTS                 PIC 99 VALUE 0.
           05  STUDENT-SUB                  PIC 99 VALUE 0 COMP.
           05  GRADE-ACCUM                  PIC 99 VALUE 0 COMP.
           05  COURSES-SUB                  PIC 99 VALUE 0 COMP.
004900 01  WS-STUDENT-RECORD.
           02  WS-STUDENT-TABLE OCCURS 5 TIMES.
005000       05  WS-STUDENT-NAME            PIC X(20).
             05  WS-STUDENT-COURSES.
               10 WS-STUDENT-COURSE-TAB OCCURS 5 TIMES.
                   15  WS-COURSE-NBR      PIC X(7).
                   15  WS-COURSE-GRADE    PIC X(1).
       01  TWO-DIM-TABLE-VALUES.
           05 ROW1  PIC X(48) VALUE
           'TUBA101BCALC687BSOCS200AALGB124APHYS002BFLUT140C'.
           05 ROW2  PIC X(48) VALUE
           'BIOL201ATRIG551BSHAK213CPSYC234ABIOL002CDRUM310B'.
           05 ROW3  PIC X(48) VALUE
           'POLY555CGEOM231BRLIT560ABIOL136AMECH002BACCO140D'.
           05 ROW4  PIC X(48) VALUE
           'TUBA567ASTAT043CSHOP980BCHEM534BSTT0002AVIOL610A'.
           05 ROW5  PIC X(48) VALUE
           'MEDC522DPIAN003ASPAN760AEBRT164ARUSS002APIAN170A'.
       01  WS-STUDENT-RECORD-RDF REDEFINES TWO-DIM-TABLE-VALUES.
           02  WS-STUDENT-TABLE-RDF OCCURS 5 INDEXED BY ST-IDX.
             05  WS-STUDENT-COURSES-RDF.
               10 WS-STUDENT-COURSE-TAB-RDF OCCURS 6 TIMES
                  INDEXED BY CRS-IDX.
                   15  WS-COURSE-NBR-RDF      PIC X(7).
                   15  WS-COURSE-GRADE-RDF    PIC X(1).
010100
       PROCEDURE DIVISION.
010200 000-TOP-LEVEL.
010300     PERFORM 100-INITIALIZATION.
010400     PERFORM 200-PROCESS-RECORDS VARYING STUDENT-SUB
                FROM 1 BY 1 UNTIL END-OF-DATA OR STUDENT-SUB > 5.
           MOVE 1 TO STUDENT-SUB.
           MOVE FUNCTION MAX(WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB))
                TO GRADE-ACCUM.
           PERFORM 300-TABLE-SEARCH.
010500     PERFORM 900-WRAP-UP.
010600     GOBACK.
010700 100-INITIALIZATION.
010800     OPEN INPUT  STUDENT-FILE.
010900     OPEN OUTPUT CREDITS-REPORT.
011100     PERFORM 230-READ-A-RECORD.
011300     ADD 1 TO CTR-STUDENTS.
011400 200-PROCESS-RECORDS.
           MOVE STUDENT-RECORD TO WS-STUDENT-TABLE(STUDENT-SUB).
           PERFORM VARYING COURSES-SUB FROM 1 BY 1
                UNTIL COURSES-SUB > 5
              EVALUATE WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                  WHEN 'A' MOVE '4' TO
                    WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                  WHEN 'B' MOVE '3' TO
                    WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                  WHEN 'C' MOVE '2' TO
                    WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                  WHEN 'D' MOVE '1' TO
                    WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                  WHEN 'F' MOVE '0' TO
                    WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
              END-EVALUATE
           END-PERFORM.
           WRITE REPORT-LINE-OUT.
012200     PERFORM 230-READ-A-RECORD.
014500 230-READ-A-RECORD.
014600     READ STUDENT-FILE
014700         AT END MOVE 'Y' TO SW-END-OF-DATA.
014800 300-TABLE-SEARCH.
           MOVE 3 TO STUDENT-SUB.
           MOVE WS-STUDENT-TABLE(STUDENT-SUB) TO  STUDENT-OUT.
           MOVE 2 TO COURSES-SUB.
           MOVE WS-STUDENT-NAME(STUDENT-SUB) TO STUDENT-OUT.
           MOVE  WS-COURSE-GRADE (STUDENT-SUB, COURSES-SUB)
                        TO COURSE-OUT.
           PERFORM VARYING STUDENT-SUB FROM 1 BY 1 UNTIL
              STUDENT-SUB > 5 OR STUDENT-FOUND
              PERFORM VARYING COURSES-SUB FROM 1 BY 1
                UNTIL STUDENT-SUB > 5 OR STUDENT-FOUND
                 IF WS-COURSE-NBR (STUDENT-SUB, COURSES-SUB) = 'ANTH101'
                  AND WS-COURSE-GRADE (STUDENT-SUB, COURSES-SUB) = '3'
                        MOVE WS-STUDENT-NAME(STUDENT-SUB) TO STUDENT-OUT
                        MOVE 'Y' TO SW-STUDENT-FOUND
                 END-IF
              END-PERFORM
           END-PERFORM.
           PERFORM VARYING ST-IDX FROM 1 BY 1
               UNTIL ST-IDX > 5 OR MUS-FOUND
           SET CRS-IDX TO 1
      *  Find the first TUBA student with an "A" in TUBA567
           SEARCH WS-STUDENT-COURSE-TAB-RDF
           WHEN (WS-COURSE-NBR-RDF (ST-IDX, CRS-IDX)  = 'TUBA567'
                AND WS-COURSE-GRADE-RDF (ST-IDX, CRS-IDX) = 'A')
                OR ( WS-COURSE-NBR-RDF (ST-IDX, CRS-IDX)= 'PIANO003'
                AND WS-COURSE-GRADE-RDF (ST-IDX, CRS-IDX) = 'A')
                DISPLAY '*** Musician Found ***'
                MOVE 'Y' TO SW-MUS-FOUND
           END-SEARCH
           END-PERFORM.

       900-WRAP-UP.
           MOVE 'NBR OF STUDENTS:' TO  REPORT-LINE-OUT(12:16).
           MOVE CTR-STUDENTS to  REPORT-LINE-OUT(30:2).
015200     CLOSE CREDITS-REPORT  STUDENT-FILE.