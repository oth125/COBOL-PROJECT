000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.   STUDENT.                                           00000200
000200*                                                                 00000300
000300* ***************************************************             00000400
000400* *** student.cbl                                                 00000500
000500* ***                                                             00000600
001000* ***     The program produces a report that shows                00000700
001100* ***     the number of courses and credits that                  00000800
001200* ***     a student has taken.  The input file has                00000900
001300* ***     two types of records, a student record showing          00001000
001400* ***     information about a student, followed by                00001100
001500* ***     a number of course records for that student.            00001200
001600* ***     Each course record shows the information                00001300
001700* ***     for a course taken by the student.                      00001400
003000* ***                                                             00001500
003100* ***************************************************             00001600
      * **************************************************              00001700
003300 INSTALLATION.  IBM.                                              00001800
003400 DATE-WRITTEN.  01-01-2009.                                       00001900
003500 DATE-COMPILED. 01-01-2009.                                       00002000
003600 SECURITY.   NONE.                                                00002100
003700 ENVIRONMENT DIVISION.                                            00002200
003800 CONFIGURATION SECTION.                                           00002300
003900 SOURCE-COMPUTER.   IBM.                                          00002400
004000 OBJECT-COMPUTER.   IBM.                                          00002500
004100 INPUT-OUTPUT SECTION.                                            00002600
004200 FILE-CONTROL.                                                    00002700
004300     SELECT STUDENT-FILE   ASSIGN TO UT-S-STDNTFL                 00002800
004300            ORGANIZATION IS SEQUENTIAL.                           00002900
004400     SELECT CREDITS-REPORT ASSIGN TO PROPOSAL                     00003000
004300            ORGANIZATION IS SEQUENTIAL.                           00003100
004500 DATA DIVISION.                                                   00003200
004600 FILE SECTION.                                                    00003300
004700 FD  STUDENT-FILE                                                 00003400
           RECORDING MODE IS F                                          00003500
           LABEL RECORDS ARE STANDARD                                   00003600
           RECORD CONTAINS 80 CHARACTERS                                00003700
           BLOCK CONTAINS 0 RECORDS                                     00003800
           DATA RECORD IS STUDENT-IN-REC.                               00003900
      *    RECORD IS VARYING IN SIZE                                    00004000
      *    FROM 43 TO 80 CHARACTERS DEPENDING ON REC-LEN.               00004100
       01 STUDENT-IN-REC.                                               00004200
          05 REC-BODY           PIC X(69).                              00004300
          05 SR-RECORD-TYPE     PIC X.                                  00004400
          05 FILLER             PIC X(9).                               00004500
                                                                        00004600
006700 FD  CREDITS-REPORT                                               00004700
006800      RECORDING MODE IS F                                         00004800
            LABEL RECORDS ARE STANDARD.                                 00004900
006900 01 REPORT-LINE-OUT       PIC X(60).                              00005000
007000 WORKING-STORAGE SECTION.                                         00005100
       01 STUDENT-RCD-HDR.                                              00005200
          05 SR-NAME            PIC X(19).                              00005300
          05 FILLER             PIC X(5).                               00005400
          05 SR-ADDRESS         PIC X(20).                              00005500
          05 FILLER             PIC XXXXX.                              00005600
          05 SR-PHONE           PIC X(7).                               00005700
          05 FILLER             PIC XXX.                                00005800
          05 SR-BIRTH-DATE      PIC X(6).                               00005900
          05 FILLER             PIC XXXX.                               00006000
          05 STUDENT-REC-TYPE   PIC X.                                  00006100
          05 FILLER             PIC X(10).                              00006200
       01 COURSE-RCD-DTL.                                               00006300
          05 CR-NAME            PIC X(19).                              00006400
          05 FILLER             PIC X(5).                               00006500
          05 CR-COURSE-NUMBER   PIC X(5).                               00006600
          05 FILLER             PIC X(5).                               00006700
          05 CR-CREDITS         PIC 9.                                  00006800
          05 FILLER             PIC X(33).                              00006900
          05 COURSE-REC-TYPE1   PIC X.                                  00007000
          05 COURSE-REC-TYPE2   PIC X.                                  00007100
          05 COURSE-REC-TYPE3   PIC X.                                  00007200
007100 01 SWITCHES-IN-PROGRAM.                                          00007300
          05 SW-END-OF-DATA     PIC X     VALUE 'N'.                    00007400
             88 END-OF-DATA               VALUE 'Y'.                    00007500
       01 ACCUMS-AND-COUNTERS.                                          00007600
          05 ACCUM-CREDITS      PIC 999   VALUE 0.                      00007700
          05 CTR-COURSES        PIC 999   VALUE 0.                      00007800
          05 CTR-STUDENTS       PIC 9(5)  VALUE 0.                      00007900
          05 CTR-LINES          PIC 99    VALUE 0.                      00008000
       01 SAVE-AREAS.                                                   00008100
          05 SAVE-NAME          PIC X(19).                              00008200
       01 GRAND-TOTAL-LINE.                                             00008300
          05 FILLER             PIC X(30)                               00008400
                                          VALUE                         00008500
                ' TOTAL STUDENTS PROCESSED IS: '.                       00008600
          05 GTL-STUDENT-COUNT  PIC ZZZZZ.                              00008700
       01 DETAIL-LINE.                                                  00008800
          05 FILLER             PIC X(5)  VALUE SPACE.                  00008900
          05 DL-NAME            PIC X(19).                              00009000
          05 FILLER             PIC X(8)  VALUE SPACE.                  00009100
          05 DL-COURSES         PIC ZZZ.                                00009200
009000    05 FILLER             PIC X(10) VALUE SPACE.                  00009300
009100    05 DL-CREDITS         PIC ZZZZ.                               00009400
009200 01 HEADING-1.                                                    00009500
009300    05 FILLER             PIC X(10) VALUE SPACE.                  00009600
009400    05 FILLER             PIC X(80) VALUE                         00009700
009500          'S T U D E N T   C R E D I T S   R E P O R T'.          00009800
009600 01 HEADING-2.                                                    00009900
009700    05 FILLER             PIC X(5)  VALUE SPACE.                  00010000
009800    05 FILLER             PIC X(25) VALUE 'STUDENT NAME'.         00010100
009900    05 FILLER             PIC X(15) VALUE 'COURSES'.              00010200
010000    05 FILLER             PIC X(7)  VALUE 'CREDITS'.              00010300
       77 REC-LEN               PIC 99.                                 00010400
      *                                                                 00010500
010100 PROCEDURE DIVISION.                                              00010600
010200 000-TOP-LEVEL.                                                   00010700
010300     PERFORM 100-INITIALIZATION.                                  00010800
010400     PERFORM 200-PROCESS-RECORDS UNTIL END-OF-DATA.               00010900
010500     PERFORM 800-WRAP-UP.                                         00011000
010600     GOBACK.                                                      00011100
010700 100-INITIALIZATION.                                              00011200
010800     OPEN INPUT STUDENT-FILE.                                     00011300
010900     OPEN OUTPUT CREDITS-REPORT.                                  00011400
011000     PERFORM 400-PAGE-CHANGE-RTN.                                 00011500
011100     PERFORM 700-READ-A-RECORD.                                   00011600
011400 200-PROCESS-RECORDS.                                             00011900
011500     IF SR-RECORD-TYPE = '1'  THEN
              MOVE STUDENT-IN-REC TO STUDENT-RCD-HDR                    00012100
              DISPLAY STUDENT-RCD-HDR
              IF  CTR-STUDENTS = 0 THEN
                 MOVE SR-NAME  TO SAVE-NAME
              END-IF
              IF SR-NAME  NOT = SAVE-NAME  THEN
      *   CHECK IF NEW STUDENT (new HEADER RECORD FOUND)                00012400
                    PERFORM 900-WRAP-UP-RECORD
              END-IF                                                    00013000
011700        PERFORM 300-PROCESS-1ST-REC-HDR                           00013100
011800        MOVE SR-NAME TO SAVE-NAME                                 00013200
012000     ELSE                                                         00013300
              MOVE STUDENT-IN-REC TO COURSE-RCD-DTL                     00013400
011700        PERFORM 600-PROCESS-2ND-REC-DTL                           00013500
           END-IF                                                       00013600
012200     PERFORM 700-READ-A-RECORD.                                   00013700
           IF CTR-LINES IS GREATER THAN 30                              00013800
012600        PERFORM 400-PAGE-CHANGE-RTN                               00013900
           END-IF.                                                      00014000
012300 300-PROCESS-1ST-REC-HDR.                                         00014100
           ADD 1 TO CTR-STUDENTS.                                       00014200
013200 400-PAGE-CHANGE-RTN.                                             00014300
013300         WRITE REPORT-LINE-OUT FROM HEADING-1                     00014400
013400            AFTER ADVANCING PAGE.                                 00014500
013500         WRITE REPORT-LINE-OUT FROM HEADING-2                     00014600
013600            AFTER ADVANCING 2.                                    00014700
013700         MOVE ZERO TO CTR-LINES.                                  00014800
013800 500-BUILD-DETAIL-LINE.                                           00014900
013900         MOVE SAVE-NAME TO DL-NAME.                               00015000
014000         MOVE CTR-COURSES TO DL-COURSES.                          00015100
014100         MOVE ACCUM-CREDITS TO DL-CREDITS.                        00015200
014200 600-PROCESS-2ND-REC-DTL.                                         00015300
014300         ADD CR-CREDITS TO ACCUM-CREDITS.                         00015400
014400         ADD 1 TO CTR-COURSES.                                    00015500
014500 700-READ-A-RECORD.                                               00015600
014600         READ STUDENT-FILE                                        00015700
014700         AT END                                                   00015800
                  MOVE 'Y' TO SW-END-OF-DATA                            00015900
               END-READ.                                                00016000
014800 800-WRAP-UP.
           PERFORM 900-WRAP-UP-RECORD.
014900     MOVE CTR-STUDENTS TO GTL-STUDENT-COUNT.                      00016200
015000     WRITE REPORT-LINE-OUT FROM GRAND-TOTAL-LINE                  00016300
015100        AFTER ADVANCING 2.                                        00016400
015200     CLOSE CREDITS-REPORT STUDENT-FILE.
       900-WRAP-UP-RECORD.
           PERFORM 500-BUILD-DETAIL-LINE.
012800     WRITE REPORT-LINE-OUT FROM DETAIL-LINE
012900     AFTER ADVANCING 1 .
           MOVE ZEROS  TO CTR-COURSES .
           MOVE ZEROS  TO ACCUM-CREDITS.
