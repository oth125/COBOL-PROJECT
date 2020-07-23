       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID. PAYROL02.                                            00000200
      * This                                                            00000300
       ENVIRONMENT DIVISION.                                            00000400
       INPUT-OUTPUT SECTION.                                            00000500
       FILE-CONTROL.                                                    00000600
      *    SELECT PAYROLL                                               00000700
      *    ASSIGN TO EMPTYFIL                                           00000800
      *      ORGANIZATION IS SEQUENTIAL.                                00000900
      *                                                                 00001000
      *    SELECT PAYCHECK                                              00001100
      *    ASSIGN TO UT-S-PAYCHECK                                      00001200
      *        ACCESS MODE IS SEQUENTIAL.                               00001300
           SELECT PAYROLL  ASSIGN TO EMPTYFIL                           00001400
           ORGANIZATION IS SEQUENTIAL.                                  00001500
           SELECT PAYCHECK ASSIGN TO PAYCHECK                           00001600
           ACCESS MODE IS SEQUENTIAL.                                   00001700
       DATA DIVISION.                                                   00001800
       FILE SECTION.                                                    00001900
       FD  PAYROLL                                                      00002000
           RECORDING MODE IS F                                          00002100
           LABEL RECORDS ARE STANDARD                                   00002200
           RECORD CONTAINS 80 CHARACTERS                                00002300
           BLOCK CONTAINS 0 RECORDS                                     00002400
           DATA RECORD IS PAYROLL-REC.                                  00002500
       01  PAYROLL-REC  PIC X(80).                                      00002600
       FD  PAYCHECK                                                     00002700
           RECORDING MODE IS F                                          00002800
           LABEL RECORDS ARE STANDARD                                   00002900
           RECORD CONTAINS 80 CHARACTERS                                00003000
           BLOCK CONTAINS 0 RECORDS                                     00003100
           DATA RECORD IS PAYCHECK-REC.                                 00003200
       01  PAYCHECK-REC  PIC X(80).                                     00003300
                                                                        00003400
       WORKING-STORAGE SECTION.                                         00003500
      * End of File switch                                              00003600
       01 PAYROLL-EOF               PIC X(01) VALUE SPACE.              00003700
       01 PAYROLL-IN.                                                   00003800
           05 NAME-IN.                                                  00003900
              10 FIRST-IN              PIC X(10).                       00004000
              10 LAST-IN               PIC X(10).                       00004100
           05  DATE-IN                 PIC X(10).                       00004200
           05  HOURLY-RATE-IN          PIC 99V99.                       00004300
           05  HOURS-WORKED-IN         PIC 9(2).                        00004400
           05  CATEGORY-IN             PIC X(1).                        00004500
           05  GROSS-PAY-IN            PIC 999V99.                      00004600
                                                                        00004700
       01 PAYROLL-OUT.                                                  00004800
           05 NAME-WS-OUT.                                              00004900
              10 FIRST-OUT             PIC X(10).                       00005000
              10 FILLER                PIC XX VALUE ' '.                00005100
              10 LAST-OUT              PIC X(10).                       00005200
           05  FILLER                  PIC X(02).                       00005300
           05  DATE-OUT                PIC X(10).                       00005400
           05  FILLER                  PIC X(02).                       00005500
           05  HOURLY-RATE-OUT         PIC $$.99.                       00005600
           05  FILLER                  PIC X(02).                       00005700
           05  HOURS-WORKED-OUT        PIC ZZ.                          00005800
           05  FILLER                  PIC X(02).                       00005900
           05  CATEGORY-OUT            PIC X(1).                        00006000
           05  FILLER                  PIC X(02).                       00006100
           05  GROSS-PAY-OUT           PIC $$$.99.                      00006200
                                                                        00006300
       PROCEDURE DIVISION.                                              00006400
       MAIN.                                                            00006500
           PERFORM 000-Housekeeping.                                    00006600
           PERFORM 100-Main UNTIL PAYROLL-EOF = 'Y'.                    00006700
           PERFORM 600-CLOSE-FILES.                                     00006800
           GOBACK.                                                      00006900
       000-Housekeeping.                                                00007000
      * Initialization Routine                                          00007100
           INITIALIZE PAYROLL-IN, PAYROLL-OUT.                          00007200
      * Priming Read                                                    00007300
      *     PERFORM 300-Open-Files.    *> Comment out to get ABEND 4038 00007400
           PERFORM 300-Open-Files.    *> Comment out to get ABEND 4038  00007500
           PERFORM 400-Read-Payroll.  *> Comment out with empty input fi00007600
       100-Main.                                                        00007700
           DISPLAY '100-main'.        *> For shops not using the Debugge00007800
           DISPLAY "PAYROLL REC: " PAYROLL-IN.                          00007900
           PERFORM 200-PROCESS-DATA.                                    00008000
           PERFORM 500-Write-Paycheck.                                  00008100
           PERFORM 400-Read-Payroll.                                    00008200
       200-PROCESS-DATA.                                                00008300
           MOVE FIRST-IN          TO  FIRST-OUT.                        00008400
           MOVE LAST-IN           TO  LAST-OUT.                         00008500
           MOVE DATE-IN           TO  DATE-OUT.                         00008600
           MOVE HOURLY-RATE-IN    TO  HOURLY-RATE-OUT.                  00008700
           MOVE HOURS-WORKED-IN   TO  HOURS-WORKED-OUT.                 00008800
           MOVE CATEGORY-IN       TO  CATEGORY-OUT.                     00008900
           MOVE GROSS-PAY-IN      TO  GROSS-PAY-OUT.                    00009000
           COMPUTE GROSS-PAY-OUT = HOURLY-RATE-IN * HOURS-WORKED-IN.    00009100
       300-Open-Files.                                                  00009200
           OPEN INPUT PAYROLL.                                          00009300
           OPEN OUTPUT PAYCHECK.                                        00009400
       400-Read-Payroll.                                                00009500
           DISPLAY 'READ Payroll'.                                      00009600
           READ PAYROLL INTO PAYROLL-IN                                 00009700
      * Set AT END Switch                                               00009800
               AT END MOVE "Y" TO PAYROLL-EOF                           00009900
           END-READ.                                                    00010000
       500-Write-Paycheck.                                              00010100
           DISPLAY 'WRITE Payroll'.                                     00010200
           WRITE PAYCHECK-REC FROM PAYROLL-OUT.                         00010300
       600-CLOSE-FILES.                                                 00010400
           CLOSE PAYROLL, PAYCHECK.                                     00010500