       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID. TESTCODE.                                            00000200
     ***** This is an unbelievably simple COBOL program                 00000300
       DATA DIVISION.                                                   00000400
       WORKING-STORAGE SECTION.                                         00000500
       01  EMP-RECORD.                                                  00000600
           05  EMP-NAME.                                                00000700
                10 EMP-FNAME        PIC X(15).                          00000800
                10 EMP-LNAME        PIC X(15).                          00000900
           05  EMP-HOURLY-RATE      PIC 9(3)V99.                        00001000
           05  EMP-OT-RATE          PIC V99.                            00001100
           05  EMP-HOURS            PIC 9(3).                           00001200
           05  EMP-PAY              PIC 9(7)V99.                        00001300
      *  group item "CUSTOMER-REC" contained the "PICTURE" clause.      00001400
       01  CUSTOMER-REC.                                                00001500
           05 COMPANY-NAME          PIC X(40).                          00001600
      * *   adding Underscore ( spaces is not accepted in var name )    00001700
           05 EMPLOYEE_AGE PIC 9(03).                                   00001800
           05 9RETIRED-INDICATOR PIC X(01).                             00001900
      * // ADDRESS  is  not allowed as elementary item/group item       00002000
           05 ADDRESS1.                                                 00002100
               10 STREET-ADDRESS PIC X(30).                             00002200
               10 CITY PIC X(30).                                       00002300
               10 STATE PIC X(02).                                      00002400
      *    // Move is not allowed as var name                           00002500
           10 MOVE1 PIC 9(11).                                          00002600
           10 ZIP_CODE PIC X(05).                                       00002700
       PROCEDURE DIVISION.                                              00002800
           MOVE "Millard"           TO EMP-FNAME.                       00002900
           MOVE "Fillmore"          TO EMP-LNAME.                       00003000
           MOVE 19                  TO EMP-HOURS.                       00003100
           MOVE 23.50               TO EMP-HOURLY-RATE.                 00003200
           IF  EMP-HOURS > 18                                           00003300
               MOVE .25 TO  EMP-OT-RATE                                 00003400
           ELSE                                                         00003500
               MOVE ZERO TO EMP-OT-RATE.                                00003600
           COMPUTE EMP-PAY =                                            00003700
                (EMP-HOURS * EMP-HOURLY-RATE) * (1 + EMP-OT-RATE).      00003800
           DISPLAY "Name: " EMP-NAME.                                   00003900
           DISPLAY "Hours Worked: " EMP-HOURS.                          00004000
           DISPLAY "Hourly Rate: " EMP-HOURLY-RATE.                     00004100
           DISPLAY "Bonus-Rate: " EMP-OT-RATE.                          00004200
           DISPLAY "Gross Pay: " EMP-PAY.                               00004300
           GOBACK.                                                      00004400
                                                                        00004500