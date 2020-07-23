000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID. SMPLCALC.                                            00000200
000300 ENVIRONMENT DIVISION.                                            00000300
000400 DATA DIVISION.                                                   00000400
000600 WORKING-STORAGE SECTION.                                         00000500
000800 77  FIRST-NUMBER PIC 99 VALUE  9.                                00000600
000900 77  SECOND-NUMBER PIC 99 VALUE 12.                               00000700
001000 77  THE-RESULT PIC 99.                                           00000800
001100 77  THE-FUNCTION PIC X(1).
            88  ADDTION            VALUE 'A' .
            88  SUBTRACTION        VALUE 'S' .
            88  DIVISION-BY        VALUE 'D' .
            88  MULTIPLICATION     VALUE 'M' .
            88  EXPONENT           VALUE 'E' .
            88  SQURE-ROOT         VALUE 'R' .

001200 PROCEDURE DIVISION.                                              00001000
001400 PROGRAM-BEGIN.                                                   00001100
001600     DISPLAY "This program acts like a really simple calculator". 00001200
           MOVE 'A' TO THE-FUNCTION.                                    00001300
               PERFORM COMPUTE-AND-DISPLAY.                                 0000
           MOVE 'S' TO THE-FUNCTION.                                    00001500
               PERFORM COMPUTE-AND-DISPLAY.                                 0000
           MOVE 'D' TO THE-FUNCTION.                                    00001700
               PERFORM COMPUTE-AND-DISPLAY.                                 0000
           MOVE 'M' TO THE-FUNCTION.                                    00001900
              PERFORM COMPUTE-AND-DISPLAY.
           MOVE 'E' TO THE-FUNCTION.                                    00001900
               PERFORM COMPUTE-AND-DISPLAY.
           MOVE 'R' TO THE-FUNCTION.                                    00001900
               PERFORM COMPUTE-AND-DISPLAY.                                 0000
           GOBACK.                                                      00002100
003000 COMPUTE-AND-DISPLAY.                                             00002200
               IF ADDTION
003100            COMPUTE THE-RESULT = FIRST-NUMBER + SECOND-NUMBER     00002400
               ELSE IF  SUBTRACTION
003110            COMPUTE THE-RESULT = FIRST-NUMBER - SECOND-NUMBER     00002600
               ELSE IF  DIVISION-BY
003120            COMPUTE THE-RESULT = FIRST-NUMBER / SECOND-NUMBER     00002800
               ELSE IF MULTIPLICATION
003130            COMPUTE THE-RESULT = FIRST-NUMBER * SECOND-NUMBER
               ELSE IF SQURE-ROOT
003132            COMPUTE THE-RESULT = FIRST-NUMBER ** (0.5)
               ELSE IF EXPONENT
003133            COMPUTE THE-RESULT = FIRST-NUMBER ** SECOND-NUMBER.

003300     DISPLAY "The result for " THE-FUNCTION " IS " THE-RESULT.    00003100