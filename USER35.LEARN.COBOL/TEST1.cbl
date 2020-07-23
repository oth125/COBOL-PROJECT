       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST1.
      * Comment: This program Displays a number of text strings
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  EMP-RECORD.
           05  EMP-Name.
               10 EMP-fname pic x(15).
               10 EMP-Lname pic x(15).
           05 emp-hourly-rate pic 9(3)v99.
           05 emp-ot-rate   pic v99.
           05 emp-hours     pic 9(3).
           05 emp-pay       pic 9(7)v99.
       PROCEDURE DIVISION.
           move 10 to EMP-Name .
           MOVE 10 TO EMP-NAME.
           MOVE 'SMITHFIELD HOMES' TO EMP-Name.
           MOVE 'SMITHFIELD HOMES' TO EMP-FNAME.
           MOVE EMP-FNAME TO EMP-OT-RATE.
           MOVE EMP-OT-RATE TO EMP-PAY.
           MOVE '10' TO EMP-HOURS.
           GOBACK.