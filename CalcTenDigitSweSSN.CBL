       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECURSIVE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 PERSNUMB.
           03 NUMB PIC 9(1) VALUE 0 OCCURS 9.
        01 CALCSUM PIC S9(3).
        01 KEEPRUN PIC X VALUE 'Y'.
        01 TEMP.
          03 TEM PIC 9(2) OCCURS 2 VALUE ZERO.
        01 I PIC 9(2) VALUE 1.
        01 PERSONNUMMER PIC 9(10) VALUE 0.
        01 FAILCOUNT PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MENU SECTION.
             INITIALIZE PERSNUMB, CALCSUM, PERSONNUMMER
             MOVE 1 TO I
             DISPLAY 'PLEASE ENTER FIRST 9 DIGITS OF SWEDISH SSN'
             PERFORM ACCEPTING
       .

       ACCEPTING SECTION.
          ACCEPT PERSNUMB
          IF PERSNUMB NUMERIC
             PERFORM CALCULATION
          ELSE
             ADD 1 TO FAILCOUNT
             IF FAILCOUNT > 4
                DISPLAY 'I GIVE UP'
                PERFORM 001-EXIT
             ELSE
                DISPLAY 'PLEASE ENTER A 9 DIGIT NUMBER ONLY.'
                PERFORM MENU
          .

       CALCULATION SECTION.
          PERFORM UNTIL I > 9
             ADD NUMB(I) TO PERSONNUMMER
             COMPUTE PERSONNUMMER = PERSONNUMMER * 10
             INITIALIZE TEMP
             COMPUTE TEM(2) = FUNCTION MOD (I, 2)
             IF TEM(2) > 0
                COMPUTE TEM(1) = NUMB(I) * 2
                IF TEM(1) > 9
                   SUBTRACT 9 FROM TEM(1)
                END-IF
                ADD TEM(1) TO CALCSUM
                ELSE ADD NUMB(I) TO CALCSUM
             END-IF
             ADD 1 TO I
          END-PERFORM
          PERFORM UNTIL CALCSUM <1
             SUBTRACT 10 FROM CALCSUM
          END-PERFORM
          SUBTRACT CALCSUM FROM PERSONNUMMER
          DISPLAY 'FULL 10 DIGIT SSN: ' PERSONNUMMER
          DISPLAY 'CALCULATE ANOTHER SWEDISH SSN? N/OTHER'
          ACCEPT KEEPRUN
          IF KEEPRUN = 'N'
             PERFORM 001-EXIT
          ELSE
             MOVE 0 TO FAILCOUNT
             PERFORM MENU
          GOBACK
          .

        GOBACK
        .
        001-EXIT.
           DISPLAY 'EXITING.'
           STOP RUN.

           END PROGRAM RECURSIVE.
