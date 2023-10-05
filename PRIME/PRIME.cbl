       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIME.       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N PIC 9(5) VALUE ZERO.
       01 I PIC 9(5) VALUE ZERO.
       01 FLAG PIC X VALUE 'Y'.
       PROCEDURE DIVISION.
           DISPLAY "Enter a number: "
           ACCEPT N
           IF N < 2 THEN
               MOVE 'N' TO FLAG
           ELSE
               PERFORM VARYING I FROM 2 BY 1 UNTIL I * I > N 
               OR FLAG = 'N'
                   IF FUNCTION MOD(N,I) = ZERO THEN
                       MOVE 'N' TO FLAG
                   END-IF
               END-PERFORM
           END-IF
           IF FLAG = 'Y' THEN
               DISPLAY N " is a prime number."
           ELSE
               DISPLAY N " is not a prime number."
           END-IF
           STOP RUN.
       