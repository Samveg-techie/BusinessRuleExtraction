       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PIN-NUMBER PIC 9(4) VALUE 1234.
       01 BALANCE PIC 9(5)V99 VALUE 1000.00.
       01 AMOUNT PIC 9(5)V99.
       01 CHOICE PIC 9.
       01 VALID PIC X VALUE 'N'.
       01 DENOMINATIONS OCCURS 5 TIMES PIC 9(3).
       01 COUNTS OCCURS 5 TIMES PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "WELCOME TO THE ATM".
           PERFORM ENTER-PIN UNTIL VALID = 'Y'.
           PERFORM MENU-PARAGRAPH UNTIL CHOICE = 4.
           DISPLAY "THANK YOU FOR USING THE ATM".
           STOP RUN.
       
       ENTER-PIN.
           DISPLAY "PLEASE ENTER YOUR PIN NUMBER".
           ACCEPT PIN-NUMBER.
           IF PIN-NUMBER = 1234
               MOVE 'Y' TO VALID
           ELSE
               DISPLAY "INVALID PIN NUMBER"
               MOVE 'N' TO VALID
           END-IF.
       
       MENU-PARAGRAPH.
           DISPLAY "PLEASE CHOOSE AN OPTION".
           DISPLAY "1. CHECK BALANCE".
           DISPLAY "2. WITHDRAW CASH".
           DISPLAY "3. DEPOSIT CASH".
           DISPLAY "4. EXIT".
           ACCEPT CHOICE.
           EVALUATE CHOICE
               WHEN 1
                   PERFORM BALANCE-PARAGRAPH
               WHEN 2
                   PERFORM WITHDRAW-PARAGRAPH
               WHEN 3
                   PERFORM DEPOSIT-PARAGRAPH
               WHEN 4
                   CONTINUE
               WHEN OTHER
                   DISPLAY "INVALID OPTION"
           END-EVALUATE.
       
       BALANCE-PARAGRAPH.
           DISPLAY "YOUR BALANCE IS ", BALANCE.
       
       WITHDRAW-PARAGRAPH.
           DISPLAY "PLEASE ENTER THE AMOUNT TO WITHDRAW".
           ACCEPT AMOUNT.
           IF AMOUNT > BALANCE
               DISPLAY "INSUFFICIENT FUNDS"
           ELSE
               SUBTRACT AMOUNT FROM BALANCE
               PERFORM CALCULATE-DENOMINATIONS
               PERFORM DISPLAY-DENOMINATIONS
               DISPLAY "PLEASE COLLECT YOUR CASH"
               DISPLAY "YOUR NEW BALANCE IS ", BALANCE
           END-IF.
       
       DEPOSIT-PARAGRAPH.
           DISPLAY "PLEASE ENTER THE AMOUNT TO DEPOSIT".
           ACCEPT AMOUNT.
           ADD AMOUNT TO BALANCE
           DISPLAY "YOUR NEW BALANCE IS ", BALANCE.
       
       CALCULATE-DENOMINATIONS.
           MOVE 500 TO DENOMINATIONS (1).
           MOVE 200 TO DENOMINATIONS (2).
           MOVE 100 TO DENOMINATIONS (3).
           MOVE 50 TO DENOMINATIONS (4).
           MOVE 10 TO DENOMINATIONS (5).
           MOVE ZEROES TO COUNTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5 OR AMOUNT = ZERO
               DIVIDE DENOMINATIONS (I) INTO AMOUNT 
               GIVING COUNTS (I) REMAINDER AMOUNT
           END-PERFORM.
       
       DISPLAY-DENOMINATIONS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5 
           OR COUNTS (I) = ZERO
               IF COUNTS (I) NOT = ZERO
                   DISPLAY COUNTS (I), " X ", DENOMINATIONS (I)
               END-IF
           END-PERFORM.