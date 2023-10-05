       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CHOICE PIC 9 VALUE ZERO.
       01 NUM1 PIC 9(5) VALUE ZERO.
       01 NUM2 PIC 9(5) VALUE ZERO.
       01 RESULT PIC 9(5)V99 VALUE ZERO.
       01 INCOME PIC 9(7)V99 VALUE ZERO.
       01 TAX PIC 9(5)V99 VALUE ZERO.
       01 GST PIC 9(5)V99 VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Welcome to the calculator program."
           PERFORM MENU-PARAGRAPH UNTIL CHOICE = 6
           STOP RUN.
       
       MENU-PARAGRAPH.
           DISPLAY "Please choose an option: "
           DISPLAY "1. Add two numbers"
           DISPLAY "2. Subtract two numbers"
           DISPLAY "3. Multiply two numbers"
           DISPLAY "4. Divide two numbers"
           DISPLAY "5. Calculate tax and GST based on income"
           DISPLAY "6. Exit the program"
           ACCEPT CHOICE
           EVALUATE CHOICE
               WHEN 1
                   PERFORM ADD-PARAGRAPH
               WHEN 2
                   PERFORM SUB-PARAGRAPH
               WHEN 3
                   PERFORM MUL-PARAGRAPH
               WHEN 4
                   PERFORM DIV-PARAGRAPH
               WHEN 5
                   PERFORM TAX-PARAGRAPH
               WHEN 6
                   DISPLAY "Thank you for using the calculator program."
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
           END-EVALUATE.
       
       ADD-PARAGRAPH.
           DISPLAY "Enter the first number: "
           ACCEPT NUM1
           DISPLAY "Enter the second number: "
           ACCEPT NUM2
           COMPUTE RESULT = NUM1 + NUM2
           DISPLAY "The sum of the two numbers is: " RESULT.
       
       SUB-PARAGRAPH.
           DISPLAY "Enter the first number: "
           ACCEPT NUM1
           DISPLAY "Enter the second number: "
           ACCEPT NUM2
           COMPUTE RESULT = NUM1 - NUM2
           DISPLAY "The difference of the two numbers is: " RESULT.
       
       MUL-PARAGRAPH.
           DISPLAY "Enter the first number: "
           ACCEPT NUM1
           DISPLAY "Enter the second number: "
           ACCEPT NUM2
           COMPUTE RESULT = NUM1 * NUM2
           DISPLAY "The product of the two numbers is: " RESULT.
       
       DIV-PARAGRAPH.
           DISPLAY "Enter the first number: "
           ACCEPT NUM1
           DISPLAY "Enter the second number: "
           ACCEPT NUM2
           IF NUM2 = 0 THEN
               DISPLAY "Cannot divide by zero."
               MOVE ZERO TO RESULT
           ELSE
               COMPUTE RESULT = NUM1 / NUM2
               DISPLAY "The quotient of the two numbers is: " RESULT
           END-IF.
       
       TAX-PARAGRAPH.
           DISPLAY "Enter your income: "
           ACCEPT INCOME
           IF INCOME < 250000 THEN
               MOVE ZERO TO TAX
               MOVE ZERO TO GST
               DISPLAY "You are exempted from tax and GST."
           ELSE IF INCOME < 500000 THEN
               COMPUTE TAX = INCOME * 0.05
               COMPUTE GST = TAX * 0.18
               DISPLAY "Your tax amount is: " TAX
               DISPLAY "Your GST amount is: " GST
               COMPUTE RESULT = INCOME - TAX - GST 
               DISPLAY "Your net income is: " RESULT 
           ELSE IF INCOME < 1000000 THEN 
               COMPUTE TAX = INCOME * 0.20 
               COMPUTE GST = TAX * 0.18 
               DISPLAY "Your tax amount is: " TAX 
               DISPLAY "Your GST amount is: " GST 
               COMPUTE RESULT = INCOME - TAX - GST 
               DISPLAY "Your net income is: " RESULT 
            ELSE 
                COMPUTE TAX = INCOME * 0.30 
                COMPUTE GST = TAX * 0.18 
                DISPLAY "Your tax amount is: " TAX 
                DISPLAY "Your GST amount is: " GST 
                COMPUTE RESULT = INCOME - TAX - GST 
                DISPLAY "Your net income is: " RESULT 
            END-IF.