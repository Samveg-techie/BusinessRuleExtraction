       IDENTIFICATION DIVISION.
       PROGRAM-ID. BMI.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PERSON-DETAILS.
          05 NAME PIC X(20) VALUE SPACES.
          05 HEIGHTT PIC 9(3)V9(2) VALUE ZEROES.
          05 WEIGHT PIC 9(3)V9(2) VALUE ZEROES.
          05 AGE PIC 9(2) VALUE ZEROES.
          05 GENDER PIC X VALUE SPACE.
       01 CALCULATIONS.
          05 BMI PIC 9(2)V9(2) VALUE ZEROES.
          05 HEART-RATE PIC 9(3) VALUE ZEROES.
          05 BLOOD-PRESSURE PIC 9(3) VALUE ZEROES.
       01 CONSTANTS.
          05 MAX-HEART-RATE PIC 9(3) VALUE 220.
          05 MIN-BLOOD-PRESSURE PIC 9(3) VALUE 120.
          05 MAX-BLOOD-PRESSURE PIC 9(3) VALUE 180.
       01 COMMENT PIC X(30) VALUE SPACES.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
       
           DISPLAY "Enter your name: ".
           ACCEPT NAME FROM CONSOLE.
       
           DISPLAY "Enter your height in meters: ".
           ACCEPT HEIGHTT FROM CONSOLE.
       
           DISPLAY "Enter your weight in kilograms: ".
           ACCEPT WEIGHT FROM CONSOLE.
       
           DISPLAY "Enter your age: ".
           ACCEPT AGE FROM CONSOLE.
       
           DISPLAY "Enter your gender (M/F): ".
           ACCEPT GENDER FROM CONSOLE.
       
           COMPUTE BMI = WEIGHT / (HEIGHTT ** 2).
       
           EVALUATE TRUE
               WHEN BMI < 18.5
                   MOVE "Underweight" TO COMMENT
               WHEN BMI >= 18.5 AND BMI < 25
                   MOVE "Normal weight" TO COMMENT
               WHEN BMI >= 25 AND BMI < 30
                   MOVE "Overweight" TO COMMENT
               WHEN BMI >= 30
                   MOVE "Obese" TO COMMENT
               WHEN OTHER
                   MOVE "Invalid input" TO COMMENT
           END-EVALUATE.
       
           DISPLAY "Your BMI is: ", BMI.
       
           DISPLAY "Your weight status is: ", COMMENT.
       
           COMPUTE HEART-RATE = MAX-HEART-RATE - AGE.
       
           DISPLAY "Your maximum heart",
            "rate is: ", HEART-RATE, " beats per minute".
       
           IF GENDER = 'M' OR GENDER = 'm'
               COMPUTE BLOOD-PRESSURE = MIN-BLOOD-PRESSURE + AGE
           ELSE IF GENDER = 'F' OR GENDER = 'f'
               COMPUTE BLOOD-PRESSURE = MAX-BLOOD-PRESSURE - AGE
           ELSE
               MOVE ZEROES TO BLOOD-PRESSURE
           END-IF.
       
           DISPLAY "Your blood pressure is: ", BLOOD-PRESSURE, " mmHg".
       
       STOP RUN.
       
       