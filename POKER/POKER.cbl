       IDENTIFICATION DIVISION.
       PROGRAM-ID. POKER-HANDS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CARD-TABLE VALUE SPACES.
          05 CARD-VALUE PIC X(1) OCCURS 6 TIMES.
          05 CARD-SUIT PIC X(1) OCCURS 6 TIMES.
       01 HAND-RANKING PIC X(20) VALUE SPACES.
       01 HAND-COUNTS VALUE ZEROS.
          05 PAIR-COUNT PIC 9(1).
          05 THREE-COUNT PIC 9(1).
          05 FOUR-COUNT PIC 9(1).
       01 HAND-FLAGS VALUE ZEROS.
          05 STRAIGHT-FLAG PIC 9(1).
          05 FLUSH-FLAG PIC 9(1).
       01 INDEXES VALUE ZEROS.
          05 I PIC 9(1).
          05 J PIC 9(1).
          05 K PIC 9(1).
       01 TEMPORARY VALUE ZEROS.
          05 TEMP-VALUE PIC X(1).
          05 TEMP-SUIT PIC X(1).
       PROCEDURE DIVISION.
       MAIN-SECTION.
           ACCEPT CARD-TABLE
           PERFORM SORT-CARDS
           PERFORM COUNT-HANDS
           PERFORM CHECK-STRAIGHT
           PERFORM CHECK-FLUSH
           PERFORM DETERMINE-RANKING
           DISPLAY HAND-RANKING
           STOP RUN.
       
       SORT-CARDS SECTION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
              PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > 6
                 IF CARD-VALUE (I) < CARD-VALUE (J)
                    MOVE CARD-VALUE (I) TO TEMP-VALUE
                    MOVE CARD-SUIT (I) TO TEMP-SUIT
                    MOVE CARD-VALUE (J) TO CARD-VALUE (I)
                    MOVE CARD-SUIT (J) TO CARD-SUIT (I)
                    MOVE TEMP-VALUE TO CARD-VALUE (J)
                    MOVE TEMP-SUIT TO CARD-SUIT (J)
                 END-IF
              END-PERFORM
           END-PERFORM.
       
       COUNT-HANDS SECTION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
              PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > 6
                 IF CARD-VALUE (I) = CARD-VALUE (J)
                    ADD 1 TO K
                 END-IF
              END-PERFORM
              EVALUATE K
                 WHEN ZERO CONTINUE
                 WHEN ONE ADD 1 TO PAIR-COUNT
                 WHEN TWO ADD -1 TO PAIR-COUNT ADD 1 TO THREE-COUNT
                 WHEN THREE ADD -2 TO PAIR-COUNT 
                 ADD -1 TO THREE-COUNT ADD 1 TO FOUR-COUNT
              END-EVALUATE
              MOVE ZERO TO K
           END-PERFORM.
       
       CHECK-STRAIGHT SECTION.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 6
              IF CARD-VALUE (I) NOT = CARD-VALUE (I - 1) + 1 
              AND NOT (CARD-VALUE (I) = "A" 
              AND CARD-VALUE (I - 1) = "5")
                 EXIT SECTION
              END-IF
           END-PERFORM
           MOVE ONE TO STRAIGHT-FLAG.
       
       CHECK-FLUSH SECTION.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 6
              IF CARD-SUIT (I) NOT = CARD-SUIT (I - 1)
                 EXIT SECTION
              END-IF
           END-PERFORM
           MOVE ONE TO FLUSH-FLAG.
       
       DETERMINE-RANKING SECTION.
           EVALUATE TRUE 
              WHEN STRAIGHT-FLAG = ONE AND FLUSH-FLAG = ONE 
              AND CARD-VALUE (6) = "A"
                 MOVE "ROYAL FLUSH" TO HAND-RANKING 
              WHEN STRAIGHT-FLAG = ONE AND FLUSH-FLAG = ONE 
                 MOVE "STRAIGHT FLUSH" TO HAND-RANKING 
              WHEN FOUR-COUNT = ONE 
                 MOVE "FOUR OF A KIND" TO HAND-RANKING 
              WHEN THREE-COUNT = ONE AND PAIR-COUNT = ONE 
                 MOVE "FULL HOUSE" TO HAND-RANKING 
              WHEN FLUSH-FLAG = ONE 
                 MOVE "FLUSH" TO HAND-RANKING 
              WHEN STRAIGHT-FLAG = ONE 
                 MOVE "STRAIGHT" TO HAND-RANKING 
              WHEN THREE-COUNT = ONE 
                 MOVE "THREE OF A KIND" TO HAND-RANKING 
              WHEN PAIR-COUNT = TWO 
                 MOVE "TWO PAIR" TO HAND-RANKING 
              WHEN PAIR-COUNT = ONE 
                 MOVE "ONE PAIR" TO HAND-RANKING 
              WHEN OTHER 
                 MOVE "HIGH CARD" TO HAND-RANKING
           END-EVALUATE.
       