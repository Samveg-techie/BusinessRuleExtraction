      *========================== COB-FAKER ===========================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-02-11  1.0.1    Add BANK numbers
      *================================================================*

       IDENTIFICATION DIVISION.
      *========================

       PROGRAM-ID.             FAKBANK.

       ENVIRONMENT DIVISION.
      *=====================

       CONFIGURATION SECTION.
      *----------------------

       SOURCE-COMPUTER.
           IBM-Z15.
      *    IBM-Z15 DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
      *---------------------

       FILE-CONTROL.
      /
       DATA DIVISION.
      *==============

       FILE SECTION.
      *-------------

       WORKING-STORAGE SECTION.
      *------------------------

       01  W-FOUND-DX              PIC S9(4)  COMP.
       01  W-RANDOM-NO             PIC S9(4)V9(9)
                                              COMP.
       01  W-RANDOM-SUB            PIC S9(4)  COMP.
       01  W-SUB-1                 PIC S9(4)  COMP.
       01  W-SUB-1-SAVE            PIC S9(4)  COMP.
       01  W-SUB-2                 PIC S9(4)  COMP.
       01  W-SUB-D                 PIC S9(4)  COMP.
       01  W-DIGIT-CNT             PIC S9(4)  COMP.
       01  W-RANDOM-DIG            PIC 9.
       01  W-TABLE-1               PIC X(30).
       01  W-TABLE-2               PIC X(30)       VALUE SPACES.
       01  W-FAKER-RESULT          PIC X(80).
       01  W-FAKER-FORMAT          PIC X(80).
       01  W-FORMAT-START          PIC X           VALUE '{'.
       01  W-FORMAT-END            PIC X           VALUE '}'.

       01  FILLER                  PIC X(01)       VALUE 'Y'.
           88  W-FIRST-CALL                        VALUE 'Y'.
           88  W-NOT-FIRST-CALL                    VALUE 'N'.

       01  W-COMPILED-DATE.
           05  W-COMPILED-DATE-YYYY
                                   PIC X(04).
           05  W-COMPILED-DATE-MM  PIC X(02).
           05  W-COMPILED-DATE-DD  PIC X(02).
           05  W-COMPILED-TIME-HH  PIC X(02).
           05  W-COMPILED-TIME-MM  PIC X(02).
           05  W-COMPILED-TIME-SS  PIC X(02).
           05  FILLER              PIC X(07).

       01  W-RECURSED-FORMAT.
           05  W-RECURSED-FORMAT-CHAR
                                   PIC X           OCCURS 80
                                                   INDEXED W-RF-DX.
       01  W-RECURSED-FORMAT-REST  PIC X(80).

       01  W-HASH                  PIC X(01)       VALUE '#'.
       01  W-PERCENT               PIC X(01)       VALUE '%'.
       01  W-FAKRAND-PROG          PIC X(8)        VALUE 'FAKRAND'.
       01  W-PRODUCTS              PIC S9(09) COMP.
       01  W-QUOTIENT              PIC S9(09) COMP.
       01  W-REMAINDER             PIC S9(09) COMP.
       01  W-CHECK-DIGIT           PIC S9(09) COMP.

       01  W-BANK-ROUTING          PIC X(09).
       01  FILLER REDEFINES W-BANK-ROUTING.
           05  W-BANK-ROUTING-DIG  PIC 9(01)       OCCURS 9.

       01  W-FAKRAND-PARAMETER.    
           05  FAKRAND-SEED-NO     PIC 9(09)  COMP VALUE 0.

           05  FAKRAND-SEED-TEXT   PIC X(80)       VALUE SPACES.
           
           05  FAKRAND-RANDOM-NO   PIC V9(09) COMP.

       01  BANK-ACCOUNT-FORMATS.
           05  BANK-ACCOUNT-FORMAT-CNT
                                   PIC S9(4)  COMP VALUE 7.
           05  BANK-ACCOUNT-FORMAT-OCCS.
               10  FILLER          PIC X(12)       VALUE '######'.
               10  FILLER          PIC X(12)       VALUE '#######'.
               10  FILLER          PIC X(12)       VALUE '########'.
               10  FILLER          PIC X(12)       VALUE '#########'.
               10  FILLER          PIC X(12)       VALUE '##########'.
               10  FILLER          PIC X(12)       VALUE '###########'.
               10  FILLER          PIC X(12)       VALUE '############'.
           05  FILLER REDEFINES BANK-ACCOUNT-FORMAT-OCCS.
               10  FILLER                          OCCURS 7
                                                   INDEXED BR-DX.
                   15  BANK-ACCOUNT-FORMAT
                                   PIC X(12).

       01  BANK-ROUTING-FORMATS.
           05  BANK-ROUTING-FORMAT-CNT
                                   PIC S9(4)  COMP VALUE 1.
           05  BANK-ROUTING-FORMAT-OCCS.
      ****     Standard 8-digit routing number plus check digit:
               10  FILLER          PIC X(10)       VALUE '########C'.
           05  FILLER REDEFINES BANK-ROUTING-FORMAT-OCCS.
               10  FILLER                          OCCURS 1
                                                   INDEXED BR-DX.
                   15  BANK-ROUTING-FORMAT
                                   PIC X(10).

      /
       LINKAGE SECTION.
      *----------------

       01  L-PARAMETER.            
           05  FAKER-PROVIDER-FUNCTION
                                   PIC X(30).
               88  ADDRESS-ADDRESS                 VALUE 
                                   'ADDRESS-ADDRESS'.
               88  ADDRESS-BUILDING-NO             VALUE 
                                   'ADDRESS-BUILDING-NO'.
               88  ADDRESS-CITY                    VALUE 
                                   'ADDRESS-CITY'.
               88  ADDRESS-CITY-PREFIX             VALUE 
                                   'ADDRESS-CITY-PREFIX'.
               88  ADDRESS-CITY-SUFFIX             VALUE 
                                   'ADDRESS-CITY-SUFFIX'.
               88  ADDRESS-MILITARY-APO            VALUE
                                   'ADDRESS-MILITARY-APO'.
               88  ADDRESS-MILITARY-DPO            VALUE
                                   'ADDRESS-MILITARY-DPO'.
               88  ADDRESS-MILITARY-SHIP-PREFIX    VALUE
                                   'ADDRESS-MILITARY-SHIP-PREFIX'.
               88  ADDRESS-MILITARY-STATE-ABBR     VALUE
                                   'ADDRESS-MILITARY-STATE-ABBR'.
               88  ADDRESS-POSTCODE                VALUE 
                                   'ADDRESS-POSTCODE'.
               88  ADDRESS-SECONDARY-ADDRESS       VALUE 
                                   'ADDRESS-SECONDARY-ADDRESS'.
               88  ADDRESS-STATE                   VALUE 
                                   'ADDRESS-STATE'.
               88  ADDRESS-STATE-ABBR              VALUE 
                                   'ADDRESS-STATE-ABBR'.
               88  ADDRESS-STATE-POSTCODE          VALUE 
                                   'ADDRESS-STATE-POSTCODE'.
               88  ADDRESS-STREET-ADDRESS          VALUE 
                                   'ADDRESS-STREET-ADDRESS'.
               88  ADDRESS-STREET-NAME             VALUE 
                                   'ADDRESS-STREET-NAME'.
               88  ADDRESS-STREET-SUFFIX           VALUE 
                                   'ADDRESS-STREET-SUFFIX'.
               88  ADDRESS-TERRITORY-ABBR          VALUE
                                   'ADDRESS-TERRITORY-ABBR'.
               88  BANK-ACCOUNT                    VALUE
                                   'BANK-ACCOUNT'.
               88  BANK-ROUTING                    VALUE
                                   'BANK-ROUTING'.
               88  COMPANY-COMPANY                 VALUE
                                   'COMPANY-COMPANY'.
               88  COMPANY-SUFFIX                  VALUE
                                   'COMPANY-SUFFIX'.
               88  PERSON-FIRST-NAME               VALUE 
                                   'PERSON-FIRST-NAME'.    
               88  PERSON-FIRST-NAME-MALE          VALUE
                                   'PERSON-FIRST-NAME-MALE'.    
               88  PERSON-FIRST-NAME-FEMALE        VALUE 
                                   'PERSON-FIRST-NAME-FEMALE'.    
               88  PERSON-LAST-NAME                VALUE 
                                   'PERSON-LAST-NAME'.    
               88  PERSON-LAST-NAME-MALE           VALUE 
                                   'PERSON-LAST-NAME-MALE'.    
               88  PERSON-LAST-NAME-FEMALE         VALUE 
                                   'PERSON-LAST-NAME-FEMALE'.    
               88  PERSON-NAME                     VALUE 
                                   'PERSON-NAME'.    
               88  PERSON-NAME-MALE                VALUE 
                                   'PERSON-NAME-MALE'.    
               88  PERSON-NAME-FEMALE              VALUE 
                                   'PERSON-NAME-FEMALE'.    
               88  PERSON-PREFIX                   VALUE 
                                   'PERSON-PREFIX'.    
               88  PERSON-PREFIX-MALE              VALUE 
                                   'PERSON-PREFIX-MALE'.    
               88  PERSON-PREFIX-FEMALE            VALUE 
                                   'PERSON-PREFIX-FEMALE'.    
               88  PERSON-SUFFIX                   VALUE 
                                   'PERSON-SUFFIX'.    
               88  PERSON-SUFFIX-MALE              VALUE 
                                   'PERSON-SUFFIX-MALE'.    
               88  PERSON-SUFFIX-FEMALE            VALUE 
                                   'PERSON-SUFFIX-FEMALE'. 
               88  TAXID-EIN                       VALUE 
                                   'TAXID-EIN'. 
               88  TAXID-EIN-HYPHEN                VALUE 
                                   'TAXID-EIN-HYPHEN'. 
               88  TAXID-ITIN                      VALUE 
                                   'TAXID-ITIN'. 
               88  TAXID-ITIN-HYPHEN               VALUE 
                                   'TAXID-ITIN-HYPHEN'. 
               88  TAXID-SSN                       VALUE 
                                   'TAXID-SSN'. 
               88  TAXID-SSN-HYPHEN                VALUE 
                                   'TAXID-SSN-HYPHEN'. 
               88  TELEPHONE                       VALUE 
                                   'TELEPHONE'. 

           05  FAKER-SEED-NO       PIC 9(9)   COMP VALUE 0.

           05  FAKER-SEED-TEXT     PIC X(80)       VALUE SPACES.

      **** Output fields:
      ****     FAKER-RESPONSE-CODE
      ****         Use 88 levels to determine result of calls.
      ****     FAKER-RESPONSE-MSG
      ****         Non-space if bad response.
      ****     FAKER-RESULT
      ****         Returned result of the call.
      ****     FAKER-RESULT-FIELDS
      ****         Populated for certain compound results - redefined
      ****         for address and person fields.
      ****     FAKER-INFO-CNT
      ****         Debugging information count.
      ****     FAKER-INFO-OCCS
      ****         Debugging information.

           05  FAKER-RESPONSE-CODE PIC 9(4). 
               88  FAKER-RESPONSE-GOOD             VALUE 0.
               88  FAKER-UNKNOWN-PROVIDER          VALUE 10.
               88  FAKER-UNKNOWN-FUNCTION          VALUE 20.
               88  FAKER-UNKNOWN-FORMAT            VALUE 30.

           05  FAKER-RESPONSE-MSG  PIC X(80). 

           05  FAKER-RESULT        PIC X(80). 

           05  FAKER-RESULT-FIELDS PIC X(80). 

      **** These fields are populated only for ADDRESS-ADDRESS calls:
           05  FAKER-ADDRESS REDEFINES FAKER-RESULT-FIELDS.
               10  FAKER-ADDRESS-STREET
                                   PIC X(35).
               10  FAKER-ADDRESS-CITY
                                   PIC X(25).
               10  FAKER-ADDRESS-STATE
                                   PIC X(10).
               10  FAKER-ADDRESS-POSTCODE
                                   PIC X(10).

      **** These fields are populated only for PERSON-NAME, 
      **** PERSON-NAME-MALE and PERSON-NAME-FEMALE calls:
           05  FAKER-PERSON REDEFINES FAKER-RESULT-FIELDS.
               10  FAKER-PERSON-PREFIX
                                   PIC X(10).
               10  FAKER-PERSON-FIRST-NAME
                                   PIC X(25).
               10  FAKER-PERSON-LAST-NAME
                                   PIC X(35).
               10  FAKER-PERSON-SUFFIX
                                   PIC X(10).

      **** These fields are populated only for TELEPHONE calls:
           05  FAKER-TELEPHONE REDEFINES FAKER-RESULT-FIELDS.
               10  FAKER-TELEPHONE-AREA-CODE
                                   PIC X(03).
               10  FILLER          PIC X(01).
               10  FAKER-TELEPHONE-PREFIX
                                   PIC X(03).
               10  FILLER          PIC X(01).
               10  FAKER-TELEPHONE-SUFFIX
                                   PIC X(04).
               10  FILLER          PIC X(01).
               10  FAKER-TELEPHONE-EXTENSION
                                   PIC X(04).

           05  FAKER-INFO-CNT      PIC S9(4)  COMP. 

           05  FAKER-INFO-OCCS.
               10  FAKER-INFO                      OCCURS 20
                                                   INDEXED FI-DX
                                                           FI-DX2.
                   15  FAKER-TABLE PIC X(30).
                   15  FAKER-RANDOM-NO-SUB
                                   PIC S9(4)V9(9)
                                              COMP.
                   15  FAKER-TABLE-ENTRY
                                   PIC S9(4)  COMP.
      /
       PROCEDURE DIVISION USING L-PARAMETER.
      *==================

       MAIN.
      *-----

           PERFORM SUB-1000-START-UP THRU SUB-1000-EXIT

           PERFORM SUB-2000-PROCESS THRU SUB-2000-EXIT

           PERFORM SUB-3000-SHUT-DOWN THRU SUB-3000-EXIT
           .
       MAIN-EXIT.
           GOBACK.
      /
       SUB-1000-START-UP.
      *------------------

           IF      W-NOT-FIRST-CALL
               GO TO SUB-1000-EXIT
           END-IF

           SET W-NOT-FIRST-CALL    TO TRUE
           MOVE FUNCTION WHEN-COMPILED 
                                   TO W-COMPILED-DATE

           DISPLAY 'FAKBANK  compiled on '
               W-COMPILED-DATE-YYYY '/'
               W-COMPILED-DATE-MM   '/'
               W-COMPILED-DATE-DD   ' at '
               W-COMPILED-TIME-HH   ':'
               W-COMPILED-TIME-MM   ':'
               W-COMPILED-TIME-SS
           .
       SUB-1000-EXIT.
           EXIT.
      /
       SUB-2000-PROCESS.
      *-----------------

           MOVE 0                  TO FAKER-INFO-CNT
           SET  FI-DX              TO FAKER-INFO-CNT
           MOVE LOW-VALUES         TO FAKER-INFO-OCCS

           EVALUATE TRUE
             WHEN BANK-ACCOUNT
               PERFORM SUB-9010-ACCOUNT THRU SUB-9010-EXIT

             WHEN BANK-ROUTING
               PERFORM SUB-9020-ROUTING THRU SUB-9020-EXIT

             WHEN OTHER
               SET  FAKER-UNKNOWN-FUNCTION
                                   IN L-PARAMETER
                                   TO TRUE
               STRING 'Unknown FAKBANK function "'
                       FAKER-PROVIDER-FUNCTION
                                   IN L-PARAMETER
                       '"'  DELIMITED SIZE
                                 INTO FAKER-RESPONSE-MSG
                                   IN L-PARAMETER   
               GO TO SUB-2000-EXIT
           END-EVALUATE

           SET  FAKER-INFO-CNT     TO FI-DX 

           MOVE W-FAKER-RESULT     TO FAKER-RESULT
           .
       SUB-2000-EXIT.
           EXIT.
      /
       SUB-3000-SHUT-DOWN.
      *-------------------

      D    IF      FAKER-RESPONSE-GOOD
      D        DISPLAY 'FAKBANK completed successfully'
      D    ELSE
      D        DISPLAY 'FAKBANK ended with error '
      D                FAKER-RESPONSE-CODE
      D                ': '
      D                FAKER-RESPONSE-MSG
      D    END-IF
           .
       SUB-3000-EXIT.
           EXIT.
      /
       SUB-9010-ACCOUNT.
      *-----------------

           PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

           COMPUTE W-RANDOM-SUB    =  FAKRAND-RANDOM-NO
                                      * BANK-ACCOUNT-FORMAT-CNT
                                      + 1

           SET  FI-DX           UP BY 1
           MOVE 'BANK-ACCOUNT'     TO FAKER-TABLE(FI-DX)
           MOVE FAKRAND-RANDOM-NO  TO FAKER-RANDOM-NO-SUB(FI-DX)
           MOVE W-RANDOM-SUB       TO FAKER-TABLE-ENTRY(FI-DX)

           MOVE BANK-ACCOUNT-FORMAT(W-RANDOM-SUB)
                                   TO W-FAKER-RESULT

           PERFORM SUB-9810-REPLACE-DIGIT THRU SUB-9810-EXIT
               VARYING W-SUB-D FROM 1 BY 1
                 UNTIL W-SUB-D > LENGTH OF BANK-ACCOUNT-FORMAT
           .
       SUB-9010-EXIT.
           EXIT.
      /
       SUB-9020-ROUTING.
      *-----------------

           PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

           COMPUTE W-RANDOM-SUB    =  FAKRAND-RANDOM-NO
                                      * BANK-ROUTING-FORMAT-CNT
                                      + 1

           SET  FI-DX           UP BY 1
           MOVE 'BANK-ROUTING'     TO FAKER-TABLE(FI-DX)
           MOVE FAKRAND-RANDOM-NO  TO FAKER-RANDOM-NO-SUB(FI-DX)
           MOVE W-RANDOM-SUB       TO FAKER-TABLE-ENTRY(FI-DX)

           MOVE BANK-ROUTING-FORMAT(W-RANDOM-SUB)
                                   TO W-FAKER-RESULT

           PERFORM SUB-9810-REPLACE-DIGIT THRU SUB-9810-EXIT
               VARYING W-SUB-D FROM 1 BY 1
                 UNTIL W-SUB-D > LENGTH OF BANK-ACCOUNT-FORMAT

           MOVE W-FAKER-RESULT     TO W-BANK-ROUTING

           COMPUTE W-PRODUCTS      =  ( W-BANK-ROUTING-DIG(1)
                                      + W-BANK-ROUTING-DIG(4)
                                      + W-BANK-ROUTING-DIG(7)) * 3
                                      + 
                                      ( W-BANK-ROUTING-DIG(2)
                                      + W-BANK-ROUTING-DIG(5)
                                      + W-BANK-ROUTING-DIG(8)) * 7
                                      +
                                        W-BANK-ROUTING-DIG(3)
                                      + W-BANK-ROUTING-DIG(6)

           DIVIDE W-PRODUCTS       BY 10
                               GIVING W-QUOTIENT
                            REMAINDER W-REMAINDER

           IF      W-REMAINDER = 0
               MOVE 0              TO W-CHECK-DIGIT
           ELSE    
               SUBTRACT W-REMAINDER
                                 FROM 10 
                               GIVING W-CHECK-DIGIT
           END-IF    

           MOVE W-CHECK-DIGIT      TO W-BANK-ROUTING-DIG(9)
           MOVE W-BANK-ROUTING     TO W-FAKER-RESULT
           .
       SUB-9020-EXIT.
           EXIT.
      /
       SUB-9810-REPLACE-DIGIT.
      *-----------------------

           IF      W-FAKER-RESULT(W-SUB-D : 1) NOT = W-HASH
           AND                                       W-PERCENT
               GO TO SUB-9810-EXIT
           END-IF

           PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

           IF      W-FAKER-RESULT(W-SUB-D : 1) = W-PERCENT
               COMPUTE W-RANDOM-DIG
                                   =  FAKRAND-RANDOM-NO
                                      * 9
                                      + 1
           ELSE       
               COMPUTE W-RANDOM-DIG
                                   =  FAKRAND-RANDOM-NO
                                      * 10
           END-IF

           MOVE W-RANDOM-DIG       TO W-FAKER-RESULT(W-SUB-D : 1)
           .
       SUB-9810-EXIT.
           EXIT.
      /
       SUB-9901-CALL-FAKRAND.
      *----------------------

           CALL W-FAKRAND-PROG  USING W-FAKRAND-PARAMETER 
           .
       SUB-9901-EXIT.
           EXIT.
