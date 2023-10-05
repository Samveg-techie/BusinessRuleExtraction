      *========================== COB-FAKER ===========================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-02-08  1.0      First release
      *================================================================*

       IDENTIFICATION DIVISION.
      *========================

       PROGRAM-ID.             FAKCOMP.

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

       01  W-FAKPERS-PROG          PIC X(08)       VALUE 'FAKPERS'.
       01  W-FAKRAND-PROG          PIC X(08)       VALUE 'FAKRAND'.

       01  W-FORMAT-ENTRY          PIC X(04).
           88  W-FORMAT-ENTRY-IS-FORMAT            VALUE '  '.

       01  W-FAKER-PARAMETER.      
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

       01  W-FAKRAND-PARAMETER.    
           05  FAKRAND-SEED-NO     PIC 9(09)  COMP VALUE 0.

           05  FAKRAND-SEED-TEXT   PIC X(80)       VALUE SPACES.
           
           05  FAKRAND-RANDOM-NO   PIC V9(09) COMP.

       01  FORMATS-COMPANY.
           05  FORMAT-COMPANY-CNT  PIC S9(4)  COMP VALUE 3.
           05  FORMAT-COMPANY-WEIGHT-TOT
                                   PIC S99V9(9)
                                              COMP VALUE 0.
           05  FORMAT-COMPANY-OCCS.
               10  FILLER          PIC X(32)       VALUE '{LN} {CS}'.
               10  FILLER          PIC SV9(9) COMP VALUE  0.500000000.
               10  FILLER          PIC X(32)       VALUE 
                                                       '{LN}-{LN} {CS}'.
               10  FILLER          PIC SV9(9) COMP VALUE  0.250000000.
               10  FILLER          PIC X(32)       VALUE
                                              '{LN}, {LN} and {LN} LLC'.
               10  FILLER          PIC SV9(9) COMP VALUE  0.250000000.
           05  FILLER REDEFINES FORMAT-COMPANY-OCCS.
               10  FILLER                          OCCURS 3
                                                   INDEXED FC-DX.
                   15  FORMAT-COMPANY 
                                   PIC X(32).
                   15  FORMAT-COMPANY-WEIGHT
                                   PIC SV9(9) COMP.

       01  COMPANY-SUFFIXES.
           05  COMPANY-SUFFIX-CNT  PIC S9(4)       COMP VALUE 6.
           05  COMPANY-SUFFIX-OCCS.
               10  FILLER          PIC X(14)       VALUE 'Inc'.
               10  FILLER          PIC X(14)       VALUE 'and Sons'.
               10  FILLER          PIC X(14)       VALUE 'LLC'.
               10  FILLER          PIC X(14)       VALUE 'Group'.
               10  FILLER          PIC X(14)       VALUE 'PLC'.
               10  FILLER          PIC X(14)       VALUE 'Ltd'.
           05  FILLER REDEFINES COMPANY-SUFFIX-OCCS.
               10  FILLER                          OCCURS 6
                                                   INDEXED CS-DX.
                   15  COMPANY-SUFFIX PIC X(14).
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
       
       01  L-FORMAT-TABLE-1.
           05  L-FORMAT-ENTRY-CNT-1
                                   PIC S9(4)  COMP.
           05  L-FORMAT-WEIGHT-TOT-1
                                   PIC S99V9(9)
                                              COMP.
           05  L-FORMAT-OCCS-1.
               10  FILLER                          OCCURS 10
                                                   INDEXED L-F-DX-1.
                   15  L-FORMAT-ENTRY-1
                                   PIC X(32).
                   15  L-FORMAT-WEIGHT-1
                                   PIC SV9(9) COMP.

       01  L-FORMAT-TABLE-2.
           05  L-FORMAT-ENTRY-CNT-2
                                   PIC S9(4)  COMP.
           05  L-FORMAT-WEIGHT-TOT-2
                                   PIC S99V9(9)
                                              COMP.
           05  L-FORMAT-OCCS-2.
               10  FILLER                          OCCURS 10
                                                   INDEXED L-F-DX-2.
                   15  L-FORMAT-ENTRY-2
                                   PIC X(32).
                   15  L-FORMAT-WEIGHT-2
                                   PIC SV9(9) COMP.

       01  L-COMPANY-TABLE-1.
           05  L-COMPANY-ENTRY-CNT-1
                                   PIC S9(4)  COMP.
           05  L-COMPANY-OCCS-1.
               10  FILLER                          OCCURS 1000
                                                   INDEXED L-A-DX-1.
                   15  L-COMPANY-ENTRY-1
                                   PIC X(14).
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

           DISPLAY 'FAKCOMP  compiled on '
               W-COMPILED-DATE-YYYY '/'
               W-COMPILED-DATE-MM   '/'
               W-COMPILED-DATE-DD   ' at '
               W-COMPILED-TIME-HH   ':'
               W-COMPILED-TIME-MM   ':'
               W-COMPILED-TIME-SS

           PERFORM SUB-1100-SUM-WEIGHTS THRU SUB-1100-EXIT
           .
       SUB-1000-EXIT.
           EXIT.
      /
       SUB-1100-SUM-WEIGHTS.
      *---------------------

           PERFORM VARYING FC-DX FROM 1 BY 1
                     UNTIL FC-DX > FORMAT-COMPANY-CNT
               ADD  FORMAT-COMPANY-WEIGHT(FC-DX)
                 TO FORMAT-COMPANY-WEIGHT-TOT
           END-PERFORM

      D    DISPLAY 'FAKCOMP weight totals: '
      D    DISPLAY '    ' FORMAT-COMPANY-WEIGHT-TOT
           .
       SUB-1100-EXIT.
           EXIT.
      /
       SUB-2000-PROCESS.
      *-----------------

           MOVE 0                  
             TO FAKER-INFO-CNT     IN L-PARAMETER
           MOVE LOW-VALUES         
             TO FAKER-INFO-OCCS    IN L-PARAMETER

           EVALUATE TRUE
             WHEN COMPANY-COMPANY  IN L-PARAMETER
               PERFORM SUB-9010-COMPANY THRU SUB-9010-EXIT

             WHEN COMPANY-SUFFIX   IN L-PARAMETER
               PERFORM SUB-9020-SUFFIX THRU SUB-9020-EXIT

             WHEN OTHER
               SET  FAKER-UNKNOWN-FUNCTION
                                   IN L-PARAMETER
                                   TO TRUE
               STRING 'Unknown FAKCOMP function "'
                       FAKER-PROVIDER-FUNCTION
                                   IN L-PARAMETER
                       '"'  DELIMITED SIZE
                                 INTO FAKER-RESPONSE-MSG
                                   IN L-PARAMETER
               GO TO SUB-2000-EXIT
           END-EVALUATE

           ADD  1                  
             TO FAKER-INFO-CNT     IN L-PARAMETER
           SET  FI-DX              
             TO FAKER-INFO-CNT     IN L-PARAMETER
           MOVE W-TABLE-1          
             TO FAKER-TABLE        IN L-PARAMETER(FI-DX)

           IF      W-TABLE-1(1:8) = 'FORMATS-'
               PERFORM SUB-2100-FORMAT THRU SUB-2100-EXIT

               IF      NOT FAKER-RESPONSE-GOOD
                                   IN L-PARAMETER
                   GO TO SUB-2000-EXIT
               END-IF
           ELSE
               PERFORM SUB-9800-FIND-RANDOM-COMPANY THRU SUB-9800-EXIT

               MOVE W-FAKER-RESULT 
                 TO FAKER-RESULT   IN L-PARAMETER
           END-IF
           .
       SUB-2000-EXIT.
           EXIT.
      /
       SUB-2100-FORMAT.
      *----------------

           PERFORM SUB-9700-FIND-RANDOM-FORMAT THRU SUB-9700-EXIT

           MOVE W-FAKER-FORMAT     TO W-RECURSED-FORMAT
           MOVE 1                  TO W-SUB-1

           PERFORM SUB-2110-RECURSE-FORMATS THRU SUB-2110-EXIT
               UNTIL W-SUB-1 > LENGTH OF W-RECURSED-FORMAT
               OR    NOT FAKER-RESPONSE-GOOD
                                    IN L-PARAMETER

           IF      NOT FAKER-RESPONSE-GOOD
                                   IN L-PARAMETER
               GO TO SUB-2100-EXIT
           END-IF

           MOVE SPACES         
             TO FAKER-RESULT       IN L-PARAMETER
           MOVE 1                  TO W-SUB-1
                                      W-SUB-2

           PERFORM SUB-2120-PROCESS-FORMATS THRU SUB-2120-EXIT
               UNTIL W-SUB-1 > LENGTH OF W-RECURSED-FORMAT
               OR    W-SUB-2 > LENGTH OF FAKER-RESULT
                                          IN L-PARAMETER
               OR    W-RECURSED-FORMAT(W-SUB-1 : ) = SPACES
           .
       SUB-2100-EXIT.
           EXIT.
      /
       SUB-2110-RECURSE-FORMATS.
      *-------------------------

           IF      W-RECURSED-FORMAT-CHAR(W-SUB-1) NOT = W-FORMAT-START
               ADD  1              TO W-SUB-1
               GO TO SUB-2110-EXIT
           END-IF

           MOVE W-SUB-1            TO W-SUB-1-SAVE
           ADD  1                  TO W-SUB-1

           UNSTRING W-RECURSED-FORMAT
                            DELIMITED W-FORMAT-END
                                 INTO W-FORMAT-ENTRY
                              POINTER W-SUB-1 

           IF      W-FORMAT-ENTRY-IS-FORMAT
               MOVE W-RECURSED-FORMAT(W-SUB-1 : )
                                   TO W-RECURSED-FORMAT-REST
               MOVE W-SUB-1-SAVE   TO W-SUB-1 

               PERFORM SUB-9000-EXAMINE-FIND-FORMAT THRU SUB-9000-EXIT

               IF      NOT FAKER-RESPONSE-GOOD
                                   IN L-PARAMETER
                   GO TO SUB-2110-EXIT
               END-IF
    
               STRING W-FAKER-FORMAT
                            DELIMITED '  '
                                 INTO W-RECURSED-FORMAT
                              POINTER W-SUB-1-SAVE

               MOVE W-RECURSED-FORMAT-REST
                                   TO W-RECURSED-FORMAT(W-SUB-1-SAVE : )
           END-IF
           .
       SUB-2110-EXIT.
           EXIT.
      /
       SUB-2120-PROCESS-FORMATS.
      *-------------------------

           IF      W-RECURSED-FORMAT-CHAR(W-SUB-1) = W-FORMAT-START
               ADD  1              TO W-SUB-1

               UNSTRING W-RECURSED-FORMAT
                            DELIMITED W-FORMAT-END
                                 INTO W-FORMAT-ENTRY
                              POINTER W-SUB-1 

               PERFORM SUB-9000-EXAMINE-FIND-FORMAT THRU SUB-9000-EXIT

               STRING W-FAKER-RESULT
                            DELIMITED '  '
                                 INTO FAKER-RESULT
                                      IN L-PARAMETER
                              POINTER W-SUB-2
           ELSE
               MOVE W-RECURSED-FORMAT-CHAR(W-SUB-1)
                 TO FAKER-RESULT   IN L-PARAMETER(W-SUB-2 : 1)
               ADD  1              TO W-SUB-1
                                      W-SUB-2
           END-IF
           .
       SUB-2120-EXIT.
           EXIT.
      /
       SUB-3000-SHUT-DOWN.
      *-------------------

      D    IF      FAKER-RESPONSE-GOOD
      D                            IN L-PARAMETER
      D        DISPLAY 'FAKCOMP completed successfully'
      D    ELSE
      D        DISPLAY 'FAKCOMP ended with error '
      D                FAKER-RESPONSE-CODE
      D                            IN L-PARAMETER
      D                ': '
      D                FAKER-RESPONSE-MSG
      D                            IN L-PARAMETER
      D    END-IF
           .
       SUB-3000-EXIT.
           EXIT.
      /
       SUB-9000-EXAMINE-FIND-FORMAT.
      *-----------------------------

           EVALUATE W-FORMAT-ENTRY
             WHEN 'CS'
               PERFORM SUB-9020-SUFFIX THRU SUB-9020-EXIT

             WHEN 'LN'
               PERFORM SUB-9200-LAST-NAME THRU SUB-9200-EXIT

             WHEN OTHER
               MOVE SPACES         TO W-TABLE-1
               SET  FAKER-UNKNOWN-FORMAT
                                   IN L-PARAMETER
                                   TO TRUE
               STRING 'Unknown FAKCOMP format "'
                       W-FORMAT-ENTRY
                       '"'  DELIMITED SIZE
                                 INTO FAKER-RESPONSE-MSG
                                   IN L-PARAMETER
               GO TO SUB-9000-EXIT
           END-EVALUATE

           ADD  1                  
             TO FAKER-INFO-CNT     IN L-PARAMETER
           SET  FI-DX              
             TO FAKER-INFO-CNT     IN L-PARAMETER
           MOVE W-TABLE-1          
             TO FAKER-TABLE        IN L-PARAMETER(FI-DX)

           EVALUATE TRUE
             WHEN W-TABLE-1(1:8) = 'FORMATS-'
               PERFORM SUB-9700-FIND-RANDOM-FORMAT THRU SUB-9700-EXIT

             WHEN W-TABLE-1 = 'FIRST-NAME'
             OR               'LAST-NAME'
               MOVE FAKER-RESULT   IN W-FAKER-PARAMETER
                 TO W-FAKER-RESULT             

             WHEN OTHER     
               PERFORM SUB-9800-FIND-RANDOM-COMPANY THRU SUB-9800-EXIT
           END-EVALUATE
           .
       SUB-9000-EXIT.
           EXIT.
      /
       SUB-9010-COMPANY.
      *-----------------

           MOVE 'FORMATS-COMPANY'  TO W-TABLE-1            

           SET  ADDRESS OF L-FORMAT-TABLE-1
             TO ADDRESS OF FORMATS-COMPANY
           .
       SUB-9010-EXIT.
           EXIT.
      /
       SUB-9020-SUFFIX.
      *----------------

           MOVE 'COMPANY-SUFFIXES' TO W-TABLE-1            

           SET  ADDRESS OF L-COMPANY-TABLE-1
             TO ADDRESS OF COMPANY-SUFFIXES
           .
       SUB-9020-EXIT.
           EXIT.
      /
       SUB-9200-LAST-NAME.
      *-------------------
        
           MOVE 'LAST-NAME'        TO W-TABLE-1 
           SET  PERSON-LAST-NAME   IN W-FAKER-PARAMETER
             TO TRUE

           CALL W-FAKPERS-PROG  USING W-FAKER-PARAMETER
           .
       SUB-9200-EXIT.
           EXIT.
      /
       SUB-9700-FIND-RANDOM-FORMAT.
      *----------------------------

           PERFORM SUB-9901-CALL-FAKRAND

           IF      W-TABLE-2 = SPACES
               COMPUTE W-RANDOM-NO =  FAKRAND-RANDOM-NO
                                      * L-FORMAT-WEIGHT-TOT-1
           ELSE
               COMPUTE W-RANDOM-NO =  FAKRAND-RANDOM-NO
                                      * (L-FORMAT-WEIGHT-TOT-1 +
                                         L-FORMAT-WEIGHT-TOT-2)
           END-IF

           MOVE W-RANDOM-NO        TO FAKER-RANDOM-NO-SUB
                                        IN L-PARAMETER(FI-DX)
           MOVE 0                  TO W-FOUND-DX

           PERFORM SUB-9710-FIND-FORMAT THRU SUB-9710-EXIT
               VARYING L-F-DX-1 FROM 1 BY 1
                 UNTIL W-FOUND-DX > 0
                 OR    L-F-DX-1 > L-FORMAT-ENTRY-CNT-1

           EVALUATE TRUE
             WHEN W-FOUND-DX > 0
               MOVE L-FORMAT-ENTRY-1(W-FOUND-DX)
                                   TO W-FAKER-FORMAT

             WHEN W-TABLE-2 NOT = SPACES
               ADD  1              
                 TO FAKER-INFO-CNT IN L-PARAMETER
               SET  FI-DX          
                 TO FAKER-INFO-CNT IN L-PARAMETER
               MOVE W-TABLE-2      
                 TO FAKER-TABLE    IN L-PARAMETER(FI-DX)
               MOVE W-RANDOM-NO    
                 TO FAKER-RANDOM-NO-SUB
                                   IN L-PARAMETER(FI-DX)

               MOVE 0              TO W-FOUND-DX

               PERFORM SUB-9720-FIND-FORMAT THRU SUB-9720-EXIT
                   VARYING L-F-DX-2 FROM 1 BY 1
                     UNTIL W-FOUND-DX > 0
                     OR    L-F-DX-2 > L-FORMAT-ENTRY-CNT-2

               IF      W-FOUND-DX > 0
                   MOVE L-FORMAT-ENTRY-2(W-FOUND-DX)
                                   TO W-FAKER-FORMAT
               ELSE
                   MOVE 'Random format not found'
                                   TO W-FAKER-FORMAT
               END-IF

             WHEN OTHER
               MOVE 'Random format not found'
                                   TO W-FAKER-FORMAT
           END-EVALUATE
           .
       SUB-9700-EXIT.
           EXIT.
      /
       SUB-9710-FIND-FORMAT.
      *---------------------
      
           IF      W-RANDOM-NO <= L-FORMAT-WEIGHT-1(L-F-DX-1)
               SET  W-FOUND-DX     TO L-F-DX-1
               MOVE W-FOUND-DX     TO FAKER-TABLE-ENTRY
                                        IN L-PARAMETER(FI-DX)
           ELSE
               SUBTRACT L-FORMAT-WEIGHT-1(L-F-DX-1)
                                 FROM W-RANDOM-NO
           END-IF
           .
       SUB-9710-EXIT.
           EXIT.
      /
       SUB-9720-FIND-FORMAT.
      *---------------------
      
           IF      W-RANDOM-NO <= L-FORMAT-WEIGHT-2(L-F-DX-2)
               SET  W-FOUND-DX     TO L-F-DX-2
               MOVE W-FOUND-DX     TO FAKER-TABLE-ENTRY
                                        IN L-PARAMETER(FI-DX)
           ELSE
               SUBTRACT L-FORMAT-WEIGHT-2(L-F-DX-2)
                                 FROM W-RANDOM-NO
           END-IF
           .
       SUB-9720-EXIT.
           EXIT.
      /
       SUB-9800-FIND-RANDOM-COMPANY.
      *-----------------------------

           PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

           COMPUTE W-RANDOM-SUB    =  FAKRAND-RANDOM-NO
                                      * L-COMPANY-ENTRY-CNT-1
                                      + 1

           MOVE W-RANDOM-SUB       TO FAKER-RANDOM-NO-SUB
                                        IN L-PARAMETER(FI-DX)
                                      W-FOUND-DX
                                      FAKER-TABLE-ENTRY
                                        IN L-PARAMETER(FI-DX)

           MOVE L-COMPANY-ENTRY-1(W-FOUND-DX)
                                   TO W-FAKER-RESULT
           .
       SUB-9800-EXIT.
           EXIT.
      /
       SUB-9901-CALL-FAKRAND.
      *----------------------

           CALL W-FAKRAND-PROG  USING W-FAKRAND-PARAMETER 
           .
       SUB-9901-EXIT.
           EXIT.
