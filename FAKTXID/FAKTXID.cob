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

       PROGRAM-ID.             FAKTXID.

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

       01  W-EIN-SEQUENCE          PIC 9(7).
       01  W-AREA-COMP             PIC 9(03)  COMP.
       01  W-AREA                  PIC 9(03).
       01  W-GROUP                 PIC 9(02).
       01  W-SERIAL                PIC 9(04).
       01  W-FAKRAND-PROG          PIC X(8)        VALUE 'FAKRAND'.

       01  W-FAKRAND-PARAMETER.    
           05  FAKRAND-SEED-NO     PIC 9(09)  COMP VALUE 0.

           05  FAKRAND-SEED-TEXT   PIC X(80)       VALUE SPACES.
           
           05  FAKRAND-RANDOM-NO   PIC V9(09) COMP.

       01  FORMAT-EIN.
           05  FORMAT-EIN-PREFIX   PIC 9(2).
           05  FORMAT-EIN-SEQUENCE PIC 9(7).
       
       01  FORMAT-EIN-HYPHEN.
           05  FORMAT-EIN-HYPHEN-PREFIX
                                   PIC 9(2).
           05  FILLER              PIC X           VALUE '-'.
           05  FORMAT-EIN-HYPHEN-SEQUENCE
                                   PIC 9(7).
       
      **** Only certain EIN Prefix values are assigned:
      **** 'https://www.irs.gov/businesses/...
      **** small-businesses-self-employed/...
      **** how-eins-are-assigned-and-valid-ein-prefixes'.
       
       01  EIN-PREFIXES.
           05  EIN-PREFIX-CNT      PIC S9(4)       COMP VALUE 83.
           05  EIN-PREFIX-OCCS.
               10  FILLER          PIC 9(2)        VALUE 01.
               10  FILLER          PIC 9(2)        VALUE 02.
               10  FILLER          PIC 9(2)        VALUE 03.
               10  FILLER          PIC 9(2)        VALUE 04.
               10  FILLER          PIC 9(2)        VALUE 05.
               10  FILLER          PIC 9(2)        VALUE 06.
               10  FILLER          PIC 9(2)        VALUE 10.
               10  FILLER          PIC 9(2)        VALUE 11.
               10  FILLER          PIC 9(2)        VALUE 12.
               10  FILLER          PIC 9(2)        VALUE 13.
               10  FILLER          PIC 9(2)        VALUE 14.
               10  FILLER          PIC 9(2)        VALUE 15.
               10  FILLER          PIC 9(2)        VALUE 16.
               10  FILLER          PIC 9(2)        VALUE 20.
               10  FILLER          PIC 9(2)        VALUE 21.
               10  FILLER          PIC 9(2)        VALUE 22.
               10  FILLER          PIC 9(2)        VALUE 23.
               10  FILLER          PIC 9(2)        VALUE 24.
               10  FILLER          PIC 9(2)        VALUE 25.
               10  FILLER          PIC 9(2)        VALUE 26.
               10  FILLER          PIC 9(2)        VALUE 27.
               10  FILLER          PIC 9(2)        VALUE 30.
               10  FILLER          PIC 9(2)        VALUE 31.
               10  FILLER          PIC 9(2)        VALUE 32.
               10  FILLER          PIC 9(2)        VALUE 33.
               10  FILLER          PIC 9(2)        VALUE 34.
               10  FILLER          PIC 9(2)        VALUE 35.
               10  FILLER          PIC 9(2)        VALUE 36.
               10  FILLER          PIC 9(2)        VALUE 37.
               10  FILLER          PIC 9(2)        VALUE 38.
               10  FILLER          PIC 9(2)        VALUE 39.
               10  FILLER          PIC 9(2)        VALUE 40.
               10  FILLER          PIC 9(2)        VALUE 41.
               10  FILLER          PIC 9(2)        VALUE 42.
               10  FILLER          PIC 9(2)        VALUE 43.
               10  FILLER          PIC 9(2)        VALUE 44.
               10  FILLER          PIC 9(2)        VALUE 45.
               10  FILLER          PIC 9(2)        VALUE 46.
               10  FILLER          PIC 9(2)        VALUE 47.
               10  FILLER          PIC 9(2)        VALUE 48.
               10  FILLER          PIC 9(2)        VALUE 50.
               10  FILLER          PIC 9(2)        VALUE 51.
               10  FILLER          PIC 9(2)        VALUE 52.
               10  FILLER          PIC 9(2)        VALUE 53.
               10  FILLER          PIC 9(2)        VALUE 54.
               10  FILLER          PIC 9(2)        VALUE 55.
               10  FILLER          PIC 9(2)        VALUE 56.
               10  FILLER          PIC 9(2)        VALUE 57.
               10  FILLER          PIC 9(2)        VALUE 58.
               10  FILLER          PIC 9(2)        VALUE 59.
               10  FILLER          PIC 9(2)        VALUE 60.
               10  FILLER          PIC 9(2)        VALUE 61.
               10  FILLER          PIC 9(2)        VALUE 62.
               10  FILLER          PIC 9(2)        VALUE 63.
               10  FILLER          PIC 9(2)        VALUE 64.
               10  FILLER          PIC 9(2)        VALUE 65.
               10  FILLER          PIC 9(2)        VALUE 66.
               10  FILLER          PIC 9(2)        VALUE 67.
               10  FILLER          PIC 9(2)        VALUE 68.
               10  FILLER          PIC 9(2)        VALUE 71.
               10  FILLER          PIC 9(2)        VALUE 72.
               10  FILLER          PIC 9(2)        VALUE 73.
               10  FILLER          PIC 9(2)        VALUE 74.
               10  FILLER          PIC 9(2)        VALUE 75.
               10  FILLER          PIC 9(2)        VALUE 76.
               10  FILLER          PIC 9(2)        VALUE 77.
               10  FILLER          PIC 9(2)        VALUE 80.
               10  FILLER          PIC 9(2)        VALUE 81.
               10  FILLER          PIC 9(2)        VALUE 82.
               10  FILLER          PIC 9(2)        VALUE 83.
               10  FILLER          PIC 9(2)        VALUE 84.
               10  FILLER          PIC 9(2)        VALUE 85.
               10  FILLER          PIC 9(2)        VALUE 86.
               10  FILLER          PIC 9(2)        VALUE 87.
               10  FILLER          PIC 9(2)        VALUE 88.
               10  FILLER          PIC 9(2)        VALUE 90.
               10  FILLER          PIC 9(2)        VALUE 91.
               10  FILLER          PIC 9(2)        VALUE 92.
               10  FILLER          PIC 9(2)        VALUE 93.
               10  FILLER          PIC 9(2)        VALUE 94.
               10  FILLER          PIC 9(2)        VALUE 95.
               10  FILLER          PIC 9(2)        VALUE 98.
               10  FILLER          PIC 9(2)        VALUE 99.
           05  FILLER REDEFINES EIN-PREFIX-OCCS.
               10  FILLER                          OCCURS 83
                                                   INDEXED EP-DX.
                   15  EIN-PREFIX  PIC 9(2).
       
       01  EIN-SEQUENCE.
           05  EIN-SEQUENCE-MIN    PIC 9(7)        VALUE 0000000.
           05  EIN-SEQUENCE-MAX    PIC 9(7)        VALUE 9999999.
       
      **** A United States Individual Taxpayer Identification Number
      **** (ITIN) is a tax processing number issued by the Internal
      **** Revenue Service. It is a nine-digit number that always begins
      **** with the number 9 and has a range of 70-88 in the fourth and
      **** fifth digit. Effective April 12, 2011, the range was extended
      **** to include 900-70-0000 through 999-88-9999, 900-90-0000
      **** through 999-92-9999 and 900-94-0000 through 999-99-9999.
      
      **** ITINs follow a format of a three digit area, a hyphen, a two
      **** digit group, a hyphen, and a four digit serial: ###-##-####.
      
      **** 'https://www.irs.gov/individuals/international-taxpayers/...
      **** general-itin-information'.
       
       01  FORMAT-ITIN.
           05  FORMAT-ITIN-AREA    PIC 9(3).
           05  FORMAT-ITIN-GROUP   PIC 9(2).
           05  FORMAT-ITIN-SERIAL  PIC 9(4).
       
       01  FORMAT-ITIN-HYPHEN.
           05  FORMAT-ITIN-HYPHEN-AREA
                                   PIC 9(3).
           05  FILLER              PIC X           VALUE '-'.
           05  FORMAT-ITIN-HYPHEN-GROUP
                                   PIC 9(2).
           05  FILLER              PIC X           VALUE '-'.
           05  FORMAT-ITIN-HYPHEN-SERIAL
                                   PIC 9(4).
       
       01  ITIN-AREA.
           05  ITIN-AREA-MIN       PIC 9(3)        VALUE 900.
           05  ITIN-AREA-MAX       PIC 9(3)        VALUE 999.

       01  ITIN-GROUPS.
           05  ITIN-GROUP-CNT      PIC S9(4)       COMP VALUE 28.
           05  ITIN-GROUP-OCCS.
               10  FILLER          PIC 9(2)        VALUE 70.
               10  FILLER          PIC 9(2)        VALUE 71.
               10  FILLER          PIC 9(2)        VALUE 72.
               10  FILLER          PIC 9(2)        VALUE 73.
               10  FILLER          PIC 9(2)        VALUE 74.
               10  FILLER          PIC 9(2)        VALUE 75.
               10  FILLER          PIC 9(2)        VALUE 76.
               10  FILLER          PIC 9(2)        VALUE 77.
               10  FILLER          PIC 9(2)        VALUE 78.
               10  FILLER          PIC 9(2)        VALUE 79.
               10  FILLER          PIC 9(2)        VALUE 80.
               10  FILLER          PIC 9(2)        VALUE 81.
               10  FILLER          PIC 9(2)        VALUE 82.
               10  FILLER          PIC 9(2)        VALUE 83.
               10  FILLER          PIC 9(2)        VALUE 84.
               10  FILLER          PIC 9(2)        VALUE 85.
               10  FILLER          PIC 9(2)        VALUE 86.
               10  FILLER          PIC 9(2)        VALUE 87.
               10  FILLER          PIC 9(2)        VALUE 88.
               10  FILLER          PIC 9(2)        VALUE 90.
               10  FILLER          PIC 9(2)        VALUE 91.
               10  FILLER          PIC 9(2)        VALUE 92.
               10  FILLER          PIC 9(2)        VALUE 94.
               10  FILLER          PIC 9(2)        VALUE 95.
               10  FILLER          PIC 9(2)        VALUE 96.
               10  FILLER          PIC 9(2)        VALUE 97.
               10  FILLER          PIC 9(2)        VALUE 98.
               10  FILLER          PIC 9(2)        VALUE 99.
           05  FILLER REDEFINES ITIN-GROUP-OCCS.
               10  FILLER                          OCCURS 28
                                                   INDEXED IG-DX.
                   15  ITIN-GROUP  PIC 9(2).
       
       01  ITIN-SERIAL.
           05  ITIN-SERIAL-MIN     PIC 9(4)        VALUE 0001.
           05  ITIN-SERIAL-MAX     PIC 9(4)        VALUE 9999.
       
      **** A United States Social Security Number (SSN) is a nine-digit
      **** number that also follows a format of a three digit area,
      **** a hyphen, a two digit group, a hyphen, and a four digit 
      **** serial: ###-##-####.
       
      **** Invalid SSNs have the following characteristics:
      ****   Cannot begin with the number 9
      ****   Cannot begin with 666 in positions 1 - 3
      ****   Cannot begin with 000 in positions 1 - 3
      ****   Cannot contain 00 in positions 4 - 5
      ****   Cannot contain 0000 in positions 6 - 9
       
      **** See https://www.ssa.gov/kc/SSAFactSheet--IssuingSSNs.pdf
       
       01  FORMAT-SSN.
           05  FORMAT-SSN-AREA     PIC 9(3).
           05  FORMAT-SSN-GROUP    PIC 9(2).
           05  FORMAT-SSN-SERIAL   PIC 9(4).
       
       01  FORMAT-SSN-HYPHEN.
           05  FORMAT-SSN-HYPHEN-AREA
                                   PIC 9(3).
           05  FILLER              PIC X           VALUE '-'.
           05  FORMAT-SSN-HYPHEN-GROUP
                                   PIC 9(2).
           05  FILLER              PIC X           VALUE '-'.
           05  FORMAT-SSN-HYPHEN-SERIAL
                                   PIC 9(4).
       
       01  SSN-AREA.
           05  SSN-AREA-MIN        PIC 9(3)        VALUE 001.
           05  SSN-AREA-MAX        PIC 9(3)        VALUE 899.
           05  SSN-AREA-EXCL       PIC 9(3)        VALUE 666.
       
       01  SSN-GROUP.
           05  SSN-GROUP-MIN       PIC 9(2)        VALUE 01.
           05  SSN-GROUP-MAX       PIC 9(2)        VALUE 99.
       
       01  SSN-SERIAL.
           05  SSN-SERIAL-MIN      PIC 9(4)        VALUE 0001.
           05  SSN-SERIAL-MAX      PIC 9(4)        VALUE 9999.
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

           DISPLAY 'FAKTXID  compiled on '
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
             WHEN TAXID-EIN
               PERFORM SUB-9010-EIN THRU SUB-9010-EXIT

               MOVE FORMAT-EIN     TO FAKER-RESULT

             WHEN TAXID-EIN-HYPHEN
               PERFORM SUB-9010-EIN THRU SUB-9010-EXIT

               MOVE FORMAT-EIN-HYPHEN
                                   TO FAKER-RESULT

             WHEN TAXID-ITIN
               PERFORM SUB-9020-ITIN THRU SUB-9020-EXIT

               MOVE FORMAT-ITIN    TO FAKER-RESULT

             WHEN TAXID-ITIN-HYPHEN
               PERFORM SUB-9020-ITIN THRU SUB-9020-EXIT

               MOVE FORMAT-ITIN-HYPHEN
                                   TO FAKER-RESULT

             WHEN TAXID-SSN
               PERFORM SUB-9030-SSN THRU SUB-9030-EXIT

               MOVE FORMAT-SSN     TO FAKER-RESULT

             WHEN TAXID-SSN-HYPHEN
               PERFORM SUB-9030-SSN THRU SUB-9030-EXIT

               MOVE FORMAT-SSN-HYPHEN
                                   TO FAKER-RESULT

             WHEN OTHER
               SET  FAKER-UNKNOWN-FUNCTION
                                   TO TRUE
               STRING 'Unknown FAKTXID function "'
                       FAKER-PROVIDER-FUNCTION
                       '"'  DELIMITED SIZE
                                 INTO FAKER-RESPONSE-MSG
               GO TO SUB-2000-EXIT
           END-EVALUATE

           SET  FAKER-INFO-CNT     TO FI-DX 
           .
       SUB-2000-EXIT.
           EXIT.
      /
       SUB-3000-SHUT-DOWN.
      *-------------------

      D    IF      FAKER-RESPONSE-GOOD
      D        DISPLAY 'FAKTXID completed successfully'
      D    ELSE
      D        DISPLAY 'FAKTXID ended with error '
      D                FAKER-RESPONSE-CODE
      D                ': '
      D                FAKER-RESPONSE-MSG
      D    END-IF
           .
       SUB-3000-EXIT.
           EXIT.
      /
       SUB-9010-EIN.
      *-------------

      **** EIN PREFIX:

           PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

           COMPUTE W-RANDOM-SUB    =  FAKRAND-RANDOM-NO
                                      * EIN-PREFIX-CNT
                                      + 1

           SET  FI-DX           UP BY 1
           MOVE 'EIN-PREFIXES'     TO FAKER-TABLE(FI-DX)
           MOVE FAKRAND-RANDOM-NO  TO FAKER-RANDOM-NO-SUB(FI-DX)
           MOVE W-RANDOM-SUB       TO FAKER-TABLE-ENTRY(FI-DX)

           MOVE EIN-PREFIX(W-RANDOM-SUB)
                                   TO FORMAT-EIN-PREFIX
                                      FORMAT-EIN-HYPHEN-PREFIX

      **** EIN SEQUENCE:

           PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

           COMPUTE W-EIN-SEQUENCE  =  EIN-SEQUENCE-MAX 
                                      - EIN-SEQUENCE-MIN
                                      + 1

           COMPUTE W-EIN-SEQUENCE  =  FAKRAND-RANDOM-NO
                                      * W-EIN-SEQUENCE
                                      + EIN-SEQUENCE-MIN  

           SET  FI-DX           UP BY 1
           MOVE 'EIN-SEQUENCE'     TO FAKER-TABLE(FI-DX)
           MOVE FAKRAND-RANDOM-NO  TO FAKER-RANDOM-NO-SUB(FI-DX)

           MOVE W-EIN-SEQUENCE     TO FORMAT-EIN-SEQUENCE
                                      FORMAT-EIN-HYPHEN-SEQUENCE
           .
       SUB-9010-EXIT.
           EXIT.
      /
       SUB-9020-ITIN.
      *--------------

      **** ITIN AREA:

           PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

           COMPUTE W-AREA          =  ITIN-AREA-MAX 
                                      - ITIN-AREA-MIN
                                      + 1

           COMPUTE W-AREA          =  FAKRAND-RANDOM-NO
                                      * W-AREA
                                      + ITIN-AREA-MIN  

           SET  FI-DX           UP BY 1
           MOVE 'ITIN-AREA'        TO FAKER-TABLE(FI-DX)
           MOVE FAKRAND-RANDOM-NO  TO FAKER-RANDOM-NO-SUB(FI-DX)

           MOVE W-AREA             TO FORMAT-ITIN-AREA
                                      FORMAT-ITIN-HYPHEN-AREA

      **** ITIN GROUP:

           PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

           COMPUTE W-RANDOM-SUB    =  FAKRAND-RANDOM-NO
                                      * ITIN-GROUP-CNT
                                      + 1

           SET  FI-DX           UP BY 1
           MOVE 'ITIN-GROUPS'      TO FAKER-TABLE(FI-DX)
           MOVE FAKRAND-RANDOM-NO  TO FAKER-RANDOM-NO-SUB(FI-DX)
           MOVE W-RANDOM-SUB       TO FAKER-TABLE-ENTRY(FI-DX)

           MOVE ITIN-GROUP(W-RANDOM-SUB)
                                   TO FORMAT-ITIN-GROUP
                                      FORMAT-ITIN-HYPHEN-GROUP

      **** ITIN SERIAL:

           PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

           COMPUTE W-SERIAL        =  ITIN-SERIAL-MAX 
                                      - ITIN-SERIAL-MIN
                                      + 1
 
           COMPUTE W-SERIAL        =  FAKRAND-RANDOM-NO
                                      * W-SERIAL
                                      + ITIN-SERIAL-MIN  

           SET  FI-DX           UP BY 1
           MOVE 'ITIN-SERIAL'      TO FAKER-TABLE(FI-DX)
           MOVE FAKRAND-RANDOM-NO  TO FAKER-RANDOM-NO-SUB(FI-DX)

           MOVE W-SERIAL           TO FORMAT-ITIN-SERIAL
                                      FORMAT-ITIN-HYPHEN-SERIAL
           .
       SUB-9020-EXIT.
           EXIT.
      /
       SUB-9030-SSN.
      *-------------

      **** SSN AREA:

           MOVE SSN-AREA-EXCL      TO W-AREA

           PERFORM UNTIL W-AREA NOT = SSN-AREA-EXCL
               PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

               COMPUTE W-AREA      =  SSN-AREA-MAX 
                                      - SSN-AREA-MIN
                                      + 1

               COMPUTE W-AREA      =  FAKRAND-RANDOM-NO
                                      * W-AREA
                                      + SSN-AREA-MIN
           END-PERFORM  

           SET  FI-DX           UP BY 1
           MOVE 'SSN-AREA'         TO FAKER-TABLE(FI-DX)
           MOVE FAKRAND-RANDOM-NO  TO FAKER-RANDOM-NO-SUB(FI-DX)

           MOVE W-AREA             TO FORMAT-SSN-AREA
                                      FORMAT-SSN-HYPHEN-AREA

      **** SSN GROUP:

           PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

           COMPUTE W-GROUP         =  SSN-GROUP-MAX 
                                      - SSN-GROUP-MIN
                                      + 1

           COMPUTE W-GROUP         =  FAKRAND-RANDOM-NO
                                      * W-GROUP
                                      + SSN-GROUP-MIN  

           SET  FI-DX           UP BY 1
           MOVE 'SSN-GROUP'        TO FAKER-TABLE(FI-DX)
           MOVE FAKRAND-RANDOM-NO  TO FAKER-RANDOM-NO-SUB(FI-DX)

           MOVE W-GROUP            TO FORMAT-SSN-GROUP
                                      FORMAT-SSN-HYPHEN-GROUP

      **** SSN SERIAL:

           PERFORM SUB-9901-CALL-FAKRAND THRU SUB-9901-EXIT

           COMPUTE W-SERIAL        =  SSN-SERIAL-MAX 
                                      - SSN-SERIAL-MIN
                                      + 1

           COMPUTE W-SERIAL        =  FAKRAND-RANDOM-NO
                                      * W-SERIAL
                                      + SSN-SERIAL-MIN  

           SET  FI-DX           UP BY 1
           MOVE 'SSN-SERIAL'      TO FAKER-TABLE(FI-DX)
           MOVE FAKRAND-RANDOM-NO  TO FAKER-RANDOM-NO-SUB(FI-DX)

           MOVE W-SERIAL           TO FORMAT-SSN-SERIAL
                                      FORMAT-SSN-HYPHEN-SERIAL
           .
       SUB-9030-EXIT.
           EXIT.
      /
       SUB-9901-CALL-FAKRAND.
      *----------------------

           CALL W-FAKRAND-PROG  USING W-FAKRAND-PARAMETER 
           .
       SUB-9901-EXIT.
           EXIT.
