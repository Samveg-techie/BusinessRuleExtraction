      *****************************************************************
      *                                                               *
      * This software have been developed under GNU GPL v3 License.   *
      *   That means, no closed distribution of this software is      *
      *   allowed.                                                    *
      *                                                               *
      * Please refer to the License text here:                        *
      *   https://www.gnu.org/licenses/gpl-3.0.txt                    *
      *                                                               *
      * Advanced Encription Standard (AES) COBOL Implementation.      *
      *                                                               *
      * This code was written by David O. Solé González (aka DoHITB). *
      *                                                               *
      * The implementation was made following FIPS-197 official text. *
      * You can find the mentioned rfc on the Internet for free, on   *
      * https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf      *
      *                                                               *
      * For any comment, suggestion or similar, you can reach me via  *
      * mail on "doscar.sole@gmail.com"                               *
      *                                                               *
      * Galois Field arithmetic was bypassed by static tables for     *
      * best performance.                                             *
      *                                                               *
      * How it works: https://github.com/DoHITB/CryptoCobol           *
      *****************************************************************
       ID DIVISION.
       PROGRAM-ID. AESMAIN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
 
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *  Interface raw linkage with main variables defined
         01 DG-S.
            02 PUT-MESSAGE-LS          PIC X(2178).
            02 PUT-MESSAGE             PIC X(8).
            02 LDG-TEXT                PIC X(2860).
            02 LDG-STATUS              PIC X(3).
               88 LDGS-OK                       VALUE 'OK '.
               88 LDGS-ERR                      VALUE 'ERR'.
               88 LDGS-EOF                      VALUE 'EOF'.
            02 LDG-INPUT.
               03 LDGS-CFILE           PIC X(1).
               03 FILLER               PIC X(1023).
            02 LDG-OUTPUT.
               03 FILLER               PIC X(1024).

         01 TG-S.
            02 PUT-MESSAGE-LS          PIC X(2178).
            02 PUT-MESSAGE             PIC X(8).
            02 LXG-TEXT                PIC X(32).
            02 LXG-TLENGTH             PIC 9(02).
            02 LXG-STATUS              PIC X(3).
               88 LXGS-OK                       VALUE 'OK '.
               88 LXGS-ERR                      VALUE 'ERR'.
               88 LXGS-EOF                      VALUE 'EOF'.
               88 LXGS-SKP                      VALUE 'SKP'.
            02 LXG-INPUT.
               03 LXGS-CFILE           PIC X(1).
               03 FILLER               PIC X(1023).
            02 LXG-OUTPUT.
               03 FILLER               PIC X(1024).

         01 TP-S.
            02 PUT-MESSAGE-LS          PIC X(2178).
            02 PUT-MESSAGE             PIC X(8).
            02 LXP-TEXT                PIC X(32).
            02 LXP-TLENGTH             PIC 9(2).
            02 LXP-STATUS              PIC X(3).
               88 LXPS-OK                       VALUE 'OK '.
               88 LXPS-ERR                      VALUE 'ERR'.
            02 LXP-INPUT.
               03 LXPS-CFILE           PIC X(1).
               03 FILLER               PIC X(1023).
            02 LXP-OUTPUT.
               03 FILLER               PIC X(1024).

         01 PM-S.
            02 LMESSAGE-LEVEL          PIC 9(01).
               88 LLEVEL-INF                     VALUE 0.
               88 LLEVEL-WAR                     VALUE 1.
               88 LLEVEL-ERR                     VALUE 2.
               88 LLEVEL-MAX                     VALUE 9.
            02 LLOG-LEVEL              PIC 9(01).
               88 LLOG-INF                       VALUE 0.
               88 LLOG-WAR                       VALUE 1.
               88 LLOG-ERR                       VALUE 2.
               88 LLOG-MAX                       VALUE 9.
            02 LTEXT                   PIC X(128).
            02 EXTRA-IN.
               03 CFILE                PIC X(01).
               03 FILLER               PIC X(1023).
            02 EXTRA-OUT.
               03 FILLER               PIC X(1024).


      ******************************************
      * AESCORE COMMUNICATION AREA             *
      ******************************************
         01 IOCOMM.
            02 IO-ACTION               PIC X(1).
            02 IO-MODE                 PIC X(3).
            02 IO-BITS                 PIC X(3).
            02 IO-TEXT                 PIC X(32).
            02 IO-KEY                  PIC X(64).
            02 IO-KSCH                 PIC X(480).
         01 IOTAB.
           02 M-BOX.
             05 MB-A OCCURS 4.
                10 MB-B OCCURS 256.
                   15 MB-R              PIC X(02).
           02 X-BOX.
             05 XB-A OCCURS 16.
                10 XB-B OCCURS 16.
                   15 XB-R              PIC X(01).
           02 S-BOX.
             05 SB-A OCCURS 16.
                10 SB-BX.
                   15 SB-B OCCURS 16.
                      20 SB-R           PIC X(02).
           02 M-MAP.
             05 MMA OCCURS 4.
                10 MMR                  PIC X(01).

         01 IOPUTM                     PIC X(2178).


      *  Main Interface values
         77 WS-DATA-GET                PIC X(08).
         77 WS-TEXT-GET                PIC X(08).
         77 WS-TEXT-PUT                PIC X(08).
         77 WS-PUT-MESSAGE             PIC X(08).
         77 WS-ACTION                  PIC X(08).
         77 WS-MODE                    PIC X(03).
         77 WS-BITS                    PIC X(03).
         77 WS-CIPHER-DATA             PIC X(2860).
         77 WS-DECIPHER-DATA           PIC X(2860).

      *  CTR Mode specific data
         01 COUNTER.
            02 WS-CTR.
               03 WS-C OCCURS 32       PIC X(01).
            02 WS-CARRY                PIC X(01).
               88 SW-CARRY-T                     VALUE 'Y'.
               88 SW-CARRY-F                     VALUE 'N'.
            02 HA-I                    PIC 9(02).

      *  XOR structure
         01 XOR-DATA.
            02 XOR-A.
               03 XOR-AP OCCURS 16     PIC X(02).
            02 XOR-B.
               03 XOR-BP OCCURS 16     PIC X(02).
            02 XOR-R.
               03 XOR-RP OCCURS 16     PIC X(02).
            02 XOR-I                   PIC 9(02).

      *  Padding structure
         01 PAD-DATA.
            02 PAD-X                   PIC X(32).

      *  XOR computing
         01 SUMM.
            05 XB-X                     PIC 9(2).
            05 XB-Y                     PIC 9(2).
            05 BYTE-A.
               10 BYTES-A OCCURS 2.
                  15 WORD-A             PIC X(1).
            05 BYTE-B.
               10 BYTES-B OCCURS 2.
                  15 WORD-B             PIC X(1).
            05 BYTE-R.
               10 BYTES-R OCCURS 2.
                  15 WORD-R             PIC X(1).

      *  Data conversion
         01 W2D-DATA.
            05 W2D-A                    PIC X(1).
            05 W2D-D                    PIC 9(2).
            05 W2D-R                    PIC 9(2).

       LINKAGE SECTION.
         01 LS.
          02 LMAIN-DATA-GET              PIC X(08).
          02 LMAIN-TEXT-GET              PIC X(08).
          02 LMAIN-TEXT-PUT              PIC X(08).
          02 LMAIN-PUT-MESSAGE           PIC X(08).
          02 LMAIN-ACTION                PIC X(08).
          02 LMAIN-MODE                  PIC X(03).
          02 LMAIN-BITS                  PIC X(03).
          02 LMAIN-KEY                   PIC X(128).
          02 LMAIN-IV                    PIC X(32).
          02 LMAIN-MESSAGE-LEVEL         PIC 9(01).
             88 LMAIN-LEVEL-INF                    VALUE 0.
             88 LMAIN-LEVEL-WAR                    VALUE 1.
             88 LMAIN-LEVEL-ERR                    VALUE 2.
             88 LMAIN-LEVEL-MAX                    VALUE 9.
          02 LMAIN-STATUS                PIC X(03).
             88 LMAINS-OK                          VALUE 'OK '.
             88 LMAINS-ERR                         VALUE 'ERR'.
          02 LMAIN-EXTRA-DG              PIC X(7097).
          02 LMAIN-EXTRA-TG              PIC X(4271).
          02 LMAIN-EXTRA-TP              PIC X(4271).
          02 LMAIN-EXTRA-PM              PIC X(2178).



       PROCEDURE DIVISION USING LS.
      *****************************************************************
      *                          MAINLINE                             *
      *****************************************************************
       MAINLINE.
            SET LMAINS-OK                   TO TRUE.

      *     Move LINKAGE-SECTION variables to WORKING-STORAGE
            MOVE LMAIN-MESSAGE-LEVEL        TO LLOG-LEVEL OF PM-S.

            MOVE LMAIN-EXTRA-DG             TO DG-S.
            MOVE LMAIN-EXTRA-TG             TO TG-S.
            MOVE LMAIN-EXTRA-TP             TO TP-S.
            MOVE LMAIN-EXTRA-PM             TO PM-S.
            MOVE PM-S                       TO PUT-MESSAGE-LS OF DG-S.
            MOVE PM-S                       TO PUT-MESSAGE-LS OF TG-S.
            MOVE PM-S                       TO PUT-MESSAGE-LS OF TP-S.

      *     Perform initial checkings
            PERFORM CHECK-INPUT.

      *     Continue assigning values
            MOVE WS-PUT-MESSAGE             TO PUT-MESSAGE OF DG-S
                                               PUT-MESSAGE OF TG-S
                                               PUT-MESSAGE OF TP-S.

      *     Put starting log
            MOVE 'Starting process'         TO LTEXT OF PM-S.
            SET LLEVEL-INF OF PM-S          TO TRUE

            CALL WS-PUT-MESSAGE USING PM-S.

      *     Do actions until EOF
            PERFORM MAIN-ACTION.

      *     Put ending log
            MOVE 'Ending process'           TO LTEXT OF PM-S.
            SET LLEVEL-INF OF PM-S          TO TRUE.

            CALL WS-PUT-MESSAGE USING PM-S.

      *     Close streams
            PERFORM CLOSE-STREAMS.

            GOBACK.


       CHECK-INPUT.
      *     If no interface name informed, use default
            IF LMAIN-DATA-GET = SPACES OR LOW-VALUES
              MOVE 'AESTGET'                TO WS-DATA-GET
            ELSE
              MOVE LMAIN-DATA-GET           TO WS-DATA-GET
            END-IF.

      *     If no interface name informed, use default
            IF LMAIN-TEXT-GET = SPACES OR LOW-VALUES
              MOVE 'AESXGET'                TO WS-TEXT-GET
            ELSE
              MOVE LMAIN-DATA-GET           TO WS-TEXT-GET
            END-IF.

      *     If no interface name informed, use default
            IF LMAIN-TEXT-PUT = SPACES OR LOW-VALUES
              MOVE 'AESTPUT'                TO WS-TEXT-PUT
            ELSE
              MOVE LMAIN-TEXT-PUT           TO WS-TEXT-PUT
            END-IF.

      *     If no interface name informed, use default
            IF LMAIN-PUT-MESSAGE = SPACES OR LOW-VALUES
              MOVE 'AESMPUT'                TO WS-PUT-MESSAGE
            ELSE
              MOVE LMAIN-PUT-MESSAGE        TO WS-PUT-MESSAGE
            END-IF.

      *     If no mode informed, use ECB as default
            IF LMAIN-MODE = 'ECB' OR 'CBC' OR 'CFB' OR 
                            'OFB' OR 'CTR'
              MOVE LMAIN-MODE               TO WS-MODE
            ELSE 
              MOVE 'ECB'                    TO WS-MODE
            END-IF.

      *     If no action informed, use CIPHER as default
            IF LMAIN-ACTION = 'CIPHER  ' OR 'DECIPHER'
              MOVE LMAIN-ACTION             TO WS-ACTION
            ELSE
              MOVE 'CIPHER  '               TO WS-ACTION
            END-IF.

      *     If no bit lenght informed, use 128 as default
            IF LMAIN-BITS = '128' OR '192' OR '256'
              MOVE LMAIN-BITS               TO WS-BITS
            ELSE
              MOVE '128'                    TO WS-BITS
            END-IF.


       MAIN-ACTION.
      *     Get cipher/decipher data and move to WORKING-STORAGE
            PERFORM GET-DATA.

      *     Basic setting of AESCORE
            MOVE WS-ACTION(1:1)             TO IO-ACTION OF IOCOMM.
            MOVE WS-MODE                    TO IO-MODE OF IOCOMM.
            MOVE WS-BITS                    TO IO-BITS OF IOCOMM.
            MOVE PM-S                       TO IOPUTM.

      *     Get the first chunk of text            
            PERFORM CALL-TEXT-GET.

      *     Initial set-up for each mode
            EVALUATE WS-MODE
              WHEN 'CBC'
      *         Initial move for CBC mode
                MOVE LMAIN-IV               TO XOR-A
              WHEN 'CFB'
      *       Initial move for CFB mode
                MOVE LMAIN-IV               TO IO-TEXT OF IOCOMM
                MOVE LMAIN-KEY              TO IO-KEY OF IOCOMM               
                MOVE WS-CIPHER-DATA         TO IOTAB

      *         Force encoding
                MOVE 'C'                    TO IO-ACTION

                CALL 'AESCORE' USING IOCOMM IOTAB IOPUTM

                MOVE IO-TEXT                TO XOR-A
              WHEN 'OFB'
      *       Initial move for OFB mode
                MOVE LMAIN-IV               TO IO-TEXT OF IOCOMM
              WHEN 'CTR'
      *       Initial move for CTR mode
                MOVE ALL '0'                TO WS-CTR
                MOVE '1'                    TO WS-C(32)
            END-EVALUATE.

      *     Uniti EOF, keep parsing
            PERFORM UNTIL LXGS-EOF OR LXGS-ERR
              IF LXGS-SKP
                CONTINUE
              ELSE
      *         Action, then move result to output
                EVALUATE WS-MODE ALSO WS-ACTION
                  WHEN 'ECB' ALSO 'CIPHER  '
                    PERFORM ECB-CIPHER
                    MOVE IO-TEXT OF IOCOMM  TO LXP-TEXT
                  WHEN 'CBC' ALSO 'CIPHER  '
                    PERFORM CBC-CIPHER
                    MOVE IO-TEXT OF IOCOMM  TO LXP-TEXT
                  WHEN 'CFB' ALSO 'CIPHER  '
                    PERFORM CFB-CIPHER
                    MOVE XOR-R              TO LXP-TEXT
                  WHEN 'OFB' ALSO 'CIPHER  '
                    PERFORM OFB-CIPHER
                    MOVE XOR-R              TO LXP-TEXT
                  WHEN 'CTR' ALSO 'CIPHER  '
                    PERFORM CTR-CIPHER
                    MOVE XOR-R              TO LXP-TEXT
                  WHEN 'ECB' ALSO 'DECIPHER'
                    PERFORM ECB-DECIPHER
                    MOVE IO-TEXT OF IOCOMM  TO LXP-TEXT
                  WHEN 'CBC' ALSO 'DECIPHER'
                    PERFORM CBC-DECIPHER
                    MOVE XOR-R              TO LXP-TEXT
                  WHEN 'CFB' ALSO 'DECIPHER' 
                    PERFORM CFB-DECIPHER
                    MOVE XOR-R              TO LXP-TEXT
                  WHEN 'OFB' ALSO 'DECIPHER'
                    PERFORM OFB-DECIPHER
                    MOVE XOR-R              TO LXP-TEXT
                  WHEN 'CTR' ALSO 'DECIPHER'
                    PERFORM CTR-DECIPHER
                    MOVE XOR-R              TO LXP-TEXT
                END-EVALUATE


      *         Log intermediate results
                STRING 'Result: '
                        LXP-TEXT
                DELIMITED BY SIZE         INTO LTEXT OF PM-S
                SET LLEVEL-INF OF PM-S      TO TRUE

                CALL WS-PUT-MESSAGE USING PM-S

      *         Move result to output
                PERFORM CALL-TEXT-PUT

      *         Post-actions
                EVALUATE WS-MODE ALSO WS-ACTION
                  WHEN 'CFB' ALSO 'CIPHER  '
                    MOVE XOR-R              TO IO-TEXT OF IOCOMM
                    MOVE LMAIN-KEY          TO IO-KEY OF IOCOMM

                    CALL 'AESCORE' USING IOCOMM IOTAB IOPUTM

                    MOVE IO-TEXT OF IOCOMM  TO XOR-A
                  WHEN 'CFB' ALSO 'DECIPHER'
                    PERFORM PAD

                    MOVE PAD-X              TO IO-TEXT OF IOCOMM
                    MOVE LMAIN-KEY          TO IO-KEY OF IOCOMM

      *             Force cipher
                    MOVE 'C'                TO IO-ACTION
                    CALL 'AESCORE' USING IOCOMM IOTAB IOPUTM

                    MOVE IO-TEXT OF IOCOMM  TO XOR-A
                END-EVALUATE
              END-IF

      *       Get next chunk of text
              PERFORM CALL-TEXT-GET
            END-PERFORM.


       GET-DATA.
      *     Call interface and check for errors
            PERFORM CALL-DATA-GET.
            MOVE LDG-TEXT                   TO WS-CIPHER-DATA.

            PERFORM CALL-DATA-GET.
            MOVE LDG-TEXT                   TO WS-DECIPHER-DATA.

      *     Close DATA-GET stream
            MOVE 'Closing DATA-GET'         TO LTEXT OF PM-S.
            SET LLEVEL-INF OF PM-S          TO TRUE
            CALL WS-PUT-MESSAGE USING PM-S.

            MOVE 'C'                        TO LDGS-CFILE OF DG-S.
            PERFORM CALL-DATA-GET.


      *****************************************************************
      *                       CIPHER / DECIPHER                       *
      *****************************************************************
       ECB-CIPHER.
           PERFORM PAD.

           MOVE PAD-X                       TO IO-TEXT OF IOCOMM.
           MOVE LMAIN-KEY                   TO IO-KEY OF IOCOMM.
           MOVE WS-CIPHER-DATA              TO IOTAB.

           CALL 'AESCORE' USING IOCOMM IOTAB IOPUTM.


       CBC-CIPHER.
           PERFORM PAD.


           MOVE WS-CIPHER-DATA              TO IOTAB.
           MOVE PAD-X                       TO XOR-B.
           PERFORM XOR.
           MOVE XOR-R                       TO IO-TEXT OF IOCOMM.
           MOVE LMAIN-KEY                   TO IO-KEY OF IOCOMM.

           CALL 'AESCORE' USING IOCOMM IOTAB IOPUTM.

           MOVE IO-TEXT OF IOCOMM           TO XOR-A.


       CFB-CIPHER.
           PERFORM PAD.

           MOVE WS-CIPHER-DATA              TO IOTAB.
           MOVE PAD-X                       TO XOR-B.

           PERFORM XOR.


       OFB-CIPHER.
           MOVE LMAIN-KEY                   TO IO-KEY OF IOCOMM.
           MOVE WS-CIPHER-DATA              TO IOTAB.

           CALL 'AESCORE' USING IOCOMM IOTAB IOPUTM.

           PERFORM PAD.

           MOVE IO-TEXT OF IOCOMM           TO XOR-A.
           MOVE PAD-X                       TO XOR-B.
           PERFORM XOR.


       CTR-CIPHER.       
           MOVE WS-CTR                      TO IO-TEXT OF IOCOMM.
           MOVE LMAIN-KEY                   TO IO-KEY OF IOCOMM.
           MOVE WS-CIPHER-DATA              TO IOTAB.

           CALL 'AESCORE' USING IOCOMM IOTAB IOPUTM.

           PERFORM PAD.

           MOVE IO-TEXT OF IOCOMM           TO XOR-A.
           MOVE PAD-X                       TO XOR-B.
           PERFORM XOR.
           PERFORM HEX-ADDITION.


       ECB-DECIPHER.
           PERFORM PAD.

           MOVE PAD-X                       TO IO-TEXT OF IOCOMM.
           MOVE LMAIN-KEY                   TO IO-KEY OF IOCOMM.
           MOVE WS-DECIPHER-DATA            TO IOTAB.

      *    Get KEY-SCHEDULE
           PERFORM KEY-SCHEDULE.

           CALL 'AESCORE' USING IOCOMM IOTAB IOPUTM.


       CBC-DECIPHER.
           PERFORM ECB-DECIPHER.

           MOVE IO-TEXT OF IOCOMM           TO XOR-B.

           PERFORM XOR.
           PERFORM PAD.

           MOVE PAD-X                       TO XOR-A.


       CFB-DECIPHER.
           PERFORM PAD.

           MOVE WS-CIPHER-DATA              TO IOTAB.
           MOVE PAD-X                       TO XOR-B.

           PERFORM XOR.


       OFB-DECIPHER.
           MOVE LMAIN-KEY                   TO IO-KEY OF IOCOMM.
           MOVE WS-CIPHER-DATA              TO IOTAB.

      *    Force cipher
           MOVE 'C'                         TO IO-ACTION.

           CALL 'AESCORE' USING IOCOMM IOTAB IOPUTM.

           PERFORM PAD.

           MOVE IO-TEXT OF IOCOMM           TO XOR-A.
           MOVE PAD-X                       TO XOR-B.

           PERFORM XOR.


       CTR-DECIPHER.
           MOVE WS-CTR                      TO IO-TEXT OF IOCOMM.
           MOVE LMAIN-KEY                   TO IO-KEY OF IOCOMM.
           MOVE WS-CIPHER-DATA            TO IOTAB.

      *    Force cipher
           MOVE 'C'                         TO IO-ACTION.

           CALL 'AESCORE' USING IOCOMM IOTAB IOPUTM.

           PERFORM PAD.

           MOVE IO-TEXT OF IOCOMM           TO XOR-A.
           MOVE PAD-X                       TO XOR-B.

           PERFORM XOR.
           PERFORM HEX-ADDITION.


      *****************************************************************
      *                       INTERFACE CALLING                       *
      *****************************************************************
       CALL-DATA-GET.
            CALL WS-DATA-GET USING DG-S.

            EVALUATE LDG-STATUS
              WHEN 'OK '
              WHEN 'EOF'
                CONTINUE
              WHEN 'ERR'
                MOVE 'Error on DATA-GET'    TO LTEXT OF PM-S
                SET LLEVEL-MAX OF PM-S      TO TRUE
                CALL WS-PUT-MESSAGE USING PM-S

                PERFORM END-ON-ERROR
            END-EVALUATE.


       CALL-TEXT-GET.
      *     Log action
            MOVE 'Getting text...'          TO LTEXT OF PM-S.
            SET LLEVEL-INF OF PM-S          TO TRUE.
            CALL WS-PUT-MESSAGE USING PM-S.

      *     Call interface and check return
            CALL WS-TEXT-GET USING TG-S.

            EVALUATE LXG-STATUS
              WHEN 'OK '
              WHEN 'SKP'
              WHEN 'EOF'
                CONTINUE
              WHEN 'ERR'
                MOVE 'Error on TEXT-GET'    TO LTEXT OF PM-S
                SET LLEVEL-MAX OF PM-S      TO TRUE

                CALL WS-PUT-MESSAGE USING PM-S

                PERFORM END-ON-ERROR
            END-EVALUATE.


       CALL-TEXT-PUT.
            MOVE 32                         TO LXP-TLENGTH

            CALL WS-TEXT-PUT USING TP-S

            EVALUATE LXP-STATUS
              WHEN 'OK '
                CONTINUE
              WHEN 'ERR'
                MOVE 'Error on TEXT-PUT'    TO LTEXT OF PM-S
                SET LLEVEL-MAX OF PM-S      TO TRUE

                CALL WS-PUT-MESSAGE USING PM-S

                PERFORM END-ON-ERROR
            END-EVALUATE.


      *****************************************************************
      *                       SUPPORT FUNCTIONS                       *
      *****************************************************************      
       XOR.
      *    This XOR version will delegate on XB table from IOTAB
      *    that will have specific data for cipher or deciper
      *
           PERFORM VARYING XOR-I FROM 1 BY 1
             UNTIL XOR-I > 16
             MOVE XOR-AP(XOR-I)             TO BYTE-A OF SUMM
             MOVE XOR-BP(XOR-I)             TO BYTE-B OF SUMM

             PERFORM ADDITION

             MOVE BYTE-R OF SUMM            TO XOR-RP(XOR-I)
           END-PERFORM.


       ADDITION.
           MOVE WORD-A OF SUMM(1)           TO W2D-A.
           PERFORM W2D.
           MOVE W2D-R                       TO XB-X.

           MOVE WORD-B OF SUMM(1)           TO W2D-A. 
           PERFORM W2D.
           MOVE W2D-R                       TO XB-Y.

           ADD 1                            TO XB-X
                                               XB-Y.

           MOVE XB-R(XB-X, XB-Y)            TO WORD-R OF SUMM(1).

           MOVE WORD-A OF SUMM(2)           TO W2D-A.
           PERFORM W2D.
           MOVE W2D-R                       TO XB-X.

           MOVE WORD-B OF SUMM(2)           TO W2D-A.
           PERFORM W2D.
           MOVE W2D-R                       TO XB-Y.

           ADD 1                            TO XB-X
                                               XB-Y.

           MOVE XB-R(XB-X, XB-Y)            TO WORD-R OF SUMM(2).


        W2D.
           IF W2D-A IS NUMERIC
             MOVE W2D-A                     TO W2D-R
           ELSE
             EVALUATE W2D-A
               WHEN 'A'
                 MOVE 10                    TO W2D-R
               WHEN 'B'
                 MOVE 11                    TO W2D-R
               WHEN 'C'
                 MOVE 12                    TO W2D-R
               WHEN 'D' 
                 MOVE 13                    TO W2D-R
               WHEN 'E'
                 MOVE 14                    TO W2D-R
               WHEN 'F'
                 MOVE 15                    TO W2D-R
             END-EVALUATE
           END-IF.


       HEX-ADDITION.
      *    We add 1 to an HEX value, then iterate
      *    thru the structure until the end or no
      *    carry produced
      *
           SET SW-CARRY-T                   TO TRUE

           PERFORM VARYING HA-I FROM 32 BY -1
             UNTIL HA-I < 1 OR
                   SW-CARRY-F
             PERFORM HEX-ADD
           END-PERFORM.


       HEX-ADD.
           IF SW-CARRY-T
             SET SW-CARRY-F                 TO TRUE

             EVALUATE WS-C(HA-I)
               WHEN 'F'
                 MOVE '0'                   TO WS-C(HA-I)
                 SET SW-CARRY-T             TO TRUE
               WHEN 'E'
                 MOVE 'F'                   TO WS-C(HA-I)
               WHEN 'D'
                 MOVE 'E'                   TO WS-C(HA-I)
               WHEN 'C'
                 MOVE 'D'                   TO WS-C(HA-I)
               WHEN 'B'
                 MOVE 'C'                   TO WS-C(HA-I)
               WHEN 'A'
                 MOVE 'B'                   TO WS-C(HA-I)
               WHEN '9'
                 MOVE 'A'                   TO WS-C(HA-I)
               WHEN '8'
                 MOVE '9'                   TO WS-C(HA-I)
               WHEN '7'
                 MOVE '8'                   TO WS-C(HA-I)
               WHEN '6'
                 MOVE '7'                   TO WS-C(HA-I)
               WHEN '5'
                 MOVE '6'                   TO WS-C(HA-I)
               WHEN '4'
                 MOVE '5'                   TO WS-C(HA-I)
               WHEN '3'
                 MOVE '4'                   TO WS-C(HA-I)
               WHEN '2'
                 MOVE '3'                   TO WS-C(HA-I)
               WHEN '1'
                 MOVE '2'                   TO WS-C(HA-I)
               WHEN '0'
                 MOVE '1'                   TO WS-C(HA-I)
             END-EVALUATE
           END-IF.


       KEY-SCHEDULE.
      *    Override data with cipher, then get KEY-SCHEDULE
           MOVE WS-CIPHER-DATA              TO IOTAB.
           MOVE 'K'                         TO IO-ACTION OF IOCOMM
           CALL 'AESCORE' USING IOCOMM IOTAB IOPUTM

      *    Restore data
           MOVE 'D'                         TO IO-ACTION OF IOCOMM
           MOVE WS-DECIPHER-DATA            TO IOTAB.       


       PAD.
      *    Pad input data to X(32) structure
           MOVE ALL '0'                     TO PAD-X.
           MOVE LXG-TEXT(1:LXG-TLENGTH)     
             TO PAD-X(33 - LXG-TLENGTH: LXG-TLENGTH).


       END-ON-ERROR.
            PERFORM CLOSE-STREAMS.
            SET LMAINS-ERR                  TO TRUE.
            GOBACK.


       CLOSE-STREAMS.
            MOVE 'Closing streams'          TO LTEXT OF PM-S.
            SET LLEVEL-INF OF PM-S          TO TRUE.
            CALL WS-PUT-MESSAGE USING PM-S.

      *     AESODAT
            MOVE 'Closing Output'           TO LTEXT OF PM-S.
            SET LLEVEL-INF OF PM-S          TO TRUE.
            CALL WS-PUT-MESSAGE USING PM-S.

            MOVE 'C'                        TO LXPS-CFILE OF TP-S.
            PERFORM CALL-TEXT-PUT.

      *     AESIDAT
            MOVE 'Closing Input'            TO LTEXT OF PM-S.
            SET LLEVEL-INF OF PM-S          TO TRUE.
            CALL WS-PUT-MESSAGE USING PM-S.

            MOVE 'C'                        TO LXGS-CFILE OF TG-S.
            PERFORM CALL-TEXT-GET

      *     AESLOG
            MOVE 'Closing Log'              TO LTEXT OF PM-S.
            SET LLEVEL-INF OF PM-S          TO TRUE.
            CALL WS-PUT-MESSAGE USING PM-S.

            MOVE 'C'                        TO CFILE OF PM-S.
            CALL WS-PUT-MESSAGE USING PM-S.
