       ID DIVISION.
       PROGRAM-ID. AESXGET.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *     INPUT FILE (AESDAT - LRECL=32)
            SELECT R-DAT ASSIGN TO 'AESIDAT'
                   ORGANIZATION IS SEQUENTIAL
                   ACCESS MODE  IS SEQUENTIAL
                   FILE STATUS  IS FS-DAT.

       DATA DIVISION.
       FILE SECTION.
      *   AESIDAT
       FD R-DAT LABEL RECORD STANDARD.

       01 DAT.
          02 INPUT-DATA.   
             03 IDT OCCURS 32.
                04 ID-ITEM             PIC X(1).


       WORKING-STORAGE SECTION.
         01 FS.
            05 FS-DAT                  PIC 9(02).
               88 FS-DAT-OK                      VALUE 0.
               88 FS-DAT-EOF                     VALUE 10.
               88 FS-DAT-AOF                     VALUE 41.

         01 PMW.
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
             03 ISOPEN               PIC X(01).
             03 FILLER               PIC X(1023).


       LINKAGE SECTION.
         01 LS.
      *     PUT-MESSAGE AREA
            02 PUT-MESSAGE-LS          PIC X(2178).
      *     TEXT-GET AREA
            02 PUT-MESSAGE             PIC X(8).
            02 LXG-TEXT                PIC X(32).
            02 LXG-TLENGTH             PIC 9(02).
            02 LXG-STATUS              PIC X(3).
               88 LXGS-OK                       VALUE 'OK '.
               88 LXGS-ERR                      VALUE 'ERR'.
               88 LXGS-EOF                      VALUE 'EOF'.
               88 LXGS-SKP                      VALUE 'SKP'.
            02 LXG-INPUT.
               03 LXG-CFILE            PIC X(1).
               03 FILLER               PIC X(1023).
            02 LXG-OUTPUT.
               03 LXG-ISOPEN           PIC X(1).
               03 FILLER               PIC X(1023).

      
       PROCEDURE DIVISION USING LS.
       MAINLINE.
            MOVE PUT-MESSAGE-LS             TO PMW.
            MOVE 'OK '                      TO LXG-STATUS.

            IF LXG-CFILE = SPACES OR LOW-VALUES
              IF LXG-ISOPEN = SPACES OR LOW-VALUES
      *         File is not opened, open it
                MOVE 'OPENING AESIDAT FILE' TO LTEXT OF PMW
                SET LLEVEL-INF OF PMW       TO TRUE

                CALL PUT-MESSAGE USING PMW
                PERFORM OPEN-FILE
              END-IF

      *       Read next reg of file and move it to LS
              MOVE 'READING AESIDAT FILE'   TO LTEXT OF PMW
              SET LLEVEL-INF OF PMW         TO TRUE
 
              CALL PUT-MESSAGE USING PMW
              PERFORM READ-FILE
            ELSE
      *       Request to close the file
              MOVE 'CLOSING AESIDATA FILE'  TO LTEXT OF PMW
              SET LLEVEL-INF OF PMW         TO TRUE
  
              CALL PUT-MESSAGE USING PMW
              PERFORM CLOSE-FILE
            END-IF.

            IF FS-DAT-EOF
              MOVE 'EOF'                    TO LXG-STATUS
            END-IF

            GOBACK.


       OPEN-FILE.
            OPEN INPUT R-DAT.

            IF FS-DAT-OK OR FS-DAT-AOF
              MOVE 'O'                      TO ISOPEN
            ELSE
              STRING 'ERROR OPENING AESIDAT FILE ' 
                     FS-DAT
              DELIMITED BY SIZE           INTO LTEXT OF PMW
              SET LLEVEL-ERR                TO TRUE

              CALL PUT-MESSAGE USING PMW
              PERFORM END-ON-ERROR
            END-IF.


       READ-FILE.
            READ R-DAT.

            EVALUATE TRUE
              WHEN FS-DAT-OK 
                CONTINUE
              WHEN FS-DAT-EOF
                MOVE 'END-OF-FILE(AESIDAT)' TO LTEXT OF PMW
                SET LLEVEL-INF              TO TRUE

                CALL PUT-MESSAGE USING PMW
              WHEN OTHER
                STRING 'ERROR READING AESIDAT FILE '
                       FS-DAT
                DELIMITED BY SIZE         INTO LTEXT OF PMW
                SET LLEVEL-ERR              TO TRUE

                CALL PUT-MESSAGE USING PMW
                PERFORM END-ON-ERROR
            END-EVALUATE.

            PERFORM MOVE-TO-LS.


       CLOSE-FILE.
            CLOSE R-DAT.

            IF FS-DAT-OK
              CONTINUE
            ELSE
              STRING 'ERROR CLOSING AESIDAT FILE '
                     FS-DAT
              DELIMITED BY SIZE           INTO LTEXT OF PMW
              SET LLEVEL-ERR OF PMW         TO TRUE

              CALL PUT-MESSAGE USING PMW
              PERFORM END-ON-ERROR
            END-IF.


       MOVE-TO-LS.
            MOVE DAT                        TO LXG-TEXT.
            MOVE 32                         TO LXG-TLENGTH.


       END-ON-ERROR.
         MOVE 'ERR'                         TO LXG-STATUS.
         GOBACK.
