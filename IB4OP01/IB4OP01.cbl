        IDENTIFICATION DIVISION.                                        
        PROGRAM-ID. IB4OP01.
        ENVIRONMENT DIVISION.                                           
        CONFIGURATION SECTION.                                          
        SOURCE-COMPUTER. IBM-370.                                       
        OBJECT-COMPUTER. IBM-370.                                       
        INPUT-OUTPUT SECTION.                                           
        FILE-CONTROL.                                                   
            SELECT TRANS-FILE ASSIGN TO UT-S-TRANS                      
                FILE STATUS IS FS-TRANS.                                
            SELECT RPT-FILE ASSIGN TO UT-S-PRINT1                       
                FILE STATUS IS FS-RPT.                                  
            SELECT ERR-FILE ASSIGN TO UT-S-PRINT2                       
                FILE STATUS IS FS-ERR.                                  
        DATA DIVISION.                                                  
        FILE SECTION.                                                   
        FD  TRANS-FILE RECORDING MODE F BLOCK 0 RECORDS.                
            *> COPY TRANS.                                               
        FD  RPT-FILE RECORDING MODE F BLOCK 0 RECORDS.                  
        01  RPT-REC.                                                    
            03  R-CC               PIC X.                               
            03  FILLER             PIC X(132).                          
        FD  ERR-FILE RECORDING MODE F BLOCK 0 RECORDS.                  
        01  ERR-REC.                                                    
            03  E-CC               PIC X.                               
            03  E-TRAN-ID          PIC X(12).                           
            03  FILLER             PIC XX.                              
            03  E-MESSAGE          PIC X(100).                          
            03  FILLER             PIC X(18).                           
                                                                        
        WORKING-STORAGE SECTION.                                        
        77  MAX-LINES           PIC S9999 COMP VALUE +55.               
        77  CUR-LINE-CNT        PIC S9999 COMP VALUE +256.              
        77  ERR-LINE-CNT        PIC S9999 COMP VALUE +256.              
        77  ADV-LINES           PIC S9999 COMP VALUE +1.                
        77  LOGICAL-LINE        PIC S9999 COMP VALUE +1.                
        77  FS-TRANS            PIC 99.                                 
        77  FS-RPT              PIC 99.                                 
        77  FS-ERR              PIC 99.                                 
        77  TRANS-EOF           PIC X VALUE ' '.                        
            88  TRANS-PRESENT     VALUE ' '.                            
            88  NO-MORE-TRANS     VALUE 'Y'.                            
        77  TRANS-STATUS        PIC X.                                  
            88  VALID-INCOMPL     VALUE ' '.                            
            88  VALID-TRANS       VALUE 'V'.                            
            88  TRAN-WARN         VALUE 'W'.                            
            88  TRAN-ERR          VALUE 'E'.                            
        77  ACTION              PIC XXXX.                               
        01  PRT-DTL.                                                    
            03  P-CC            PIC X.                                  
            03  P-TRANS         PIC X(5).                               
            03  FILLER          PIC X.                                  
            03  P-ORD-NUM       PIC X(6).                               
            03  FILLER          PIC X.                                  
            03  P-ACT           PIC X(10).                              
            03  FILLER          PIC X.                                  
            03  P-F1-DTL.                                               
                05 P-CUST-NO    PIC X(6).                               
                05 FILLER       PIC X.                                  
                05 P-DESCR      PIC X(40).                              
                05 FILLER       PIC X.                                  
                05 P-ORD-QTY    PIC ZZ,ZZ9.                             
                05 P-ORD-QTY-X  REDEFINES P-ORD-QTY PIC X(6).           
                05 FILLER       PIC X.                                  
                05 P-ORD-AMT    PIC ZZ,ZZ9.99-.                         
                05 P-ORD-AMT-X  REDEFINES P-ORD-AMT PIC X(10).          
                05 FILLER       PIC X.                                  
                05 P-TYPE       PIC XX.                                 
                05 FILLER       PIC X.                                  
                05 P-ORD-STAT   PIC 99.                                 
                05 P-ORD-STAT-X REDEFINES P-ORD-STAT PIC XX.            
                05 FILLER       PIC X.                                  
                05 P-PRTY-CD    PIC X.                                  
                05 FILLER       PIC X.                                  
                05 P-ACT-ORD-QTY PIC ZZ,ZZ9.                            
                05 P-ACT-ORD-QTY-X REDEFINES P-ACT-ORD-QTY PIC X(6).    
                05 FILLER       PIC X.                                  
                05 P-TOT-SCRAP  PIC ZZ,ZZ9.                             
                05 P-TOT-SCRAP-X REDEFINES P-TOT-SCRAP PIC X(6).        
                05 FILLER       PIC X(21).                              
            03  P-F2-DTL REDEFINES P-F1-DTL.                            
                05 FILLER       PIC X(7).                               
                05 P-STRT-CAPT  PIC X(8).                               
                05 P-UNIT-STRT  PIC ZZ,ZZ9.                             
                05 P-UNIT-STRT-X REDEFINES P-UNIT-STRT PIC X(6).        
                05 FILLER       PIC X.                                  
                05 P-PCT-STRT   PIC ZZ,ZZ9.999-.                        
                05 P-PCT-STRT-X REDEFINES P-PCT-STRT PIC X(11).         
                05 P-PCT-CAPT   PIC XX.                                 
                05 P-COMP-CAPT  PIC X(8).                               
                05 P-UNIT-COMP  PIC ZZ,ZZ9.                             
                05 P-UNIT-COMP-X REDEFINES P-UNIT-COMP PIC X(6).        
                05 FILLER       PIC X.                                  
                05 P-PCT-COMP   PIC ZZ,ZZ9.999-.                        
                05 P-PCT-COMP-X REDEFINES P-PCT-COMP PIC X(11).         
                05 P-PCT-CAPT2  PIC X.                                  
                05 FILLER       PIC X(25).                              
                05 P-DATE       PIC X(10).                              
                05 FILLER       PIC X.                                  
                05 P-DATE-L     PIC X(10).                              
        01  ERR-DTL.                                                    
            03  P-CC            PIC X.                                  
            03  P-TRANS         PIC X(5).                               
            03  FILLER          PIC X.                                  
            03  P-ORD-NUM       PIC X(6).                               
            03  FILLER          PIC X.                                  
            03  P-ACT           PIC X(10).                              
            03  FILLER          PIC X.                                  
            03  P-F1-DTL.                                               
                05 P-CUST-NO    PIC X(6).                               
                05 FILLER       PIC X.                                  
                05 P-DESCR      PIC X(40).                              
                05 FILLER       PIC X.                                  
                05 P-ORD-QTY    PIC ZZ,ZZ9.                             
                05 P-ORD-QTY-X  REDEFINES P-ORD-QTY PIC X(6).           
                05 FILLER       PIC X.                                  
                05 P-ORD-AMT    PIC ZZ,ZZ9.99-.                         
                05 P-ORD-AMT-X  REDEFINES P-ORD-AMT PIC X(10).          
                05 FILLER       PIC X.                                  
                05 P-TYPE       PIC XX.                                 
                05 FILLER       PIC X.                                  
                05 P-ORD-STAT   PIC 99.                                 
                05 P-ORD-STAT-X REDEFINES P-ORD-STAT PIC XX.            
                05 FILLER       PIC X.                                  
                05 P-PRTY-CD    PIC X.                                  
                05 FILLER       PIC X.                                  
                05 P-ACT-ORD-QTY PIC ZZ,ZZ9.                            
                05 P-ACT-ORD-QTY-X REDEFINES P-ACT-ORD-QTY PIC X(6).    
                05 FILLER       PIC X.                                  
                05 P-TOT-SCRAP  PIC ZZ,ZZ9.                             
                05 P-TOT-SCRAP-X REDEFINES P-TOT-SCRAP PIC X(6).        
                05 FILLER       PIC X(21).                              
            03  P-F2-DTL REDEFINES P-F1-DTL.                            
                05 FILLER       PIC X(7).                               
                05 P-STRT-CAPT  PIC X(8).                               
                05 P-UNIT-STRT  PIC ZZ,ZZ9.                             
                05 P-UNIT-STRT-X REDEFINES P-UNIT-STRT PIC X(6).        
                05 FILLER       PIC X.                                  
                05 P-PCT-STRT   PIC ZZ,ZZ9.999-.                        
                05 P-PCT-STRT-X REDEFINES P-PCT-STRT PIC X(11).         
                05 P-PCT-CAPT   PIC XX.                                 
                05 P-COMP-CAPT  PIC X(8).                               
                05 P-UNIT-COMP  PIC ZZ,ZZ9.                             
                05 P-UNIT-COMP-X REDEFINES P-UNIT-COMP PIC X(6).        
                05 FILLER       PIC X.                                  
                05 P-PCT-COMP   PIC ZZ,ZZ9.999-.                        
                05 P-PCT-COMP-X REDEFINES P-PCT-COMP PIC X(11).         
                05 P-PCT-CAPT2  PIC X.                                  
                05 FILLER       PIC X(25).                              
                05 P-DATE       PIC X(10).                              
                05 FILLER       PIC X.                                  
                05 P-DATE-L     PIC X(10).                              
        01  H1.                                                         
            03  FILLER             PIC X(45) VALUE IS                   
            '                                             '.            
            03  FILLER             PIC X(50) VALUE IS                   
            ' ORDER PROCESSING                                 '.       
            03  FILLER             PIC X(32) VALUE IS                   
            '                                '.                         
        01  H1-E.                                                       
            03  FILLER             PIC X(40) VALUE IS                   
            '                                        '.                 
            03  FILLER             PIC X(50) VALUE IS                   
            ' ORDER PROCESSING ERRORS                          '.       
            03  FILLER             PIC X(32) VALUE IS                   
            '                                '.                         
        01  H2.                                                         
            03  FILLER             PIC X(50) VALUE IS                   
            ' TRANS  ORDR    ACTION   CUST#  * -------------- '.        
            03  FILLER             PIC X(50) VALUE IS                   
            'DESCRIPTION -------- *   QTY     AMT     TP       '.       
            03  FILLER             PIC X(32) VALUE IS                   
            '            FIRST ACT  LAST ACT '.                         
        01  W-DATE-FMT.                                                 
            03  W-YYYY          PIC X(4).                               
            03  FILLER          PIC X VALUE '/'.                        
            03  W-MM            PIC XX.                                 
            03  FILLER          PIC X VALUE '/'.                        
            03  W-DD            PIC XX.                                 
        01  W-DATE.                                                     
            03  W-YYYY          PIC XXXX.                               
            03  W-MM            PIC XX.                                 
            03  W-DD            PIC XX.                                 
        01  SSA-1.                                                      
            03  S1-SEG-NAME     PIC X(8) VALUE IS 'ORDR010 '.           
            03  FILLER          PIC X VALUE '('.                        
            03  S1-FLD-NAME     PIC X(8) VALUE 'ORDRKEY '.              
            03  S1-SC-OP        PIC XX VALUE ' ='.                      
            03  SSA-1-KEY       PIC X(6).                               
            03  FILLER          PIC XX VALUE ') '.                      
        01  SSA-2.                                                      
            03  S2-SEG-NAME     PIC X(8) VALUE IS 'ORDR010 '.           
            03  FILLER          PIC X VALUE ' '.                        
        01  ORDER-ROOT-DATA.                                             
       05 ORDER-ROOT-KEY.                                           
           07 ORDER-NUMBER-PREFIX      PIC  X(02).                  
           07 ORDER-NUMBER             PIC  9(04).                  
       05  ORDKEY-REDEF  REDEFINES    ORDER-ROOT-KEY                
                                       PIC  X(06).                  
       05 ORDER-DESCRIPTION            PIC  X(40).                  
       05 CUSTOMER-NUMBER              PIC  X(06).                  
       05 PLANNED-ORDER-QUANTITY       PIC  S9(05)       COMP-3.    
       05 PLANNED-ORDER-AMOUNT         PIC  9(05)V99     COMP-3.    
       05 ORDER-TYPE                   PIC  X(02).                  
       05 ACTUAL-ORDER-QUANTITY        PIC  S9(05)       COMP-3.    
       05 TOTAL-SCRAP-QUANTITY         PIC  S9(05)       COMP-3.    
       05 TOTAL-SCRAP-REDEFINES                                     
           REDEFINES TOTAL-SCRAP-QUANTITY PIC  X(03).               
       05 ORDER-STATUS                 PIC  9(02).                  
       05 FILLER                       PIC  X(01).                  
       05 FIRST-ACTIVITY-DATE.                                      
           07 FIRST-ACTIVITY-DATE-YR   PIC  X(04).                  
           07 FIRST-ACTIVITY-DATE-MM   PIC  X(02).                  
           07 FIRST-ACTIVITY-DATE-DD   PIC  X(02).                  
       05 LAST-ACTIVITY-DATE           PIC  X(08).                  
       05 WEEKLY-STATUS-DATA OCCURS       5 TIMES.                  
           07 NUMBER-UNITS-STARTED     PIC  9(05)        COMP-3.    
           07 NUMBER-UNITS-COMPLETED   PIC  9(05)        COMP-3.    
           07 PERCENTAGE-STARTED       PIC  S9(05)V9(03) COMP-3.    
           07 PERCENTAGE-COMPLETE      PIC  S9(05)V9(03) COMP-3.    
       05 PRIORITY-CODE                PIC  X(01).
       01  TRAN-INQY.
           03  TI-TRAN             PIC XX.
      *        A  - ADD ORDER
      *        UB - UPDATE BASE INFO
      *        UA - UPDATE ACTIVITY
      *        D  - DELETE ORDER
      *        Q  - QUERY
           03  TI-ORDR-NO          PIC X(6).
           03  FILLER              PIC X(72).
       01  TRAN-ORD-BASE.
           03  FILLER              PIC X(8).
           03  TB-CUST-NO          PIC X(6).
           03  TB-DESCR            PIC X(40).
           03  TB-PO-QTY           PIC 9(5).
           03  TB-PO-QTY-X REDEFINES TB-PO-QTY PIC X(5).
           03  TB-PO-AMT           PIC 9(5)V99.
           03  TB-PO-AMT-X REDEFINES TB-PO-AMT PIC X(7).
           03  TB-ORDR-TYPE        PIC XX.
           03  TB-PRTY             PIC X.
           03  FILLER              PIC X(11).
       01  TRAN-ACTIVITY.
           03  FILLER              PIC X(8).
           03  TA-ORD-STATUS       PIC 99.
           03  TA-ACT-DATE         PIC X(8).
           03  TA-LAST-ACT-DATE    PIC X(8).
           03  TA-UNITS-STARTED    PIC 9(5).
           03  TA-UNITS-STARTED-X REDEFINES TA-UNITS-STARTED PIC X(5).
           03  TA-UNITS-COMPL      PIC 9(5).
           03  TA-UNITS-COMPL-X REDEFINES TA-UNITS-COMPL PIC X(5).
           03  FILLER              PIC X(44).                                                                  
        LINKAGE SECTION.                                                
        01  DBPCB.                                                      
            02  DBD-NAME        PIC  X(8).                              
            02  SEG-LEVEL       PIC  X(2).                              
            02  DBSTATUS        PIC  X(2).                              
            02  PROC-OPTIONS    PIC  X(4).                              
            02  RESERVE-DLI     PIC  X(4).                              
            02  SEG-NAME-FB     PIC  X(8).                              
            02  LENGTH-FB-KEY   PIC  9(4).                              
            02  NUMB-SENS-SEGS  PIC  9(4).                              
            02  KEY-FB-AREA     PIC  X(17).                             
                                                                        
        01  IOPCB.                                                      
            02  I-DBD-NAME        PIC  X(8).                            
            02  I-SEG-LEVEL       PIC  X(2).                            
            02  I-DBSTATUS        PIC  X(2).                            
            02  I-PROC-OPTIONS    PIC  X(4).                            
            02  I-RESERVE-DLI     PIC  X(4).                            
            02  I-SEG-NAME-FB     PIC  X(8).                            
            02  I-LENGTH-FB-KEY   PIC  9(4).                            
            02  I-NUMB-SENS-SEGS  PIC  9(4).                            
            02  I-KEY-FB-AREA     PIC  X(17).                           
                                                                        
        PROCEDURE DIVISION USING IOPCB DBPCB.                           
        I100-INIT.                                                      
            DISPLAY 'ENTERING PROGRAM IB4OP01'                          
            OPEN INPUT TRANS-FILE, OUTPUT RPT-FILE, ERR-FILE.           
            IF FS-TRANS NOT = 0 OR FS-RPT NOT = 0 OR FS-ERR NOT = 0     
                DISPLAY 'UNABLE TO OPEN ONE OF THE FILES, '             
                DISPLAY '  PROGRAM TERMINATING.'                        
                DISPLAY 'TRANSACTION FILE STATUS IS ' FS-TRANS          
                DISPLAY 'REPORT FILE STATUS IS ' FS-RPT                 
                DISPLAY 'ERROR FILE STATUS IS ' FS-ERR                  
            END-IF                                                      
            PERFORM R010-READ UNTIL NO-MORE-TRANS                       
            CLOSE TRANS-FILE, RPT-FILE, ERR-FILE                        
            DISPLAY 'LEAVING  PROGRAM IB4OP01'                          
            GOBACK.                                                     
                                                                        
        R010-READ.                                                      
            READ TRANS-FILE AT END MOVE 'Y' TO TRANS-EOF                
            END-READ                                                    
            IF TRANS-PRESENT                                            
              PERFORM P100-VALIDATE                                     
            END-IF                                                      
            IF TRANS-PRESENT                                            
              PERFORM P600-PRINT-TRAN                                   
            END-IF                                                      
            IF TRANS-PRESENT AND (VALID-TRANS OR TRAN-WARN)             
              PERFORM P300-PROCESS                                      
            END-IF                                                      
            IF TRANS-PRESENT                                            
              PERFORM P500-REPORT                                       
            END-IF                                                      
            IF TRANS-PRESENT AND (TRAN-WARN OR TRAN-ERR)                
              PERFORM P800-ERR-RPT                                      
            END-IF                                                      
            EXIT.                                                       
                                                                        
        P100-VALIDATE.                                                  
            MOVE SPACE TO TRANS-STATUS                                  
            IF TI-TRAN = 'A '                                           
              IF TB-DESCR = SPACE                                       
                MOVE 'E'  TO TRANS-STATUS                               
                MOVE ALL '*' TO P-DESCR OF ERR-DTL                      
              END-IF                                                    
              IF TB-PO-QTY NOT NUMERIC                                  
                MOVE 'E' TO TRANS-STATUS                                
                MOVE ALL '*' TO P-ORD-QTY-X OF ERR-DTL                  
                MOVE TB-PO-QTY TO P-ORD-QTY-X OF PRT-DTL                
              END-IF                                                    
              IF TB-PO-AMT NOT NUMERIC                                  
                MOVE 'E' TO TRANS-STATUS                                
                MOVE ALL '*' TO P-ORD-QTY-X OF ERR-DTL                  
                MOVE TB-PO-AMT-X TO P-ORD-QTY-X OF PRT-DTL              
              END-IF                                                    
              IF NOT (TB-PRTY = '1' OR '2' OR '3')                      
                MOVE 'E' TO TRANS-STATUS                                
                MOVE '*' TO P-PRTY-CD OF ERR-DTL                        
                MOVE TB-PRTY TO P-PRTY-CD OF PRT-DTL                    
              END-IF                                                    
            END-IF                                                      
            IF TI-TRAN = 'UB'                                           
              IF TB-PO-QTY-X NOT = SPACE                                
                IF TB-PO-QTY NOT NUMERIC                                
                  MOVE 'E' TO TRANS-STATUS                              
                  MOVE ALL '*' TO P-ORD-QTY-X OF ERR-DTL                
                  MOVE TB-PO-QTY-X TO P-ORD-QTY-X OF PRT-DTL            
                END-IF                                                  
              END-IF                                                    
              IF TB-PO-AMT-X NOT = SPACE                                
                IF TB-PO-AMT NOT NUMERIC                                
                  MOVE 'E' TO TRANS-STATUS                              
                  MOVE ALL '*' TO P-ORD-AMT-X OF ERR-DTL                
                  MOVE TB-PO-AMT-X TO P-ORD-AMT-X OF PRT-DTL            
                END-IF                                                  
              END-IF                                                    
              IF TB-PRTY NOT = SPACE                                    
                IF NOT (TB-PRTY = '1' OR '2' OR '3')                    
                  MOVE 'E' TO TRANS-STATUS                              
                  MOVE '*' TO P-PRTY-CD OF ERR-DTL                      
                  MOVE TB-PRTY TO P-PRTY-CD OF PRT-DTL                  
                END-IF                                                  
              END-IF                                                    
            END-IF                                                      
            IF TI-TRAN = 'UA'                                           
              IF TA-UNITS-STARTED-X NOT = SPACE                         
                IF TA-UNITS-STARTED NOT NUMERIC                         
                  MOVE 'E' TO TRANS-STATUS                              
                  MOVE ALL '*' TO P-UNIT-STRT-X OF ERR-DTL              
                  MOVE TA-UNITS-STARTED-X TO P-UNIT-STRT-X OF PRT-DTL   
                END-IF                                                  
              END-IF                                                    
              IF TA-UNITS-COMPL-X NOT = SPACE                           
                IF TA-UNITS-COMPL NOT NUMERIC                           
                  MOVE 'E' TO TRANS-STATUS                              
                  MOVE ALL '*' TO P-UNIT-COMP-X OF ERR-DTL              
                  MOVE TA-UNITS-COMPL-X TO P-UNIT-COMP-X OF PRT-DTL     
                END-IF                                                  
              END-IF                                                    
            END-IF                                                      
            MOVE 'V' TO TRANS-STATUS                                    
            EXIT.                                                       
        P300-PROCESS.                                                   
            MOVE SPACE TO PRT-DTL                                       
            MOVE TI-ORDR-NO TO P-ORD-NUM OF PRT-DTL                     
            IF TI-TRAN = 'UB'                                           
              MOVE 'UPD B' TO P-TRANS OF PRT-DTL                        
            ELSE IF TI-TRAN = 'UA'                                      
                MOVE 'UPD A' TO P-TRANS OF PRT-DTL                      
              ELSE IF TI-TRAN = 'D '                                    
                  MOVE 'DEL  ' TO P-TRANS OF PRT-DTL                    
                ELSE IF TI-TRAN = 'Q '                                  
                    MOVE 'QUERY' TO P-TRANS OF PRT-DTL                  
                  END-IF                                                
                END-IF                                                  
              END-IF                                                    
            END-IF                                                                               
            IF TI-TRAN = 'UA' OR 'UB' OR 'D ' OR 'Q '                   
              MOVE 'GU  ' TO ACTION                                     
              IF TI-TRAN NOT = 'Q '                                     
                MOVE 'GHU ' TO ACTION                                   
              END-IF                                                    
              MOVE TI-ORDR-NO TO SSA-1-KEY                              
              CALL 'CBLTDLI' USING ACTION, DBPCB, ORDER-ROOT-DATA, SSA-1
              DISPLAY 'DBPCB: ' DBPCB                                                            
              IF DBSTATUS NOT = '  '                                    
                IF DBSTATUS = 'GE'                                      
                  MOVE 'NOT FOUND' TO P-ACT OF PRT-DTL                  
                  MOVE 'NOT FOUND' TO P-ACT OF ERR-DTL                  
                ELSE                                                    
                  MOVE 'UNKNWN ERR' TO P-ACT OF PRT-DTL                 
                  MOVE 'UNKNWN ERR' TO P-ACT OF ERR-DTL                 
                  PERFORM P900-IMS-ERR                                  
                END-IF                                                  
              MOVE 'E' TO TRANS-STATUS                                  
              END-IF                                                    
            END-IF                                                      
            IF NOT TRAN-ERR                                             
              MOVE 'V' TO TRANS-STATUS                                  
              IF TI-TRAN = 'D '                                         
                MOVE 'DLET' TO ACTION                                   
                CALL 'CBLTDLI' USING ACTION, DBPCB, ORDER-ROOT-DATA     
                DISPLAY 'DBPCB: ' DBPCB                                                     
                IF DBSTATUS NOT = '  '                                  
                  MOVE 'UNKNWN ERR' TO P-ACT OF PRT-DTL                 
                  MOVE 'UNKNWN ERR' TO P-ACT OF ERR-DTL                 
                  PERFORM P900-IMS-ERR                                  
                ELSE                                                    
                  MOVE 'DELETED' TO P-ACT OF PRT-DTL                    
                END-IF                                                  
                PERFORM P500-REPORT                                     
              END-IF                                                    
              IF TI-TRAN = 'A '                                                                         
                MOVE SPACES TO ORDER-ROOT-DATA                          
                MOVE 0 TO ACTUAL-ORDER-QUANTITY                         
                MOVE 0 TO TOTAL-SCRAP-QUANTITY                          
                MOVE 0 TO ORDER-STATUS                                  
                MOVE 0 TO NUMBER-UNITS-STARTED(1)                       
                MOVE 0 TO NUMBER-UNITS-COMPLETED(1)                     
                MOVE 0 TO PERCENTAGE-STARTED(1)                         
                MOVE 0 TO PERCENTAGE-COMPLETE(1)                        
                MOVE 0 TO NUMBER-UNITS-STARTED(2)                       
                MOVE 0 TO NUMBER-UNITS-COMPLETED(2)                     
                MOVE 0 TO PERCENTAGE-STARTED(2)                         
                MOVE 0 TO PERCENTAGE-COMPLETE(2)                        
                MOVE 0 TO NUMBER-UNITS-STARTED(3)                       
                MOVE 0 TO NUMBER-UNITS-COMPLETED(3)                     
                MOVE 0 TO PERCENTAGE-STARTED(3)                         
                MOVE 0 TO PERCENTAGE-COMPLETE(3)                        
                MOVE 0 TO NUMBER-UNITS-STARTED(4)                       
                MOVE 0 TO NUMBER-UNITS-COMPLETED(4)                     
                MOVE 0 TO PERCENTAGE-STARTED(4)                         
                MOVE 0 TO PERCENTAGE-COMPLETE(4)                        
                MOVE 0 TO NUMBER-UNITS-STARTED(5)                       
                MOVE 0 TO NUMBER-UNITS-COMPLETED(5)                     
                MOVE 0 TO PERCENTAGE-STARTED(5)                         
                MOVE 0 TO PERCENTAGE-COMPLETE(5)                        
                MOVE TI-ORDR-NO TO ORDER-ROOT-KEY                       
                MOVE TI-ORDR-NO TO SSA-1-KEY                            
                MOVE TB-CUST-NO TO CUSTOMER-NUMBER                      
                MOVE TB-DESCR  TO ORDER-DESCRIPTION                     
                MOVE TB-PO-QTY TO PLANNED-ORDER-QUANTITY                
                MOVE TB-PO-AMT TO PLANNED-ORDER-AMOUNT                  
                MOVE TB-ORDR-TYPE TO ORDER-TYPE                         
                MOVE TB-PRTY TO PRIORITY-CODE                           
                                                                        
                MOVE 'ISRT' TO ACTION                                   
                CALL 'CBLTDLI' USING ACTION, DBPCB, ORDER-ROOT-DATA,    
                    SSA-2                                               
                DISPLAY 'DBPCB: ' DBPCB                                                        
                IF DBSTATUS NOT = SPACE                                 
                  MOVE 'E' TO TRANS-STATUS                              
                  IF DBSTATUS = 'II'                                    
                    MOVE 'DUPLICATE' TO P-ACT OF PRT-DTL                
                    MOVE 'DUPLICATE' TO P-ACT OF ERR-DTL                
                  ELSE                                                  
                    MOVE 'UNKNWN ERR' TO P-ACT OF PRT-DTL               
                    MOVE 'UNKNWN ERR' TO P-ACT OF ERR-DTL               
                    PERFORM P900-IMS-ERR                                
                  END-IF                                                
                ELSE                                                    
                    MOVE 'ADDED    ' TO P-ACT OF PRT-DTL                
                END-IF                                                  
                PERFORM P500-REPORT                                     
              END-IF                                                    
              IF TI-TRAN = 'UB'                                                                       
                IF TB-CUST-NO NOT = SPACES                              
                  MOVE TB-CUST-NO TO CUSTOMER-NUMBER                    
                END-IF                                                  
                IF TB-DESCR NOT = SPACES                                
                  MOVE TB-DESCR  TO ORDER-DESCRIPTION                   
                END-IF                                                  
                IF TB-PO-QTY-X NOT = SPACES                             
                  MOVE TB-PO-QTY-X TO PLANNED-ORDER-QUANTITY            
                END-IF                                                  
                IF TB-PO-AMT-X NOT = SPACES                             
                  MOVE TB-PO-AMT TO PLANNED-ORDER-AMOUNT                
                END-IF                                                  
                IF TB-ORDR-TYPE NOT = SPACE                             
                  MOVE TB-ORDR-TYPE TO ORDER-TYPE                       
                END-IF                                                  
                IF TB-PRTY NOT = SPACE                                  
                  MOVE TB-PRTY TO PRIORITY-CODE                         
                END-IF                                                  
                                                                        
                MOVE 'REPL' TO ACTION                                   
                CALL 'CBLTDLI' USING ACTION, DBPCB, ORDER-ROOT-DATA     
                DISPLAY 'DBPCB: ' DBPCB                                                         
                IF DBSTATUS NOT = SPACE                                 
                  MOVE 'E' TO TRANS-STATUS                              
                  MOVE 'UNKNWN ERR' TO P-ACT OF PRT-DTL                 
                  MOVE 'UNKNWN ERR' TO P-ACT OF ERR-DTL                 
                  PERFORM P900-IMS-ERR                                  
                ELSE                                                    
                  MOVE 'CHANGED' TO P-ACT OF PRT-DTL                    
                END-IF                                                  
                PERFORM P500-REPORT                                     
              END-IF                                                    
              IF TI-TRAN = 'UA'                                                                       
                IF TA-ORD-STATUS NUMERIC                                
                  MOVE TA-ORD-STATUS TO ORDER-STATUS                    
                END-IF                                                  
                IF TA-ACT-DATE NOT = SPACE                              
                  MOVE TA-ACT-DATE TO FIRST-ACTIVITY-DATE               
                END-IF                                                  
                IF TA-LAST-ACT-DATE NOT = SPACE                         
                  MOVE TA-LAST-ACT-DATE TO LAST-ACTIVITY-DATE           
                END-IF                                                              
                                                                        
                MOVE 'REPL' TO ACTION                                   
                CALL 'CBLTDLI' USING ACTION, DBPCB, ORDER-ROOT-DATA     
                DISPLAY 'DBPCB: ' DBPCB                                                         
                IF DBSTATUS NOT = SPACE                                 
                  MOVE 'E' TO TRANS-STATUS                              
                  MOVE 'UNKNWN ERR' TO P-ACT OF PRT-DTL                 
                  MOVE 'UNKNWN ERR' TO P-ACT OF ERR-DTL                 
                  PERFORM P900-IMS-ERR                                  
                ELSE                                                    
                  MOVE 'CHANGED' TO P-ACT OF PRT-DTL                    
                END-IF                                                  
                PERFORM P500-REPORT                                     
              END-IF                                                    
            END-IF                                                      
            EXIT.                                                       
        P500-REPORT.                                                    
            IF (CUR-LINE-CNT + 5) > MAX-LINES                           
              MOVE H1 TO RPT-REC                                        
              PERFORM P750-PRINT                                        
              MOVE H2 TO RPT-REC                                        
              PERFORM P750-PRINT                                        
              MOVE '0' TO P-CC OF PRT-DTL                               
              MOVE 3 TO CUR-LINE-CNT                                    
            END-IF                                                      
            IF (ORDER-ROOT-KEY = SSA-1-KEY)                             
              MOVE 1 TO LOGICAL-LINE                                    
              MOVE ORDER-ROOT-KEY TO         P-ORD-NUM OF PRT-DTL       
              MOVE ORDER-DESCRIPTION TO      P-DESCR OF PRT-DTL         
              MOVE CUSTOMER-NUMBER TO        P-CUST-NO OF PRT-DTL       
              MOVE PLANNED-ORDER-QUANTITY TO P-ORD-QTY OF PRT-DTL       
              MOVE PLANNED-ORDER-AMOUNT   TO P-ORD-AMT OF PRT-DTL       
              MOVE ORDER-TYPE TO             P-TYPE OF PRT-DTL          
              MOVE ACTUAL-ORDER-QUANTITY TO  P-ACT-ORD-QTY OF PRT-DTL     
              MOVE ORDER-STATUS TO           P-ORD-STAT OF PRT-DTL      
              MOVE PRIORITY-CODE TO          P-PRTY-CD OF PRT-DTL       
              MOVE FIRST-ACTIVITY-DATE TO W-DATE                        
                MOVE 'YYYY/MM/DD' TO W-DATE-FMT                         
                MOVE CORRESPONDING W-DATE TO W-DATE-FMT                 
                MOVE W-DATE-FMT TO P-DATE OF PRT-DTL                    
              MOVE LAST-ACTIVITY-DATE TO W-DATE                         
                MOVE 'YYYY/MM/DD' TO W-DATE-FMT                         
                MOVE CORRESPONDING W-DATE TO W-DATE-FMT                 
                MOVE W-DATE-FMT TO P-DATE-L OF PRT-DTL                  
              PERFORM P700-PRINT                                        
              PERFORM VARYING LOGICAL-LINE FROM 1 BY 1                  
                  UNTIL LOGICAL-LINE > 5                                
                IF NUMBER-UNITS-STARTED (LOGICAL-LINE) > 0              
                  MOVE SPACES TO PRT-DTL                                
                  MOVE SPACES TO ERR-DTL                                
                  MOVE 'STARTED' TO P-STRT-CAPT OF PRT-DTL              
                  MOVE NUMBER-UNITS-STARTED(LOGICAL-LINE) TO            
                       P-UNIT-STRT OF PRT-DTL                           
                  MOVE PERCENTAGE-STARTED (LOGICAL-LINE) TO             
                       P-PCT-STRT OF PRT-DTL                            
                  MOVE NUMBER-UNITS-COMPLETED(LOGICAL-LINE) TO          
                       P-PCT-COMP OF PRT-DTL                            
                  MOVE 'COMPL' TO P-COMP-CAPT OF PRT-DTL                
                  PERFORM  P700-PRINT                                   
                END-IF                                                  
              END-PERFORM                                               
            ELSE                                                        
              PERFORM P700-PRINT                                        
            END-IF                                                      
            MOVE SPACES TO PRT-DTL                                      
            MOVE SPACES TO ERR-DTL                                      
            EXIT.                                                       
        P600-PRINT-TRAN.                                                
            IF (CUR-LINE-CNT + 1) > MAX-LINES                           
              MOVE H1 TO RPT-REC                                        
              PERFORM P750-PRINT                                        
              MOVE H2 TO RPT-REC                                        
              PERFORM P750-PRINT                                        
              MOVE '0' TO P-CC OF PRT-DTL                               
              MOVE 3 TO CUR-LINE-CNT                                    
            END-IF                                                      
            MOVE SPACE TO PRT-DTL                                       
            MOVE TI-TRAN TO P-TRANS OF PRT-DTL                          
            MOVE TI-ORDR-NO TO P-ORD-NUM OF PRT-DTL                     
            IF TI-TRAN = 'A ' OR TI-TRAN = 'UB'                         
              MOVE TB-CUST-NO TO P-CUST-NO OF PRT-DTL                   
              MOVE TB-DESCR TO P-DESCR OF PRT-DTL                       
              MOVE TB-PO-QTY-X TO P-ORD-QTY-X OF PRT-DTL                
              MOVE TB-PO-AMT-X TO P-ORD-AMT-X OF PRT-DTL                
              MOVE TB-ORDR-TYPE TO P-TYPE OF PRT-DTL                    
              MOVE TB-PRTY TO P-PRTY-CD OF PRT-DTL                      
            END-IF                                                      
            IF TI-TRAN = 'UA'                                           
              MOVE TA-ORD-STATUS TO P-ORD-STAT-X OF PRT-DTL             
              MOVE TA-ACT-DATE TO W-DATE                                
                MOVE 'YYYY/MM/DD' TO W-DATE-FMT                         
                MOVE CORRESPONDING W-DATE TO W-DATE-FMT                 
                MOVE W-DATE-FMT TO P-DATE OF PRT-DTL                    
              MOVE TA-LAST-ACT-DATE TO W-DATE                           
                MOVE 'YYYY/MM/DD' TO W-DATE-FMT                         
                MOVE CORRESPONDING W-DATE TO W-DATE-FMT                 
                MOVE W-DATE-FMT TO P-DATE-L OF PRT-DTL                  
              MOVE TA-UNITS-STARTED-X TO P-UNIT-STRT-X OF PRT-DTL       
              MOVE TA-UNITS-COMPL-X TO P-UNIT-COMP-X OF PRT-DTL         
            END-IF                                                      
            MOVE PRT-DTL TO RPT-REC                                     
            PERFORM P750-PRINT                                          
            MOVE SPACES TO PRT-DTL                                      
            MOVE SPACES TO ERR-DTL                                      
            EXIT.                                                       
        P700-PRINT.                                                     
            MOVE PRT-DTL TO RPT-REC                                     
            PERFORM P750-PRINT                                          
            EXIT.                                                       
        P750-PRINT.                                                     
            MOVE 1 TO ADV-LINES                                         
            IF R-CC = '0'                                               
              MOVE 2 TO ADV-LINES                                       
            END-IF                                                      
            IF R-CC = 'T'                                               
              MOVE 0 TO ADV-LINES                                       
            END-IF                                                      
            IF R-CC = '-'                                               
              MOVE 3 TO ADV-LINES                                       
            END-IF                                                      
            WRITE RPT-REC AFTER ADVANCING ADV-LINES                     
            ADD ADV-LINES TO CUR-LINE-CNT                               
            EXIT.                                                       
        P800-ERR-RPT.                                                   
            IF (ERR-LINE-CNT + 5) > MAX-LINES                           
              MOVE H1-E TO ERR-REC                                      
              PERFORM P850-ERR-PRINT                                    
              MOVE H2 TO ERR-REC                                        
              PERFORM P850-ERR-PRINT                                    
              MOVE '0' TO P-CC OF PRT-DTL                               
              MOVE 3 TO ERR-LINE-CNT                                    
            END-IF                                                      
            MOVE TI-TRAN TO P-TRANS OF ERR-DTL                          
            IF (ORDER-ROOT-KEY = SSA-1-KEY)                             
              MOVE 1 TO LOGICAL-LINE                                    
              MOVE ORDER-ROOT-KEY TO         P-ORD-NUM OF ERR-DTL       
              MOVE ORDER-DESCRIPTION TO      P-DESCR OF ERR-DTL         
              MOVE CUSTOMER-NUMBER TO        P-CUST-NO OF ERR-DTL       
              MOVE PLANNED-ORDER-QUANTITY TO P-ORD-QTY OF ERR-DTL       
              MOVE PLANNED-ORDER-AMOUNT TO   P-ORD-AMT OF ERR-DTL       
              MOVE ORDER-TYPE TO             P-TYPE OF ERR-DTL          
              MOVE ACTUAL-ORDER-QUANTITY TO  P-ACT-ORD-QTY OF ERR-DTL      
              MOVE ORDER-STATUS TO           P-ORD-STAT OF ERR-DTL      
              MOVE PRIORITY-CODE TO          P-PRTY-CD OF ERR-DTL       
            END-IF                                                      
            PERFORM P810-ERR-PRINT                                      
            EXIT.                                                       
        P810-ERR-PRINT.                                                 
            MOVE ERR-DTL TO ERR-REC                                     
            PERFORM P850-ERR-PRINT                                      
            MOVE SPACES TO ERR-DTL                                       
            EXIT.                                                        
        P850-ERR-PRINT.                                                  
            MOVE 1 TO ADV-LINES                                          
            IF E-CC = '0'                                                
              MOVE 2 TO ADV-LINES                                        
            END-IF                                                       
            IF E-CC = 'T'                                                
              MOVE 0 TO ADV-LINES                                        
            END-IF                                                       
            IF E-CC = '-'                                                
              MOVE 3 TO ADV-LINES                                        
            END-IF                                                       
            WRITE ERR-REC AFTER ADVANCING ADV-LINES                      
            ADD ADV-LINES TO ERR-LINE-CNT                                
            EXIT.                                                        
        P900-IMS-ERR.                                                    
            DISPLAY 'IMS ERROR DBD: ' DBD-NAME 'DBSTATUS: ' DBSTATUS     
            DISPLAY '   SEG-LEVEL: ' SEG-LEVEL ' PROCOPTIONS: '          
               PROC-OPTIONS                                              
            DISPLAY '   SEG-NAME-FB: '                                   
               SEG-NAME-FB                                               
            DISPLAY '   LENGTH-FB-KEY: ' LENGTH-FB-KEY ' NUM SENS SEGS:'
               NUMB-SENS-SEGS                                            
            DISPLAY '   KEY-FB-AREA: ' KEY-FB-AREA                       
            DISPLAY '   SSA: ' SSA-1                                     
            EXIT.                                                        