       CBL CICS('COBOL3') APOST
      *****************************************************************
      *                                                               *
      *  MODULE NAME = DFH0XCMN                                       *
      *                                                               *
      *  DESCRIPTIVE NAME = CICS TS  (Samples) Example Application -  *
      *                     Catalog Manager Program                   *
      *                                                               *
      *                                                               *
      *                                                               *
      *      Licensed Materials - Property of IBM                     *
      *                                                               *
      *      "Restricted Materials of IBM"                            *
      *                                                               *
      *      5655-Y04                                                 *
      *                                                               *
      *      (C) Copyright IBM Corp. 2004, 2005"                      *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *  STATUS = 7.2.0                                               *
      *                                                               *
      *  TRANSACTION NAME = n/a                                       *
      *                                                               *
      *  FUNCTION =                                                   *
      *  This module is the controller for the Catalog application,   *
      *  all requests pass through this module                        *
      *                                                               *
      *-------------------------------------------------------------  *
      *                                                               *
      *  ENTRY POINT = DFH0XCMN                                       *
      *                                                               *
      *-------------------------------------------------------------  *
      *                                                               *
      *  CHANGE ACTIVITY :                                            *
      *                                                               *
      *  $MOD(DFH0XCMN),COMP(PIPELINE),PROD(CICS TS ):                *
      *                                                               *
      *  PN= REASON REL YYMMDD HDXXIII : REMARKS                      *
      * $D0= I07544 640 041126 HDIPCB  : ExampleApp: Outbound support *
      * $P1= D13727 640 050217 HDIPCB  : Minor fixes to the web servic*
      *  $D0= I07544 640 040910 HDIPCB  : EXAMPLE - BASE APPLICATION  *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSRSMAIN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'VSRSMAIN------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC S9(7) COMP-3.
           03 WS-CALEN                 PIC S9(4) COMP.
           03 WK-LEN                   PIC S9(4) COMP VALUE 200.

      * Variables for time/date processing
       01  ABS-TIME                    PIC S9(8) COMP VALUE +0.
       01  TIME1                       PIC X(8)  VALUE SPACES.
       01  DATE1                       PIC X(10) VALUE SPACES.

      * Error Message structure
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' EXMPCMAN'.
           03 FILLER                   PIC X(11) VALUE ' REQUESTID='.
           03 EM-REQUEST-ID            PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-DETAIL                PIC X(50) VALUE SPACES.

      * Working variables
       01 WORKING-VARIABLES.
           03 WS-RETURN-CODE           PIC S9(8) COMP.

      * Key into the configuration file
       01 EXAMPLE-APP-CONFIG       PIC X(9)
               VALUE 'EXMP-CONF'.

      * Format of the configuration file
       01 APP-CONFIG.
           03 FILE-KEY             PIC X(9).
           03 FILLER               PIC X.
           03 DATASTORE            PIC X(4).
           03 FILLER               PIC X.
           03 DO-OUTBOUND-WS       PIC X.
           03 FILLER               PIC X.
           03 CATMAN-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 DSSTUB-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 DSVSAM-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 ODSTUB-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 ODWEBS-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 STKMAN-PROG          PIC X(8).
           03 FILLER               PIC X.
           03 OUTBOUND-URL         PIC X(255).
           03 FILLER               PIC X(10).

      * Flag for Data Store program to call
       01 WS-DATASTORE-INUSE-FLAG         PIC X(4).
           88 DATASTORE-STUB                         VALUE 'STUB'.
           88 DATASTORE-VSAM                         VALUE 'VSAM'.

      * Switch For OutBound WebService on Order Dispatch
       01 WS-DISPATCHER-AS-WS-SWITCH       PIC X     VALUE 'N'.
           88 WS-DO-DISPATCHER-AS-WS                 VALUE 'Y'.

      * WORKING FOR MISCELANEOUS
       01 WS-MISCELANEOUR.
           03  WS-STARTCODE                PIC X(2)  VALUE SPACES.
           03  WS-DATASTORE-PROG           PIC X(8).
           03  WS-DISPATCH-PROG            PIC X(8).
           03  WS-STOCKMANAGER-PROG        PIC X(8).

      * Commarea structure for Order Dispatcher and Stock Manager Progs
       01 WK-SCRE-RESO.
          05 FILLER               PIC X(004) VALUE ZEROES.
          05 WK-SCRE-RESO-CODE    PIC 9(002) VALUE ZEROES.
          05 WK-SCRE-RESO-NAME    PIC X(008) VALUE SPACES.
          05 WK-SCRE-RESO-REDE REDEFINES WK-SCRE-RESO-NAME.
             07 WK-SCRE-RESO-TRAN PIC X(004) .
             07 FILLER            PIC X(004) .
          05 WK-SCRE-RESO-FILL    PIC X(186) VALUE SPACES.
       01 WK-COMM-RESO.
          05 WK-COMM-RESO-CODE    PIC 9(002) VALUE ZEROES.
          05 WK-COMM-RESO-NAME    PIC X(008) VALUE SPACES.
          05 WK-COMM-RESO-REDE REDEFINES WK-COMM-RESO-NAME.
             07 WK-COMM-RESO-TRAN PIC X(004) .
             07 FILLER            PIC X(004) .
          05 WK-COMM-RESO-FILL    PIC X(190) VALUE SPACES.
       01 WK-SCRE-SEND            PIC X(200) VALUE SPACES.
       01 WK-COMM-PROG.
          05 WK-COMM-PROG-CODE    PIC 9(002) VALUE ZEROES.
          05 WK-COMM-PROG-NAME    PIC X(008) VALUE SPACES.
          05 WK-COMM-PROG-USCH    PIC X(008) VALUE SPACES.
          05 WK-COMM-PROG-USIN    PIC X(008) VALUE SPACES.
          05 WK-COMM-PROG-IDDN    PIC X(008) VALUE SPACES.
          05 WK-COMM-PROG-IDSN    PIC X(044) VALUE SPACES.
          05 WK-COMM-TRAN-FILL    PIC X(122) VALUE SPACES.
       01 WK-COMM-TRAN.
          05 WK-COMM-TRAN-CODE    PIC 9(002) VALUE ZEROES.
          05 WK-COMM-TRAN-NAME    PIC X(004) VALUE SPACES.
          05 WK-COMM-TRAN-PROG    PIC X(008) VALUE SPACES.
          05 WK-COMM-TRAN-PROF    PIC X(008) VALUE SPACES.
          05 WK-COMM-TRAN-USCH    PIC X(008) VALUE SPACES.
          05 WK-COMM-TRAN-USIN    PIC X(008) VALUE SPACES.
          05 WK-COMM-TRAN-TCLA    PIC X(008) VALUE SPACES.
          05 WK-COMM-TRAN-FILL    PIC X(154) VALUE SPACES.

      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA             PIC X(200) VALUE SPACES.

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.

      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*
      * initialize working storage variables
           INITIALIZE APP-CONFIG.
           INITIALIZE ERROR-MSG.

      * set up general variable
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
           EXEC CICS INQUIRE TASK(WS-TASKNUM)
                     STARTCODE(WS-STARTCODE)
           END-EXEC
      *
           EXEC CICS WRITEQ TS QUEUE('VALTER')
                     FROM(WS-STARTCODE)
                     LENGTH(2)
           END-EXEC

      *---------------------------------------------------------------*
      * Verifica o codigo de START da task
      * O meio utilizado de resposta pela task depende da forma que
      * iniciada, se foi iniciada por terminal devera responder por
      * terminal, se iniciada por Dynamic Program Link (DPL) devera
      * responder por COMMAREA (LINKAGE)
      * Para avaliar o meio de retorno eh utilizado o valor contido
      * na variavel STARTCODE do INQUIRE TASK)
      *      -------------------------------------------------
      *      CODIGO - ORIGEM
      *       'TO'  - Terminal 3270
      *       'DS'  - DPL com Syncpoint, inclui z/OS Connect
      *---------------------------------------------------------------*
           EVALUATE WS-STARTCODE
           WHEN 'TO'
              EXEC CICS RECEIVE INTO(WK-SCRE-RESO)
                        LENGTH(WK-LEN)
              END-EXEC
      *
              MOVE WK-SCRE-RESO-CODE TO WK-COMM-RESO-CODE
              MOVE WK-SCRE-RESO-NAME TO WK-COMM-RESO-NAME
      *
              PERFORM EVALUATE-RESOURCE
      *
              EXEC CICS SEND FROM(WK-SCRE-SEND)
                        LENGTH(WK-LEN)
              END-EXEC
      *
           WHEN 'DS'
              MOVE DFHCOMMAREA TO WK-COMM-RESO
              PERFORM EVALUATE-RESOURCE
      *
           WHEN OTHER
              EXEC CICS WRITEQ TD('CSSL')
                   FROM ('VSRS0001E - SABE DEUS QUEM INICIOU')
                   LENGTH (34)
              END-EXEC
           END-EVALUATE
      *
           EXEC CICS RETURN
           END-EXEC
           .
       EVALUATE-RESOURCE.

           EVALUATE WK-COMM-RESO-CODE
               WHEN 1
                   PERFORM INQ-PROGRAM
                   IF EIBCALEN IS EQUAL TO ZERO
                      MOVE WK-COMM-PROG TO WK-SCRE-SEND
                   ELSE
                      MOVE WK-COMM-PROG TO DFHCOMMAREA
                   END-IF
               WHEN 2
                   PERFORM INQ-TRANSACTION
                   IF EIBCALEN IS EQUAL TO ZERO
                      MOVE WK-COMM-TRAN TO WK-SCRE-SEND
                   ELSE
                      MOVE WK-COMM-TRAN TO DFHCOMMAREA
                   END-IF
               WHEN OTHER
                   IF EIBCALEN IS EQUAL TO ZERO
                      MOVE '00????????' TO WK-SCRE-SEND
                   ELSE
                      MOVE '00????????' TO DFHCOMMAREA
                   END-IF
           END-EVALUATE
      *
           EXIT
           .
       INQ-PROGRAM.
           EXEC CICS INQUIRE PROGRAM(WK-COMM-RESO-NAME)
                     CHANGEUSRID(WK-COMM-PROG-USCH)
                     INSTALLUSRID(WK-COMM-PROG-USIN)
                     LIBRARY(WK-COMM-PROG-IDDN)
                     LIBRARYDSN(WK-COMM-PROG-IDSN)
           END-EXEC
      *
           MOVE WK-COMM-RESO-NAME TO WK-COMM-PROG-NAME
      *
           EXIT.
       INQ-TRANSACTION.
           EXEC CICS INQUIRE TRANSACTION(WK-COMM-RESO-TRAN)
                     PROGRAM(WK-COMM-TRAN-PROG)
                     PROFILE(WK-COMM-TRAN-PROF)
                     CHANGEUSRID(WK-COMM-TRAN-USCH)
                     INSTALLUSRID(WK-COMM-TRAN-USIN)
                     TRANCLASS(WK-COMM-TRAN-TCLA)
           END-EXEC
      *
           MOVE WK-COMM-RESO-TRAN TO WK-COMM-TRAN-NAME
      *
           EXIT.