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
       PROGRAM-ID. VSRS0000.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      * ---------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WK-HEADER.
           03 WK-EYECATCHER            PIC X(16)
                                        VALUE 'VSRS0000------WS'.
           03 WK-TRANSID               PIC X(4).
           03 WK-TERMID                PIC X(4).
           03 WK-TASKNUM               PIC S9(7) COMP-3.
           03 WK-CALEN                 PIC S9(4) COMP.
           03 WK-LEN                   PIC S9(4) COMP VALUE 200.

      * Variables for time/date processing
       01  ABS-TIME                    PIC S9(8) COMP VALUE +0.
       01  TIME1                       PIC X(8)  VALUE SPACES.
       01  DATE1                       PIC X(10) VALUE SPACES.

      * Error Message structure

      * Working variables
       01 WORKING-VARIABLES.
           03 WK-RETURN-CODE           PIC S9(8) COMP.

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

      * WORKING FOR MISCELANEOUS
       01 WK-MISCELANEOUS.
           03  WK-STARTCODE                PIC X(2)  VALUE SPACES.
           03  WK-DATASTORE-PROG           PIC X(8).
           03  WK-DISPATCH-PROG            PIC X(8).
           03  WK-STOCKMANAGER-PROG        PIC X(8).

       01 WK-ERROR.
           03  WK-ERROR-VSRS001E           PIC X(35)
               VALUE 'VSRS0001E - SABE DEUS QUEM INICIOU '.

      * Commarea structure for Order Dispatcher and Stock Manager Progs
       01 WK-CICS-RESO.
          03 WK-CICS-RESO-SERV-CODE        PIC X(004) VALUE SPACES.
          03 WK-CICS-RESO-NAME             PIC X(008) VALUE SPACES.
          03 WK-CICS-REDF   REDEFINES      WK-CICS-RESO-NAME.
             05 WK-CICS-TRAN-NAME          PIC X(004).
             05 FILLER                     PIC X(004).
          03 WK-CICS-RESO-FILL             PIC X(188) VALUE SPACES.
          03 WK-CICS-TRAN  REDEFINES  WK-CICS-RESO-FILL.
             05 WK-CICS-TRAN-PROG          PIC X(008).
             05 WK-CICS-TRAN-PROF          PIC X(008).
             05 WK-CICS-TRAN-USCH          PIC X(008).
             05 WK-CICS-TRAN-USIN          PIC X(008).
             05 WK-CICS-TRAN-TCLA          PIC X(008).
             05 WK-CICS-TRAN-FILL          PIC X(148).
          03 WK-CICS-PROG  REDEFINES  WK-CICS-RESO-FILL.
             05 WK-CICS-PROG-USCH          PIC X(008).
             05 WK-CICS-PROG-USIN          PIC X(008).
             05 WK-CICS-PROG-IDDN          PIC X(008).
             05 WK-CICS-PROG-IDSN          PIC X(044).
             05 WK-CICS-PROG-FILL          PIC X(120).
      *
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

      * set up general variable
           MOVE EIBTRNID TO WK-TRANSID.
           MOVE EIBTRMID TO WK-TERMID.
           MOVE EIBTASKN TO WK-TASKNUM.

           EXEC CICS INQUIRE TASK(WK-TASKNUM)
                     STARTCODE(WK-STARTCODE)
           END-EXEC

           EXEC CICS WRITEQ TS QUEUE('VALTER')
                     FROM(WK-STARTCODE)
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
      *       'TO'  - Terminal 3270 lu62
      *       'DS'  - DPL com Syncpoint, inclui z/OS Connect
      *---------------------------------------------------------------*
           EVALUATE WK-STARTCODE
           WHEN 'TO'
              EXEC CICS RECEIVE INTO(WK-CICS-RESO)
                        LENGTH(WK-LEN)
              END-EXEC

              PERFORM EVALUATE-RESOURCE

              MOVE 200 TO WK-LEN

              EXEC CICS SEND FROM(WK-CICS-RESO)
                        LENGTH(WK-LEN)
              END-EXEC

           WHEN 'DS'
              MOVE DFHCOMMAREA TO WK-CICS-RESO
              PERFORM EVALUATE-RESOURCE
              MOVE WK-CICS-RESO TO DFHCOMMAREA

           WHEN OTHER
              EXEC CICS WRITEQ TD QUEUE('CSSL')
                   FROM (WK-ERROR-VSRS001E)
                   LENGTH (35)
              END-EXEC
           END-EVALUATE

           EXEC CICS RETURN
           END-EXEC
           .
       EVALUATE-RESOURCE.

           MOVE FUNCTION UPPER-CASE(WK-CICS-RESO)
                         TO         WK-CICS-RESO

           EXEC CICS WRITEQ TS QUEUE('VALTER')
                     FROM(WK-CICS-RESO)
                     LENGTH(200)
           END-EXEC

           EVALUATE WK-CICS-RESO-SERV-CODE
               WHEN 'RC01'
                   PERFORM INQ-PROGRAM
               WHEN 'RC02'
                   PERFORM INQ-TRANSACTION
               WHEN OTHER
                   MOVE 'NAOEXIST' TO WK-CICS-RESO-NAME
           END-EVALUATE

           EXIT
           .
       INQ-PROGRAM.
           EXEC CICS INQUIRE PROGRAM(WK-CICS-RESO-NAME)
                     CHANGEUSRID    (WK-CICS-PROG-USCH)
                     INSTALLUSRID   (WK-CICS-PROG-USIN)
                     LIBRARY        (WK-CICS-PROG-IDDN)
                     LIBRARYDSN     (WK-CICS-PROG-IDSN)
                     RESP           (WK-RETURN-CODE)
           END-EXEC
      *
           IF WK-RETURN-CODE NOT EQUAL ZERO
              MOVE 'RECURSO NAO DEFINIDO' TO WK-CICS-PROG-IDSN
           END-IF

           EXIT.
       INQ-TRANSACTION.

           EXEC CICS INQUIRE TRANSACTION(WK-CICS-TRAN-NAME)
                     PROGRAM            (WK-CICS-TRAN-PROG)
                     PROFILE            (WK-CICS-TRAN-PROF)
                     CHANGEUSRID        (WK-CICS-TRAN-USCH)
                     INSTALLUSRID       (WK-CICS-TRAN-USIN)
                     TRANCLASS          (WK-CICS-TRAN-TCLA)
                     RESP               (WK-RETURN-CODE)
           END-EXEC
      *
           IF WK-RETURN-CODE NOT EQUAL ZERO
              MOVE 'INEXISTE'           TO WK-CICS-TRAN-PROG
           END-IF

           EXIT.