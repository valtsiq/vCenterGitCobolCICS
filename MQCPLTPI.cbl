       CBL CICS('COBOL3') APOST
      *****************************************************************
      *                                                               *
      *  MODULE NAME = MQCPLTPI                                       *
      *                                                               *
      *  DESCRIPTIVE NAME = ATIVA CONEXOES COM O MQ A PARTIR DA       *
      *                     PLTPI CICS START UP                      *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MQCPLTPI.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      * ---------------------------------------------------------------*
        01  WK-MQCONN.
           03 WK-CKQC       PIC X(004).
           03 WK-DISPMOD    PIC X(001).
           03 WK-CONNREQ    PIC X(010).
           03 WK-DELIM1     PIC X(001).
           03 WK-MQDEF      PIC X(001).
           03 WK-DELIM2     PIC X(001).
           03 WK-CONNSSN    PIC X(004).
           03 WK-DELIM3     PIC X(005).
           03 WK-CONNIQ     PIC X(048).

        01  WK-MISCELANEOUS.
           03 WK-EYECATCHER      PIC  X(016)
                                 VALUE 'VSRS0000------WS'.
           03 WK-TRANSID         PIC  X(004).
           03 WK-TERMID          PIC  X(004).
           03 WK-TASKNUM         PIC S9(007) COMP-3.
           03 WK-CALEN           PIC S9(004) COMP.
           03 WK-LENGTH-FULL     PIC S9(008) COMP.
           03 WK-LEN             PIC S9(004) COMP.
           03 WK-ABS-TIME        PIC S9(008) COMP.
           03 WK-TIME1           PIC  X(008) VALUE SPACES.
           03 WK-DATE1           PIC  X(010) VALUE SPACES.

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
           INITIALIZE WK-MQCONN.

           MOVE 'CKQC'                TO  WK-CKQC
           MOVE 'START     '          TO  WK-CONNREQ
           MOVE 'N'                   TO  WK-MQDEF
           MOVE 'CSQ9'                TO  WK-CONNSSN
           MOVE 'CICSTS55.000.INITQ                              '
                                      TO  WK-CONNIQ

           EXEC CICS MQCONN
                          COMMAREA(WK-MQCONN)
                          LENGTH (LENGTH OF WK-MQCONN)
           END-EXEC

           EXEC CICS RETURN
           END-EXEC

           STOP RUN.