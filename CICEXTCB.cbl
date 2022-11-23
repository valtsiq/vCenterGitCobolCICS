      *===============================================================*
      * PROGRAMA  : CICP0307
      * ANALISTA  : F9471854 VERA MACEDO
      * DATA      : 29/01/2019
      * SISTEMA   : CIC - CICS
      * LINGUAGEM : COBOL
      * COMPILACAO: 46 - PSOSE600 - Cobol 5.2 c/otimizacao p/producao
      * AMBIENTE  : ONLINE
      * OBJETIVO  : COLETAR ESTATISTICA NO CICS (TRANSACAO SC0I)
      *             E GRAVAR NA FILA MQ QE.CIC.ESTATISTICA.
      * OBSERVACAO: O GET NESTA FILA EH FEITO PELA ROTINA BATCH
      *             PCICD308, PROGRAMA CICP0308
      *----------------------------------------------------------------
      * VRS002 F9471854 16/07/2019 - TRANSFERE CALCULO DE QT-TRAN-MNTO
      *                              E QR DO MNTO PARA O CICP0308
      * VRS001 F9471854 29/01/2019 - IMPLANTACAO
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID. CICEXTCB.
       AUTHOR. VALTER SIQUEIRA.
       DATE-WRITTEN. 12/09/2022.
       DATE-COMPILED.
      *
      ***********************
       ENVIRONMENT  DIVISION.
      ***********************
      *
      ****************
       DATA  DIVISION.
      ****************
      *
      *-----------------------*
       WORKING-STORAGE SECTION.
      *-----------------------*
       77  CTE-INICIO                  PIC  X(018)
                                       VALUE 'W.S.S COMECA AQUI'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.
      *
       01  CTE-PROG-AUX.
           03  FILLER                  PIC  X(004) VALUE '*** '.
           03  CTE-PROG                PIC  X(008) VALUE 'CICP0307'.
           03  FILLER                  PIC  X(004) VALUE ' ***'.
      *
      *
       77 GDA-QT-ERRO                  PIC S9(009) COMP VALUE ZEROS.
       77 GDA-NR-DIAGNOSTIC            PIC S9(009) COMP VALUE ZEROS.
       77 GDA-LINHA-ERRO               PIC S9(009) COMP VALUE ZEROS.
      *
       01 S1CIC030-TRANSACTION.
          03 030-HR-CLA                PIC  X(008).
          03 REDEFINES 030-HR-CLA.
             05 030-HH-CLA             PIC  9(002).
             05 FILLER                 PIC  X(001).
             05 030-MM-CLA             PIC  9(002).
             05 FILLER                 PIC  X(001).
             05 030-SS-CLA             PIC  9(002).
          03 030-QT-TRAN-USU-MNTO      PIC S9(009) COMP VALUE ZEROS.
      *
      *
       LINKAGE SECTION.
      *-----------------
      * COPY BOOKS DAS AREAS CICS.
      *-----------------
       01 DFHCOMMAREA SYNCHRONIZED.
      *    LENGTH OF DATA AREA
           05  DXGLEN              PICTURE S9(4) COMPUTATIONAL.
      *    DISPATCHER DOMAIN GLOBAL STATISTICS ID
           05  DXGID               PICTURE XX.
      *        DISPATCHER DOMAIN GLOBAL STATISTICS ID MASK
               88  DXGIDE   VALUE X'003E'.
      *    STATS VERSION NUMBER
           05  DXGDVERS            PICTURE X.
      *        STATS VERSION NUMBER ID MASK
               88  DXGVERS  VALUE X'01'.
      *    FILLER
           05  FILLER              PICTURE XXX.
      *----------------------------------------------------------------
      * DXGLEN includes the length of the (DXGHDR + DXGSTATS)         *
      * effectively giving the offset to the first entry in TCB Mode  *
      * Stats (DXGTCB). DXGASIZE gives the number of entries in the   *
      *                                                               *
      * DXGASIZE gives the number of entries in the TCB Mode Stats    *
      * array.                                                        *
      *                                                               *
      * DXGPSIZE gives the number of entries in the TCB Pool Stats    *
      * array.                                                        *
      *----------------------------------------------------------------
           05  DXGHDR.
      *    GLOBAL STATS LENGTH
               10  DXGGLEN         PICTURE S9(4) COMPUTATIONAL.
      *    SIZE OF TCB MODE STATISTICS ARRAY
               10  DXGASIZE        PICTURE S9(4) COMPUTATIONAL.
      *    SIZE OF TCB POOL STATISTICS ARRAY
               10  DXGPSIZE        PICTURE S9(4) COMPUTATIONAL.
      *    Reserved
               10  FILLER          PICTURE XX.
      *----------------------------------------------------------------
      * Dispatcher Stats fields begin here.                           *
      *----------------------------------------------------------------
           05  DXGSTATS.
      *    Current ICV Time
               10  DXGICVT         PICTURE S9(8) COMPUTATIONAL.
      *    Current ICVR Time
               10  DXGICVRT        PICTURE S9(8) COMPUTATIONAL.
      *    Current ICVTSD Time
               10  DXGICVSD        PICTURE S9(4) COMPUTATIONAL.
      *    Current PRIORITY AGING Value
               10  DXGPRIAG        PICTURE S9(4) COMPUTATIONAL.
      *    Subtasks Value
               10  DXGSTSKS        PICTURE S9(4) COMPUTATIONAL.
      *    QR Batching (MRO) Value
               10  DXGMBTCH        PICTURE S9(4) COMPUTATIONAL.
      *    FILLER
               10  FILLER          PICTURE X(4).
      *    Current Number of Tasks
               10  DXGCNT          PICTURE S9(4) COMPUTATIONAL.
      *    Peak Number of Tasks
               10  DXGPNT          PICTURE S9(4) COMPUTATIONAL.
      *    FILLER
               10  FILLER          PICTURE X(8).
      *    FILLER
               10  FILLER          PICTURE X(8).
      *----------------------------------------------------------------
      *    The following 2 fields contain the sub-dispatcher start    *
      * time expressed in GMT and Local STCK formats respectively.    *
      *----------------------------------------------------------------
      *    DISPATCHER START TIME (GMT STCK)
               10  DXGSTART        PICTURE X(8).
      *    DISPATCHER START TIME (LOCAL STCK)
               10  DXGLSTRT        PICTURE X(8).
      *----------------------------------------------------------------
      *    ELAPSED JOP STEP TIMING
               10  DXGEJST         PICTURE X(8).
      *    ACCUMULATED SRB TIME
               10  DXGSRBT         PICTURE X(8).
      *    FILLER
               10  FILLER          PICTURE X(8).
      *    FILLER
               10  FILLER          PICTURE X(4).
      *    FILLER
               10  FILLER          PICTURE X(4).
      *----------------------------------------------------------------
      * Excess TCB Management Global Statistics.                      *
      *----------------------------------------------------------------
      *    NUMBER OF EXCESS TCB SCANS
               10  DXGXSCNS        PICTURE S9(8) COMPUTATIONAL.
      *    NUMBER OF EXCESS TCB SCANS WHEN NO TCB(S) DETACHED
               10  DXGXSCNN        PICTURE S9(8) COMPUTATIONAL.
      *    TOTAL NUMBER OF EXCESS TCB(S) DETACHED
               10  DXGXTCBD        PICTURE S9(8) COMPUTATIONAL.
      *    Reserved
               10  FILLER          PICTURE S9(8) COMPUTATIONAL.
      *    Time (GMT) of last excess TCB scan
               10  DXGGXSCN        PICTURE X(8).
      *    Time (local) of last excess TCB scan
               10  DXGLXSCN        PICTURE X(8).
      *    Time (GMT) of last excess TCB scan when no TCB(s) detached
               10  DXGGXSND        PICTURE X(8).
      *    Time (local) of last excess TCB scan when no TCB(s) detached
               10  DXGLXSND        PICTURE X(8).
      *    FILLER
               10  FILLER          PICTURE X(8).
      *----------------------------------------------------------------
      *    TCB MODE STATISTICS                                        *
      *                                                               *
      *    THE STATS FOR THE DISPATCHER TCB MODES ARE KEPT IN A FIXED *
      *    LENGTH ARRAY. THE NUMBER OF ENTRIES IN THE ARRAY IS IN     *
      *    FIELD DXGASIZE LOCATED AT THE BEGINING OF THE DXGHDR.      *
      *                                                               *
      *    THE TCB NUMBER TO DISPATCHER MODE MAP IS AS FOLLOWS:       *
      *                                                               *
      *     TCB1  = QUASI REENTRANT MODE                              *
      *     TCB2  = RESOURCE OWNING MODE                              *
      *     TCB3  = CONCURRENT MODE                                   *
      *     TCB4  = SECONDARY LU MODE                                 *
      *     TCB5  = ONC/RPC MODE                                      *
      *     TCB6  = FILE OWNING MODE                                  *
      *     TCB7  = SOCKETS OWNING MODE (SL)                          *
      *     TCB8  = SOCKETS OWNING MODE (SO)                          *
      *     TCB9  = SOCKETS PTHREAD Omwing Mode (SP)                  *
      *     TCB10 = EP - Event Processing Mode                        *
      *     TCB11 = TP - THREADED TCB Owning Mode                     *
      *     TCB12 = D2 - DB2 MODE                                     *
      *     TCB13 = S8 - SOCKETS (SSL) MODE                           *
      *     TCB14 = L8 - OPEN Mode                                    *
      *     TCB15 = L9 - OPEN Mode                                    *
      *     TCB16 = X8 - OPEN Mode                                    *
      *     TCB17 = X9 - OPEN Mode                                    *
      *     TCB18 = T8 - OPEN Mode                                    *
      *                                                               *
      *                                                               *
      *----------------------------------------------------------------
      *    TCB Mode Stats
           05  DXG-TCB-MODE OCCURS 18 TIMES.
      *        TCB Mode Name
               10  DXGTCBNM        PICTURE X(2).
      *        TCB Mode
               10  DXGTCBMD        PICTURE X.
                   88  DXG-TCB-MODE-UNKNOWN    VALUE X'00'.
                   88  DXG-TCB-MODE-NOT-OPEN   VALUE X'01'.
                   88  DXG-TCB-MODE-OPEN       VALUE X'02'.
      *        Reserved
               10  FILLER          PICTURE X.
      *        TCB Mode Pool Number
               10  DXGTCBMP        PICTURE S9(4) COMPUTATIONAL.
                   88  DXG-TCB-POOL-NOT-APPLIC VALUE 0.
                   88  DXG-TCB-POOL-OPEN-TCBS  VALUE 1.
                   88  DXG-TCB-POOL-XP-TCBS    VALUE 2.
                   88  DXG-TCB-POOL-SSL-TCBS   VALUE 3.
                   88  DXG-TCB-POOL-THRD-TCBS  VALUE 4.
      *        Reserved
               10  FILLER          PICTURE XX.
      *        No. of TCB Attaches
               10  DXGNTCBA        PICTURE S9(8) COMPUTATIONAL.
      *        No. of TCB Attach Failures
               10  DXGTCBAF        PICTURE S9(8) COMPUTATIONAL.
      *        Current No. of TCBs Attached
               10  DXGTCBCA        PICTURE S9(8) COMPUTATIONAL.
      *        Peak No. of TCBs Attached
               10  DXGTCBPA        PICTURE S9(8) COMPUTATIONAL.
      *        Reserved
               10  FILLER          PICTURE X(4).
      *        Current No. of TCBs Used
               10  DXGTCBCU        PICTURE S9(8) COMPUTATIONAL.
      *        Peak No. of TCBs Used
               10  DXGTCBPU        PICTURE S9(8) COMPUTATIONAL.
      *        Reserved
               10  FILLER          PICTURE S9(8) COMPUTATIONAL.
      *        Reserved
               10  FILLER          PICTURE S9(8) COMPUTATIONAL.
      *        No. TCB Allocates to a task
               10  DXGTCBAL        PICTURE S9(8) COMPUTATIONAL.
      *        Reserved
               10  FILLER          PICTURE S9(8) COMPUTATIONAL.
      *        No. of TCB Detaches - Unclean
               10  DXGTCBDU        PICTURE S9(8) COMPUTATIONAL.
      *        No. of TCB Detaches - Stolen (From this Mode)
               10  DXGTCBDS        PICTURE S9(8) COMPUTATIONAL.
      *        No. of TCB Detaches - Excess
               10  DXGTCBDX        PICTURE S9(8) COMPUTATIONAL.
      *        No. of TCB Detaches - Other
               10  DXGTCBDO        PICTURE S9(8) COMPUTATIONAL.
      *        Reserved
               10  FILLER          PICTURE S9(8) COMPUTATIONAL.
      *        No. of TCB Steals (For this Mode)
               10  DXGTCBST        PICTURE S9(8) COMPUTATIONAL.
      *        No. of TCB Mismatches
               10  DXGTCBMM        PICTURE S9(8) COMPUTATIONAL.
      *        No. of Partition Exits
               10  DXGSYSW         PICTURE S9(8) COMPUTATIONAL.
      *        Reserved
               10  FILLER          PICTURE X(12).
      *        Current tasks on dispatchable queue
               10  DXGTMCDQ        PICTURE S9(8) COMPUTATIONAL.
      *        Peak tasks on dispatchable queue
               10  DXGTMPDQ        PICTURE S9(8) COMPUTATIONAL.
      *        Average tasks on dispatchable queue
      *        (2 decimal places)
               10  DXGTMADQ        PICTURE S9(8) COMPUTATIONAL.
      *        Reserved
               10  FILLER          PICTURE S9(8) COMPUTATIONAL.
      *----------------------------------------------------------------
      * THE FOLLOWING CL8 DEFINITIONS ARE IN "STORE CLOCK" FORMAT     *
      *----------------------------------------------------------------
      *        Cumulative REAL (Elapsed) Time CICS in OS WAIT
               10  DXGTWT          PICTURE X(8).
      *        Cumulative REAL (Elapsed) Time TCB Dispatched by MVS
               10  DXGTDT          PICTURE X(8).
      *        Cumulative CPU Time for DS TASK
               10  DXGTCT          PICTURE X(8).
      *        Cumulative CPU Time for this TCB Mode
               10  DXGACT          PICTURE X(8).
      *        Reserved
               10  FILLER          PICTURE X(8).
      *        Reserved
               10  FILLER          PICTURE X(8).
      *----------------------------------------------------------------
      *    TCB POOL STATISTICS                                        *
      *                                                               *
      *    THE STATS FOR THE DISPATCHER TCB POOLS ARE KEPT IN A FIXED *
      *    LENGTH ARRAY. THE NUMBER OF ENTRIES IN THE ARRAY IS IN     *
      *    FIELD DXGPSIZE LOCATED AT THE BEGINING OF THE DXGHDR.      *
      *                                                               *
      *    THE TCB NUMBER TO DISPATCHER MODE MAP IS AS FOLLOWS:       *
      *                                                               *
      *     TCB POOL(1)  = MAXOPENTCBS                                *
      *     TCB POOL(2)  = MAXXPTCBS                                  *
      *     TCB POOL(3)  = MAXSSLTCBS                                 *
      *     TCB POOL(4)  = MAXTHRDTCBS                                *
      *                                                               *
      *----------------------------------------------------------------
      *    TCB POOL STATS
           05  DXG-TCB-POOL OCCURS 4 TIMES.
      *        TCB Pool Number
               10  DXGTCBPN        PICTURE S9(4) COMPUTATIONAL.
               88  DXG-TCB-POOL-OPEN   VALUE 1.
               88  DXG-TCB-POOL-XP     VALUE 2.
               88  DXG-TCB-POOL-SSL    VALUE 3.
               88  DXG-TCB-POOL-THRD   VALUE 4.
      *        Reserved
               10  FILLER          PICTURE XX.
      *        Max Number of TCBs (TCB Pool Limit)
               10  DXGMXTCB        PICTURE S9(8) COMPUTATIONAL.
      *        Current TCBs Attached
               10  DXGCNUAT        PICTURE S9(8) COMPUTATIONAL.
      *        Peak TCBs Attached
               10  DXGPNUAT        PICTURE S9(8) COMPUTATIONAL.
      *        Current TCBs in Use
               10  DXGCNUUS        PICTURE S9(8) COMPUTATIONAL.
      *        Peak TCBs in Use
               10  DXGPNUUS        PICTURE S9(8) COMPUTATIONAL.
      *        Reserved
               10  FILLER          PICTURE X(8).
      *        No. of Times at TCB Pool Limit
               10  DXGNTCBL        PICTURE S9(8) COMPUTATIONAL.
      *        Reserved
               10  FILLER          PICTURE S9(8) COMPUTATIONAL.
      *        Total Wait Time at Max TCB Pool Limit
               10  DXGTOTWL        PICTURE X(8).
      *        Current Waiting Time at Max TCB Pool Limit
               10  DXGCURWT        PICTURE X(8).
      *        Total MVS storage constraint delay time
               10  DXGTOTMT        PICTURE X(8).
      *        Total No. of Waits at Max TCB Pool Limit
               10  DXGTOTNW        PICTURE S9(8) COMPUTATIONAL.
      *        Requests delayed by MVS storage constraint
               10  DXGTOTMW        PICTURE S9(8) COMPUTATIONAL.
      *        Current No. of Tasks Waiting for a TCB
               10  DXGCURNW        PICTURE S9(8) COMPUTATIONAL.
      *        Peak No. of Tasks Waiting for a TCB
               10  DXGPEANW        PICTURE S9(8) COMPUTATIONAL.
      *        Reserved
               10  FILLER          PICTURE X(8).
      *        Reserved
               10  FILLER          PICTURE S9(8) COMPUTATIONAL.
      *        Total No. of TCB Mismatch Waits
               10  DXGMMWTS        PICTURE S9(8) COMPUTATIONAL.
      *        Total TCB Mismatch wait time
               10  DXGMMWTM        PICTURE X(8).
      *        Reserved
               10  FILLER          PICTURE X(8).
      *        Current No. of TCB Mismatch Waits
               10  DXGCMMWS        PICTURE S9(8) COMPUTATIONAL.
      *        Peak No. of TCB Mismatch Waits
               10  DXGPMMWS        PICTURE S9(8) COMPUTATIONAL.
      *        Total Wait Time for Current TCB Mismatch Waits
               10  DXGCMMWT        PICTURE X(8).
      *        Time (GMT) that pool limit was reached
               10  DXGGTCBL        PICTURE X(8).
      *        Time (local) that pool limit was reached
               10  DXGLTCBL        PICTURE X(8).
      *        Reserved
               10  FILLER          PICTURE X(8).
      *        Reserved
               10  FILLER          PICTURE X(8).
      *
      ***
      *
       COPY DFHDSGDS.
      *
      *** TRANSACTION MANAGER (GLOBALS) ID
      *
      *COPY DFHXMGDS.
      *
      *** TEMPORARY STORAGE
      *
      *COPY DFHTSGDS.
      *
      *** STORAGE
      *
      *COPY DFHSMSDS.
      *

      *********************
       PROCEDURE  DIVISION.
      *********************
      *
      *---------------------------------
       000000-MAIN.
      *---------------------------------
           PERFORM 000100-COLETA-DISPACHER.
           PERFORM 999999-RETURN.
      *---------------------------------
       000100-COLETA-DISPACHER.
      *---------------------------------
      *
           EXEC CICS EXTRACT STATISTICS DISPATCHER
               SET(ADDRESS OF DFHDSGDS)
               NOHANDLE
           END-EXEC.
           MOVE DFHDSGDS TO DFHCOMMAREA.
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999999-RETURN
           END-IF.
      *
       999999-RETURN.
           EXEC CICS RETURN
              NOHANDLE
           END-EXEC
           EXIT.

