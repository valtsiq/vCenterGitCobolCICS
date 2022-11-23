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
       PROGRAM-ID. CICEXSMS.
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
       77  CTE-INICIO                  PIC  X(024)
                           VALUE 'TRANSACAO EXTR ACESSADA'.
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
      *****************************************************************
      *                                                               *
      *                                                               *
      *                                                               *
      *     Licensed Materials - Property of IBM                      *
      *                                                               *
      *     "Restricted Materials of IBM"                             *
      *                                                               *
      *     5655-Y04                                                  *
      *                                                               *
      *     (C) Copyright IBM Corp. 1990, 2012"                       *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *   STATUS = 7.2.0                                              *
      *                                                               *
      * CHANGE ACTIVITY :                                             *
      *                                                               *
      *   $SEG(DFHSMXDS),COMP(STATS),PROD(CICS TS ):                  *
      *                                                               *
      *  PN= REASON REL YYMMDD HDXXIII : REMARKS                      *
      * $D1= I07345 630 030404 HD1MA   : additional statistics for MMS*
      * $D2= I07991 670 100820 HDIGPG  : Translate unprintable char   *
      * $L0= Base   210 90     HD4PALS : Base                         *
      * $L1= 891    650 060522 HD1VCJB : 4 - 64-Bit Storage Manager St*
      * $L2= R00114 670 101027 HD1VCJB : 64-bit Storage Statistics    *
      * $L3= R00327 670 101110 HD1VCJB : Storage Statistics Update    *
      * $P1= D07560 630 030626 HDHYJB  : correct field name           *
      * $P2= D16767 650 061107 HDIKTG  : Fix nondisplay characters    *
      * $P3= D11296 670 100412 HDFXAB  : Missing memlimit info        *
      *      R31410 680 111025 HDIDNCS : Additional GxDSA Support     *
      *      D58528 680 120910 HD1VCJB : Update GxDSA comments        *
      *                                                               *
      *****************************************************************
      *
      *
      * ALTHOUGH PROVIDED IN A GENERAL INTERFACE LIBRARY DFHSMXDS IS
      * NOT TO BE USED AS A GENERAL PROGRAMMING INTERFACE. REFER TO
      * PRODUCT DOCUMENTATION TO DETERMINE INTENDED USAGE
      *
      *STORAGE STATISTICS HEADER
      *    LENGTH OF DATA AREA
           05  SMXLEN   PICTURE S9(4) USAGE IS COMPUTATIONAL.
      *    DSA STORAGE STATS ID
           05  SMXID    PICTURE XX.
      *        DSA STORAGE STATS ID MASK
               88  SMXIDE   VALUE X'001D'.
      *    STATISTICS VERSION NUMBER
           05  SMXDVERS PICTURE X.
      *        DSECT VERSION NUMBER MASK
               88  SMXVERS  VALUE X'01'.
      *    FILLER
           05  FILLER   PICTURE XXX.
      *
      *****************************************************************
      *    GLOBAL STATISTICS
      *****************************************************************
      *
      *    Global stats length
           05  SMXGBLLEN       PICTURE S9(4) USAGE IS COMPUTATIONAL.
      *    Number of Pagepools
           05  SMXNPAGP        PICTURE S9(4) USAGE IS COMPUTATIONAL.
      *    STORAGE MANAGER STATE
      *    SMXSTGPROTNA - STGPROT NOT ACTIVE
      *    SMXSTGPROTA  - STGPROT ACTIVE
           05 SMXSTGPROT PICTURE X.
              88 SMXSTGPROTNA VALUE X'00'.
              88 SMXSTGPROTA  VALUE X'01'.
      *    SMXRENTPGMNP - RENTPGM NOPROTECT
      *    SMXRENPTMGP  - RENTPGM PROTECT
           05 SMXRENTPGM PICTURE X.
              88 SMXRENTPGMNP VALUE X'00'.
              88 SMXRENTPGMP  VALUE X'01'.
      *    SMXTRANISONA - TRANISO NOT ACTIVE
      *    SMXTRANISOA  - TRANISO ACTIVE
           05 SMXTRANISO PICTURE X.
              88 SMXTRANISONA VALUE X'00'.
              88 SMXTRANISOA  VALUE X'01'.
      *    SMXMEMLIMITSRC-SMF  - MEMLIMIT Set by SMFPRMxx
      *    SMXMEMLIMITSRC-JCL  - MEMLIMIT Set by JCL
      *    SMXMEMLIMITSRC-REG  - MEMLIMIT Set by JCL Region
      *    SMXMEMLIMITSRC-USI  - MEMLIMIT Set by IEFUSI Exit
      *    SMXMEMLIMITSRC-AUTH - MEMLIMIT Set by AUTHORISED CODE
      *    SMXMEMLIMITSRC-URG  - MEMLIMIT Set by IEFUSI REGION
           05  SMXMEMLIMITSRC  PICTURE X.
              88 SMXMEMLIMITSRC-SMF   VALUE X'01'.
              88 SMXMEMLIMITSRC-JCL   VALUE X'02'.
              88 SMXMEMLIMITSRC-REG   VALUE X'03'.
              88 SMXMEMLIMITSRC-USI   VALUE X'04'.
              88 SMXMEMLIMITSRC-AUTH  VALUE X'09'.
              88 SMXMEMLIMITSRC-URG   VALUE X'0A'.
      *
      *    SUBSPACE STATISTICS
      *    CURRENT NUMBER OF UNIQUE SUBSPACE USERS
           05  SMXUSSCUR PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *    CUMULATIVE NUMBER OF UNIQUE SUBSPACE USERS
           05  SMXUSSCUM PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *    HWM OF UNIQUE SUBSPACE USERS
           05  SMXUSSHWM PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *    CURRENT NUMBER OF COMMON SUBSPACE USERS
           05  SMXCSSCUR PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *    CUMULATIVE NUMBER OF COMMON SUBSPACE USERS
           05  SMXCSSCUM PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *    HWM OF COMMON SUBSPACE USERS
           05  SMXCSSHWM PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *
      *    Current DSA Limit
           05  SMXDSALIMIT     PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *    Current EDSA Limit
           05  SMXEDSALIMIT    PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *    Current DSA Total
           05  SMXDSATOTAL     PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *    Current EDSA Total
           05  SMXEDSATOTAL    PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *    HWM DSA Total
           05  SMXHWMDSATOTAL  PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *    HWM EDSA Total
           05  SMXHWMEDSATOTAL PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *
      *    total time waiting for MVS storage
           05  SMXTIMEWAITMVS PICTURE S9(16) USAGE IS COMPUTATIONAL.
      *    number of requests for MVS storage causing wait
           05  SMXMVSSTGREQWAITS PICTURE S9(8) USAGE IS COMPUTATIONAL.
           05  FILLER PICTURE S9(8) USAGE IS COMPUTATIONAL.
           05  FILLER PICTURE S9(8) USAGE IS COMPUTATIONAL.
           05  FILLER PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *    MEMLIMIT size
           05  SMX-MEMLIMIT    PICTURE 9(18)  USAGE IS COMPUTATIONAL.
               88 SMX-MEMLIMIT-NOLIMIT VALUE 17592186040320.
      *    GETSTOR request size
           05  SMX-GETSTOR-SIZE        PIC 9(18)  USAGE IS COMP.
      *    Current Address Space Addressable
           05  SMX-AS-ACTIVE           PIC 9(18)  USAGE IS COMP.
      *    HWM Address Space Addressable
           05  SMX-HWM-AS-ACTIVE       PIC 9(18)  USAGE IS COMP.
      *    Current GDSA Active
           05  SMX-GDSA-ACTIVE         PIC 9(18)  USAGE IS COMP.
      *    HWM GDSA Active
           05  SMX-HWM-GDSA-ACTIVE     PIC 9(18)  USAGE IS COMP.
      *    Current GDSA Allocated
           05  SMX-GDSA-ALLOCATED      PIC 9(18)  USAGE IS COMP.
      *    HWM GDSA Allocated
           05  SMX-HWM-GDSA-ALLOCATED  PIC 9(18)  USAGE IS COMP.
      *    Reserved
           05  FILLER                  PIC S9(8)  USAGE IS COMP.
           05  FILLER                  PIC S9(8)  USAGE IS COMP.
      *    Reserved
           05  FILLER                  PIC 9(18)  USAGE IS COMP.
           05  FILLER                  PIC 9(18)  USAGE IS COMP.
      *    Reserved
           05  FILLER                  PIC 9(18)  USAGE IS COMP.
      *    Bytes Allocated to Private Memory Objects
           05  SMXLVABYTES             PIC 9(18)  USAGE IS COMP.
      *    Bytes Hidden within Private Memory Objects
           05  SMXLVHBYTES             PIC 9(18)  USAGE IS COMP.
      *    HWM Bytes Usable within Private Memory Objects
           05  SMXLVGBYTES             PIC 9(18)  USAGE IS COMP.
      *    Number of 64-bit Private Memory Objects
           05  SMXLVNMEMOBJ            PIC 9(18)  USAGE IS COMP.
      *    Reserved
           05  FILLER                  PIC 9(18)  USAGE IS COMP.
      *    Number of 64-bit Storage FROMGUARD Failures
           05  SMXFROMGUARDFAIL        PIC 9(18)  USAGE IS COMP.
      *    64-bit Storage FROMGUARD Failure Size
           05  SMXFROMGUARDFAILSIZE    PIC 9(18)  USAGE IS COMP.
      *    Reserved
           05  FILLER                  PIC 9(18)  USAGE IS COMP.
      *    Shared Bytes from Large Memory Objects
           05  SMXLVSHRBYTES           PIC 9(18)  USAGE IS COMP.
      *    HWM Shared Bytes within Large Memory Objects
           05  SMXLVSHRGBYTES          PIC 9(18)  USAGE IS COMP.
      *    Number of Shared Memory Objects Allocated
           05  SMXLVSHRNMEMOBJ         PIC 9(18)  USAGE IS COMP.
      *    Reserved
           05  FILLER                  PIC 9(18)  USAGE IS COMP.
      *    No. Auxiliary Slots to back 64-bit Private Memory Objects
           05  SMXHVAUXSLOTS           PIC 9(18)  USAGE IS COMP.
      *    HWM of Auxiliary Slots to back 64-bit Private Memory Objects
           05  SMXHVGAUXSLOTS          PIC 9(18)  USAGE IS COMP.
      *    No. Real Frames to back 64-bit Private Memory Objects
           05  SMXHVPAGESINREAL        PIC 9(18)  USAGE IS COMP.
      *    HWM Real Frames to back 64-bit Private Storage
           05  SMXHVGPAGESINREAL       PIC 9(18)  USAGE IS COMP.
      *    Number of Large Memory Objects Allocated
           05  SMXLARGEMEMOBJ          PIC 9(18)  USAGE IS COMP.
      *    Number of Large Pages Backed in Real Storage
           05  SMXLARGEPAGESINREAL     PIC 9(18)  USAGE IS COMP.
      *    Reserved
           05  FILLER          PICTURE X(8).
           05  FILLER          PICTURE X(8).
           05  FILLER          PICTURE X(8).
           05  FILLER          PICTURE X(8).
      *
      *STORAGE STATISTICS BODY
           05  SMXBODY OCCURS 1 TO 12 TIMES DEPENDING ON SMXNPAGP.
      *        DSA NAME
               10  SMXDSANAME   PICTURE X(8).
      *        LOCATION - BELOW/ABOVE/ABOVEBAR
               10  SMXLOCN  PICTURE X.
      *            LOCATION MASK
                   88  SMXBELOW    VALUE X'01'.
                   88  SMXABOVE    VALUE X'02'.
                   88  SMXABOVEBAR VALUE X'03'.
      *        ACCESS - CICS/USER/READONLY/TRUSTED
               10  SMXACCESS PICTURE X.
      *            ACCESS MASK
                   88  SMXCICS     VALUE X'01'.
                   88  SMXUSER     VALUE X'02'.
                   88  SMXREADONLY VALUE X'03'.
                   88  SMXTRUSTED  VALUE X'04'.
      *        DSA INDEX - CDSA/UDSA/SDSA/RDSA/ECDSA/EUDSA
      *                    ESDSA/ERDSA/ETDSA/GCDSA/GUDSA/GSDSA
               10  SMXDSAINDEX PICTURE X.
      *            DSA INDEX MASK
                   88  SMXCDSA  VALUE X'01'.
                   88  SMXUDSA  VALUE X'02'.
                   88  SMXSDSA  VALUE X'03'.
                   88  SMXRDSA  VALUE X'04'.
                   88  SMXECDSA VALUE X'09'.
                   88  SMXEUDSA VALUE X'0A'.
                   88  SMXESDSA VALUE X'0B'.
                   88  SMXERDSA VALUE X'0C'.
                   88  SMXETDSA VALUE X'0D'.
                   88  SMXGCDSA VALUE X'11'.
                   88  SMXGUDSA VALUE X'12'.
                   88  SMXGSDSA VALUE X'13'.
      *        FILLER
               10  FILLER      PICTURE X.
      *        Current SIZE of DSA
               10  SMXDSASZ    PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        HWM SIZE of DSA
               10  SMXHWMDSASZ PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        Current CUSHION SIZE
               10  SMXCSIZE    PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        No. GETMAIN REQS
               10  SMXGMREQ    PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        No. FREEMAIN REQS
               10  SMXFMREQ    PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        No. ADD-SUBPOOL REQS
               10  SMXASR      PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        No. DEL-SUBPOOL REQS
               10  SMXDSR      PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        COND REQS RETURNING INSUFFICIENT STG
               10  SMXCRISS    PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        UNCOND REQS SUSPENDED
               10  SMXUCSS     PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        Current REQS SUSP FOR STORAGE
               10  SMXCSS      PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        HWM REQS SUSP FOR STORAGE
               10  SMXHWMSS    PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        No. TASKS PURGED, WAITING STORAGE
               10  SMXPWWS     PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        No. CUSHION RELEASES
               10  SMXCREL     PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        TIMES SOS OCCURRED
               10  SMXSOS      PICTURE S9(8) USAGE IS COMPUTATIONAL.
               10  FILLER      PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        TOTAL TIME SOS
               10  SMXTSOS     PICTURE S9(16) USAGE IS COMPUTATIONAL.
      *        Current No. SUBPOOLS
               10  SMXCSUBP    PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        FREE STORAGE (INC CUSHION)
               10  SMXFSTG     PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        HWMFREE STORAGE (INC CUSHION)
               10  SMXHWMFSTG  PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        LWMFREE STORAGE (INC CUSHION)
               10  SMXLWMFSTG  PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        LARGEST FREE AREA in DSA
               10  SMXLFA      PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        No. of STORAGE VIOLATIONS
               10  SMXSV       PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        Current No. of EXTENTS
               10  SMXEXTS     PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        No. of EXTENTS ADDED
               10  SMXEXTSA    PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        No. of EXTENTS RELEASED
               10  SMXEXTSR    PICTURE S9(8) USAGE IS COMPUTATIONAL.
      *        Reserved
               10  FILLER      PICTURE S9(8) USAGE IS COMPUTATIONAL.
               10  FILLER      PICTURE S9(8) USAGE IS COMPUTATIONAL.
               10  FILLER      PICTURE S9(8) USAGE IS COMPUTATIONAL.
      ***

      *
      *COPY DFHDSGDS.
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
       COPY DFHSMSDS.
      *

      *********************
       PROCEDURE  DIVISION.
      *********************
      *
      *---------------------------------
       000000-MAIN.
      *---------------------------------
           PERFORM 000100-COLETA-STORAGE.
           PERFORM 999999-RETURN.
      *---------------------------------
       000100-COLETA-STORAGE.
      *---------------------------------
      *
           EXEC CICS EXTRACT STATISTICS STORAGE
               SET(ADDRESS OF DFHSMSDS)
               NOHANDLE
           END-EXEC.
           MOVE DFHSMSDS TO DFHCOMMAREA.
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999999-RETURN
           END-IF.
      *
       999999-RETURN.
           exec cics writeq
                td queue ( 'KEP0' )
                from     ( CTE-INICIO )
                length   ( LENGTH OF CTE-INICIO )
                NOHANDLE
           end-exec
           EXEC CICS RETURN
              NOHANDLE
           END-EXEC
           EXIT.

