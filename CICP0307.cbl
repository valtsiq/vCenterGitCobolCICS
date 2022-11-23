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
       PROGRAM-ID. CICP0307.
       AUTHOR. VERA.
       DATE-WRITTEN. 29/01/2019
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
       77  MQSP9011                    PIC  X(008) VALUE 'MQSP9011'.
      *
       77  GDA-TIMESTAMP               PIC S9(015) COMP-3 VALUE +0.
       77  GDA-APPLID                  PIC  X(008) VALUE SPACES.
       77  GDA-I                       PIC S9(009) COMP VALUE ZEROS.
       77  GDA-ITEM                    PIC S9(004) COMP VALUE 1.
       77  GDA-EIBRESP                 PIC  9(004) VALUE ZEROS.
       77  GDA-EIBRESP2                PIC  9(004) VALUE ZEROS.
       77  GDA-RESP                    PIC S9(008) COMP VALUE +0.
       77  GDA-NM-TRANSACTION          PIC  X(004) VALUE SPACES.
       77  GDA-ERRO-COMPUTE            PIC  X(012) VALUE SPACES.
       77  GDA-TMP-CPU                 PIC  9(018) VALUE ZEROS.
       77  IDX                         PIC S9(004) COMP VALUE ZEROS.
       77  IDY                         PIC  9(002) VALUE ZEROS.
       77  GDA-INTERVALO-START         PIC S9(007) COMP-3 VALUE 00100.
       77  GDA-INTERVALO-DISPLAY       PIC  9(006) VALUE ZEROS.
       77  GDA-INTERVAL                PIC S9(007) COMP-3 VALUE ZEROS.
       77  GDA-INTERVAL-DISPLAY        PIC  9(006)        VALUE ZEROS.
       77  GDA-NM-TS                   PIC  X(016)
                                       VALUE 'CICP0307_SC0I'.
       77  GDA-NM-TS-CICS              PIC  X(016)
                                       VALUE 'CICP0307_RECI'.
       77  TOTAL-AUX                   PIC S9(018) COMP VALUE +0.
       77  GDA-XMGMXT                  PIC S9(009) COMP VALUE +0.
       77  GDA-XMGTAMXT                PIC S9(009) COMP VALUE +0.
       77  GDA-XMGPAT                  PIC S9(009) COMP VALUE +0.
       77  GDA-XMGTAT                  PIC S9(009) COMP VALUE +0.
       77  GDA-XMGTDT                  PIC S9(009) COMP VALUE +0.
       77  GDA-SDGSDREQ                PIC S9(009) COMP VALUE +0.
       77  GDA-SDGSDSUP                PIC S9(009) COMP VALUE +0.
       77  GDA-SDGTDREQ                PIC S9(009) COMP VALUE +0.
       77  GDA-SDGTDSUP                PIC S9(009) COMP VALUE +0.
       77  GDA-D2G-POOL-THREAD-LIMIT   PIC S9(009) COMP VALUE +0.
       77  GDA-D2G-POOL-THREAD-HWM     PIC S9(009) COMP VALUE +0.
       77  GDA-REQID                   PIC  X(008) VALUE 'REQ_SC0I'.
       77  GDA-TRANSID                 PIC  X(004) VALUE 'SC0I'.
       77  GDA-DATA                    PIC  X(010) VALUE SPACES.
       77  GDA-IN-COLETA               PIC  X(001) VALUE SPACES.
       77  GDA-IN-TS-EXTT              PIC  X(001) VALUE SPACES.
       77  GDA-NM-TRANCLASS            PIC  X(008) VALUE SPACES.
       77  GDA-XMCCQTME                PIC S9(009) COMP VALUE +0.
       77  GDA-DSGPEANW                PIC S9(008) COMP VALUE +0.
       77  GDA-TSGTSMLM                PIC S9(008) COMP VALUE +0.
       77  GDA-TSGTSMUS                PIC S9(008) COMP VALUE +0.
       77  GDA-DSGTCBNM                PIC  X(002) VALUE SPACES.
       77  GDA-DSGTCBCA                PIC S9(009) COMP VALUE +0.
       77  GDA-DSGTCBPA                PIC S9(009) COMP VALUE +0.
       77  GDA-DSGNTCBA                PIC S9(009) COMP VALUE +0.
       77  GDA-DSGTCBAF                PIC S9(009) COMP VALUE +0.
       77  GDA-DSGSYSW                 PIC S9(009) COMP VALUE +0.
       77  GDA-SMSDSALIMIT             PIC S9(008) COMP VALUE +0.
       77  GDA-SMSDSATOTAL             PIC S9(008) COMP VALUE +0.
       77  GDA-SMSEDSALIMIT            PIC S9(008) COMP VALUE +0.
       77  GDA-SMSEDSATOTAL            PIC S9(008) COMP VALUE +0.
       77  GDA-CD-EST-QR               PIC S9(004) COMP VALUE ZEROS.
       77  GDA-QT-TMP-EFT-UTZD-MNTO    PIC S9(018) COMP VALUE ZEROS.
       77  GDA-QT-TMP-DSPD-MNTO        PIC S9(018) COMP VALUE ZEROS.
       77  GDA-CD-TIP-MSG-MQ           PIC  9(002) VALUE ZEROS.
      *
      ***  Variaveis connection
      *
       77  CONNECTION-NAME             PIC  X(004) VALUE SPACES.
       77  ACCESS-METHOD               PIC S9(008) COMP VALUE +0.
       77  PROTOCOL                    PIC S9(008) COMP VALUE +0.
       77  CONNECTION-NETNAME          PIC  X(008) VALUE SPACES.
       77  CONNECTION-TYPE             PIC S9(008) COMP VALUE +0.
       77  SEND-COUNT                  PIC S9(008) COMP VALUE +0.
       77  RECEIVE-COUNT               PIC S9(008) COMP VALUE +0.
      *
      ***  Variaveis para gravacao de erro na sysout
      *
       01  GDA-CICERRO.
           03  GDA-TRAN-SYSOUT         PIC  X(004).
           03  FILLER                  PIC  X(001).
           03  GDA-DATA-SYSOUT         PIC  X(010).
           03  FILLER                  PIC  X(001).
           03  GDA-HORA-SYSOUT         PIC  X(008).
           03  FILLER                  PIC  X(001).
           03  GDA-PGM-SYSOUT          PIC  X(008).
           03  FILLER                  PIC  X(001).
           03  GDA-TX-ERRO-LIVRE       PIC  X(132).
      *
       01  GDA-HORA                    PIC  X(008) VALUE SPACES.
       01  REDEFINES GDA-HORA.
           03 GDA-HH-ATU               PIC  9(002).
           03 FILLER                   PIC  X(001).
           03 GDA-MM-ATU               PIC  9(002).
           03 FILLER                   PIC  X(001).
           03 GDA-SS-ATU               PIC  9(002).
      *
       01  GDA-DSGTWT-A                PIC  X(008).
       01  FILLER REDEFINES GDA-DSGTWT-A.
           03  GDA-DSGTWT-N            PIC S9(018) COMP.
      *
       01  GDA-DSGTDT-A                PIC  X(008).
       01  FILLER REDEFINES GDA-DSGTDT-A.
           03  GDA-DSGTDT-N            PIC S9(018) COMP.
      *
       01  GDA-DSGACT-A                PIC  X(008).
       01  FILLER REDEFINES GDA-DSGACT-A.
           03  GDA-DSGACT-N            PIC S9(018) COMP.
      *
       01  GDA-DSGEJST-A               PIC  X(008).
       01  FILLER REDEFINES GDA-DSGEJST-A.
           03  GDA-DSGEJST-N           PIC S9(018) COMP.
       01  GDA-DSGEJST-I               PIC S9(018) COMP.
      *
       01  GDA-DSGSRBT-A               PIC  X(008).
       01  FILLER REDEFINES GDA-DSGSRBT-A.
           03  GDA-DSGSRBT-N           PIC S9(018) COMP.
       01  GDA-DSGSRBT-I               PIC S9(018) COMP.
      *
       01  GDA-XMCTQTME-A              PIC  X(008).
       01  FILLER REDEFINES GDA-XMCTQTME-A.
           03  GDA-XMCTQTME-N          PIC S9(018) COMP.
      *
       01  GDA-XMCCQTME-A              PIC  X(008).
       01  FILLER REDEFINES GDA-XMCCQTME-A.
           03  GDA-XMCCQTME-N          PIC S9(015) COMP.
      *
       01 GDA-TS-QUEUE                 PIC  X(094) VALUE SPACES.
       01 REDEFINES GDA-TS-QUEUE.
          03 GDA-TX-1                  PIC  X(015).
          03 GDA-HR-INC                PIC  9(002).
          03 GDA-TX-2                  PIC  X(004).
          03 GDA-HR-FIM                PIC  9(002).
          03 GDA-TX-3                  PIC  X(011).
          03 GDA-ITVL                  PIC  9(006).
          03 GDA-TX-4                  PIC  X(039).
          03 GDA-ITVL-2                PIC  9(006).
          03 GDA-TX-5                  PIC  X(009).
      *
      *** Variáveis para compute
      *
       01 GDA-SEGUNDOS                 PIC S9(004) COMP VALUE 0.
       01 HR-CLA-ANT                   PIC  X(008) VALUE SPACES.
       01 REDEFINES HR-CLA-ANT.
          03 HH-CLA-ANT                PIC  9(002).
          03 FILLER                    PIC  X(001).
          03 MM-CLA-ANT                PIC  9(002).
          03 FILLER                    PIC  X(001).
          03 SS-CLA-ANT                PIC  9(002).
      *
       01 HR-CLA-ATU-EM-SS             PIC S9(009) COMP VALUE ZEROS.
      * 01 GDA-ITVL-ENTRE-CLA           PIC S9(004) COMP VALUE ZEROS.
      *
       01 GDA-DADOS-TS-CICS            PIC  X(016) VALUE SPACES.
       01 REDEFINES GDA-DADOS-TS-CICS.
          03 NM-CICS                   PIC  X(008).
          03 HR-CLA-ANT-EM-SS          PIC S9(009) COMP.
          03 QT-TRAN-USU-MIN-ANT       PIC S9(009) COMP.
      *
      ***  PARAMETROS ROTINA MQSP9011 - FAZ PUT EM FILA MQ
      *
-INC MQSK9011
      *
      ***  PARAMETROS ROTINA MDPS7053 - CONVENSAO TOD CLOCK
      *
       COPY MDPK7053.
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
       01  CICK0015-BOOK.
-INC CICK0015
      *
       01  CICK0030-BOOK.
-INC CICK0030
      *
       01  CICK0031-BOOK.
-INC CICK0031
      *
       01  CICK0033-BOOK.
-INC CICK0033
      *
       01  CICK0035-BOOK.
-INC CICK0035
      *
      *-----------------
       LINKAGE  SECTION.
      *-----------------
      *
      *** TRANSACTION MANAGER (GLOBALS) ID
      *
       COPY DFHXMGDS.
      *
      *** TRANCLASS
      *
       COPY DFHXMCDS.
      *
      *** DISPATCHER
      *
       COPY DFHDSGDS.
      *
      *** TEMPORARY STORAGE
      *
       COPY DFHTSGDS.
      *
      *** SYSDUMP
      *
       COPY DFHSDGDS.
      *
      *** TRANSACTION DUMP
      *
       COPY DFHTDGDS.
      *
      *** DB2CONN
      *
       COPY DFHD2GDS.
      *
      *** Connection
      *
       COPY DFHA14DS.
      *
      *** TRANSACTION MANAGER (TRANS) ID
      *
       COPY DFHXMRDS.
      *
      *** STORAGE
      *
       COPY DFHSMSDS.
      *
      *********************
       PROCEDURE  DIVISION.
      *********************
      *
      *--------------------------------*
       000000-ROTINA-PRINCIPAL  SECTION.
      *--------------------------------*
      *
           PERFORM 000010-PROCEDIMENTOS-INICIAIS.
      *
           IF GDA-IN-COLETA EQUAL 'S'
              PERFORM 000020-TRATA-NOVO-START
      *
              PERFORM 000100-COLETA-TRANSACTION
              PERFORM 000200-COLETA-TRANCLASS
              PERFORM 000300-COLETA-DISPACHER
              PERFORM 000400-COLETA-DISPACHER-TCB
              PERFORM 000500-COLETA-TCB-POOL
              PERFORM 000600-COLETA-TS
              PERFORM 000700-COLETA-DUMP
              PERFORM 000800-COLETA-DB2CONN
              PERFORM 000900-COLETA-CONNECTION
              PERFORM 001000-COLETA-TRANS-ID
              PERFORM 001100-COLETA-STORAGE
              PERFORM 200000-GRAVA-ETTC-CICS
              PERFORM 990000-PROCEDIMENTOS-FINAIS
           END-IF.
      *
           MOVE SPACES TO GDA-CICERRO.
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       000999-FIM.
           EXIT.
      *
      *---------------------------------------
       000010-PROCEDIMENTOS-INICIAIS  SECTION.
      *---------------------------------------
      *
           MOVE SPACES TO GDA-CICERRO.
      *
           INITIALIZE CICK0015-BOOK
                      CICK0030-BOOK
                      CICK0031-BOOK
                      CICK0035-BOOK
              REPLACING NUMERIC BY ZEROS
                   ALPHANUMERIC BY SPACES.
      *
      ***  RECUPERA TIMESTAMP ATUAL
      *
           EXEC CICS ASKTIME
                     ABSTIME ( GDA-TIMESTAMP )
                     NOHANDLE
           END-EXEC.
      *
           EXEC CICS FORMATTIME
                     ABSTIME     ( GDA-TIMESTAMP )
                     DDMMYYYY    ( GDA-DATA )
                     DATESEP     ( '.' )
                     TIME        ( GDA-HORA )
                     TIMESEP     ( ':' )
                     NOHANDLE
           END-EXEC.
      *
      ***  CALCULA HORA ATUAL EM SEGUNDOS
           COMPUTE HR-CLA-ATU-EM-SS = (GDA-HH-ATU * 3600)
                                    + (GDA-MM-ATU *   60)
                                    +  GDA-SS-ATU
                   ON SIZE ERROR CONTINUE
           END-COMPUTE.
      *
      ***  RECUPERA NOME DO CICS
      *
           EXEC CICS
                ASSIGN APPLID(GDA-APPLID)
           END-EXEC.
      *
      ***  VALIDA SE EXISTE TRANSAÇãO SCHEDULADA (ENFILEIRADA)
      ***  SE EXISTIR, FINALIZA PROCESSO E NãO COLETA.
      *
           EXEC CICS INQUIRE REQID    ( GDA-REQID )
                             INTERVAL ( GDA-INTERVAL )
                             TRANSID  ( GDA-TRANSID )
                             NOHANDLE
           END-EXEC.
      *
           EVALUATE EIBRESP
              WHEN 0
                   MOVE 'N'            TO GDA-IN-COLETA
              WHEN 13
      ***          Indica que não existe outra SC0I schedulada
                   PERFORM 000011-VERIFICA-RECICLAGEM
              WHEN OTHER
                   GO TO 999003-ERRO-003
           END-EVALUATE.
      *
       000009-FIM.
           EXIT.
      *
      *------------------------------------
       000011-VERIFICA-RECICLAGEM  SECTION.
      *------------------------------------
      *
           MOVE GDA-APPLID             TO NM-CICS
           MOVE ZEROS                  TO HR-CLA-ANT-EM-SS
           MOVE ZEROS                  TO QT-TRAN-USU-MIN-ANT
      *
           EXEC CICS READQ TS QNAME ( GDA-NM-TS-CICS                )
                              INTO  ( GDA-DADOS-TS-CICS             )
                              LENGTH( LENGTH OF GDA-DADOS-TS-CICS   )
                              ITEM  ( GDA-ITEM                      )
                              NOHANDLE
           END-EXEC.

      *
           EVALUATE EIBRESP
              WHEN 0
                   MOVE 'S'              TO GDA-IN-TS-EXTT
                   MOVE 'S'              TO GDA-IN-COLETA
              WHEN DFHRESP( QIDERR )
      ***          Ts não localizada
      *
                   MOVE 'N'              TO GDA-IN-TS-EXTT
                   IF GDA-HORA(7:2) EQUAL '00'
                      MOVE 'S'           TO GDA-IN-COLETA
                   ELSE
      ***             Regula horario schedule
      *
                      MOVE GDA-HORA(7:2) TO GDA-SEGUNDOS
                      MOVE 'N'           TO GDA-IN-COLETA
                      SUBTRACT GDA-SEGUNDOS FROM 60
                        GIVING GDA-INTERVAL
      *
                      EXEC CICS START TRANSID( EIBTRNID )
                                REQID        ( GDA-REQID )
                                INTERVAL     ( GDA-INTERVAL )
                      END-EXEC
                   END-IF
              WHEN OTHER
                   GO TO 999029-ERRO-029
           END-EVALUATE.
      *
       000019-FIM.
           EXIT.
      *
      *---------------------------------
       000020-TRATA-NOVO-START  SECTION.
      *---------------------------------
      *
      ***  Lê tsmodel CIC$SC0I para obter intervalo de start
      *
           EXEC CICS READQ TS QNAME ( GDA-NM-TS                     )
                              INTO  ( GDA-TS-QUEUE                  )
                              LENGTH( LENGTH OF GDA-TS-QUEUE        )
                              ITEM  ( GDA-ITEM                      )
                              NOHANDLE
           END-EXEC.
      *
           EVALUATE EIBRESP
              WHEN 0
                   IF GDA-HR-INC NOT NUMERIC
                   OR GDA-HR-FIM NOT NUMERIC
                   OR GDA-ITVL   NOT NUMERIC
                   OR GDA-ITVL-2 NOT NUMERIC
                      MOVE 'Intervalo dia: '      TO GDA-TX-1
                      MOVE  07                    TO GDA-HR-INC
                      MOVE ' as '                 TO GDA-TX-2
                      MOVE  18                    TO GDA-HR-FIM
                      MOVE ' Schedule: '          TO GDA-TX-3
                      MOVE 000100                 TO GDA-ITVL
                      MOVE ' (HHMMSS) - Demais horarios, Schedule: '
                                                  TO GDA-TX-4
                      MOVE 000100                 TO GDA-ITVL-2
                      MOVE ' (HHMMSS)'            TO GDA-TX-5
      *
                      EXEC CICS   WRITEQ TS QNAME( GDA-NM-TS  )
                             FROM     ( GDA-TS-QUEUE          )
                             LENGTH   ( LENGTH OF GDA-TS-QUEUE)
                             ITEM     ( GDA-ITEM              )
                             REWRITE
                             NOHANDLE
                      END-EXEC
      *
                      IF EIBRESP NOT EQUAL ZEROS
                         GO TO 999027-ERRO-027
                      END-IF
                   END-IF
              WHEN DFHRESP( QIDERR )
      ***          Tsmodel não localizada - cria novamente
                   MOVE 'Intervalo dia: '      TO GDA-TX-1
                   MOVE  07                    TO GDA-HR-INC
                   MOVE ' as '                 TO GDA-TX-2
                   MOVE  18                    TO GDA-HR-FIM
                   MOVE ' Schedule: '          TO GDA-TX-3
                   MOVE 000100                 TO GDA-ITVL
                   MOVE ' (HHMMSS) - Demais horários, Schedule: '
                                               TO GDA-TX-4
                   MOVE 000100                 TO GDA-ITVL-2
                   MOVE ' (HHMMSS)'            TO GDA-TX-5
      *
                   EXEC CICS WRITEQ TS QNAME( GDA-NM-TS             )
                             FROM   ( GDA-TS-QUEUE                  )
                             LENGTH ( LENGTH OF GDA-TS-QUEUE        )
                             ITEM   ( GDA-ITEM                      )
                             NOHANDLE
                   END-EXEC
      *
                   IF EIBRESP NOT EQUAL ZEROS
                      GO TO 999001-ERRO-001
                   END-IF
              WHEN OTHER
                   GO TO 999002-ERRO-002
           END-EVALUATE.
      *
           IF GDA-HORA(1:2) GREATER GDA-HR-FIM
           OR GDA-HORA(1:2) LESS    GDA-HR-INC
              MOVE GDA-ITVL-2   TO GDA-INTERVALO-START
           ELSE
              MOVE GDA-ITVL     TO GDA-INTERVALO-START
           END-IF.
      *
           EXEC CICS START TRANSID( EIBTRNID )
                     REQID        ( GDA-REQID )
                     INTERVAL     ( GDA-INTERVALO-START )
           END-EXEC.
      *
       000029-SAI.
           EXIT.
      *
      *----------------------------------*
       000100-COLETA-TRANSACTION  SECTION.
      *----------------------------------*
      *
           EXEC CICS EXTRACT STATISTICS TRANSACTION
               SET(ADDRESS OF DFHXMGDS)
               NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999004-ERRO-004
           END-IF.
      *
           INITIALIZE TOTAL-AUX
                REPLACING NUMERIC BY ZEROS.
      *
           MOVE 'TOTAL-AUX' TO GDA-ERRO-COMPUTE.
           COMPUTE TOTAL-AUX = XMGTNUM + XMGNUM
              ON SIZE ERROR GO TO 901000-TRATA-ERRO-COMPUTE
           END-COMPUTE.
      *
           MOVE XMGMXT       TO GDA-XMGMXT.
           MOVE XMGTAMXT     TO GDA-XMGTAMXT.
           MOVE XMGPAT       TO GDA-XMGPAT.
           MOVE XMGTAT       TO GDA-XMGTAT.
           MOVE XMGTDT       TO GDA-XMGTDT.
      *
       000199-FIM.
           EXIT.
      *
      *---------------------------------
       000200-COLETA-TRANCLASS  SECTION.
      *---------------------------------
      *
           MOVE ZEROS TO GDA-I.
      *
           EXEC CICS INQUIRE TRANCLASS START
               NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999005-ERRO-005
           END-IF.
      *
           EXEC CICS INQUIRE TRANCLASS(GDA-NM-TRANCLASS) NEXT
               RESP(GDA-RESP)
               NOHANDLE
           END-EXEC.
      *
           IF  EIBRESP NOT EQUAL ZEROS
           AND EIBRESP NOT EQUAL DFHRESP(END)
               GO TO 999006-ERRO-006
           END-IF
      *
           PERFORM WITH TEST BEFORE UNTIL GDA-RESP = DFHRESP(END)
      *
               EXEC CICS EXTRACT STATISTICS TRANCLASS
                   RESID (GDA-NM-TRANCLASS)
                   SET(ADDRESS OF DFHXMCDS)
                   NOHANDLE
               END-EXEC
      *
               IF EIBRESP NOT EQUAL ZEROS
                  GO TO 999007-ERRO-007
               END-IF
      *
      ***      GRAVA SOMENTE TOTAL DE REQUISIÇOES MAIOR QUE ZERO
      *
               IF XMCTAT GREATER THAN +0
                  MOVE XMCTQTME       TO GDA-XMCTQTME-A
                  IF XMCTQTME NOT EQUAL LOW-VALUES
      ***            Média de tempo total na fila
                     INITIALIZE S7053-AREA
      *
                     MOVE XMCTQTME    TO S7053-STCK
                     PERFORM 800000-CONVERTE-TOD-TIMESTAMP
                     MOVE GDA-TMP-CPU TO GDA-XMCTQTME-N
                  END-IF
      *
                  MOVE XMCCQTME       TO GDA-XMCCQTME-A
                  IF XMCCQTME NOT EQUAL LOW-VALUES
      ***            MéDIA DE TEMPO CORRENTE NA FILA
                     INITIALIZE S7053-AREA
      *
                      MOVE XMCCQTME    TO S7053-STCK
                      PERFORM 800000-CONVERTE-TOD-TIMESTAMP
                      MOVE GDA-TMP-CPU TO GDA-XMCCQTME-N
                   END-IF
      *
                   PERFORM 300000-GRAVA-TRANCLASS
               END-IF
      *
               EXEC CICS INQUIRE TRANCLASS(GDA-NM-TRANCLASS) NEXT
                   RESP(GDA-RESP)
                   NOHANDLE
               END-EXEC
      *
               IF  EIBRESP NOT EQUAL ZEROS
               AND EIBRESP NOT EQUAL DFHRESP(END)
                   GO TO 999008-ERRO-008
               END-IF
           END-PERFORM.
      *
           EXEC CICS INQUIRE TRANCLASS END
               NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999009-ERRO-009
           END-IF.
      *
       000299-FIM.
           EXIT.
      *
      *---------------------------------
       000300-COLETA-DISPACHER  SECTION.
      *---------------------------------
      *
           EXEC CICS EXTRACT STATISTICS DISPATCHER
               SET(ADDRESS OF DFHDSGDS)
               NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999010-ERRO-010
           END-IF.
      *
           MOVE DSGEJST        TO GDA-DSGEJST-A
           IF DSGEJST NOT EQUAL LOW-VALUES
      ***     ADDRESS SPACE CPU TIME
              INITIALIZE S7053-AREA
      *
              MOVE DSGEJST     TO S7053-STCK
              PERFORM 800000-CONVERTE-TOD-TIMESTAMP
              MOVE GDA-TMP-CPU TO GDA-DSGEJST-I
           END-IF
      *
           MOVE DSGSRBT        TO GDA-DSGSRBT-A
           IF DSGSRBT NOT EQUAL LOW-VALUES
      ***     ADDRESS SPACE SRB TIME
              INITIALIZE S7053-AREA
      *
              MOVE DSGSRBT     TO  S7053-STCK
              PERFORM 800000-CONVERTE-TOD-TIMESTAMP
              MOVE GDA-TMP-CPU TO GDA-DSGSRBT-I
           END-IF.
      *
       000399-FIM.
           EXIT.
      *
      *-----------------------------------*
       000400-COLETA-DISPACHER-TCB SECTION.
      *-----------------------------------*
      *
           MOVE 1 TO GDA-I.
      *
           PERFORM WITH TEST BEFORE UNTIL GDA-I > DSGASIZE
      *
      ****     GRAVA SOMENTE "QR"
      *
               EVALUATE DSGTCBNM(GDA-I)
                   WHEN 'QR'
                        MOVE DSGTCBNM(GDA-I) TO GDA-DSGTCBNM
                        MOVE DSGTCBCA(GDA-I) TO GDA-DSGTCBCA
                        MOVE DSGTCBPA(GDA-I) TO GDA-DSGTCBPA
                        MOVE DSGNTCBA(GDA-I) TO GDA-DSGNTCBA
                        MOVE DSGTCBAF(GDA-I) TO GDA-DSGTCBAF
                        MOVE DSGSYSW(GDA-I)  TO GDA-DSGSYSW
      *
                        MOVE DSGTWT(GDA-I)   TO GDA-DSGTWT-A
                        IF GDA-DSGTWT-A NOT EQUAL LOW-VALUES
      ***                  OP. SYSTEM WAIT TIME
                           INITIALIZE S7053-AREA
      *
                           MOVE GDA-DSGTWT-A TO S7053-STCK
                           PERFORM 800000-CONVERTE-TOD-TIMESTAMP
                           MOVE GDA-TMP-CPU TO GDA-DSGTWT-N
                        END-IF
      *
                        MOVE DSGTDT(GDA-I)   TO GDA-DSGTDT-A
                        IF GDA-DSGTDT-A NOT EQUAL LOW-VALUES
      ***                  TOTAL TCB DISPATCH TIME
                           INITIALIZE S7053-AREA
      *
                           MOVE GDA-DSGTDT-A TO S7053-STCK
                           PERFORM 800000-CONVERTE-TOD-TIMESTAMP
                           MOVE GDA-TMP-CPU TO GDA-DSGTDT-N
                        END-IF
      *
                        MOVE DSGACT(GDA-I)   TO GDA-DSGACT-A
                        IF GDA-DSGACT-A    NOT EQUAL LOW-VALUES
      ***                  TOTAL TCB CPU TIME
                           INITIALIZE S7053-AREA
      *
                           MOVE GDA-DSGACT-A  TO S7053-STCK
                           PERFORM 800000-CONVERTE-TOD-TIMESTAMP
                           MOVE GDA-TMP-CPU TO GDA-DSGACT-N
                        END-IF
      *
                        PERFORM 400000-GRAVA-DISPATCHER-TCB
               END-EVALUATE
      *
               ADD 1 TO GDA-I
           END-PERFORM.
      *
       000499-FIM.
           EXIT.
      *
      *------------------------------*
       000500-COLETA-TCB-POOL SECTION.
      *------------------------------*
           MOVE 1 TO GDA-I.
      *
           PERFORM WITH TEST BEFORE UNTIL GDA-I > DSGPSIZE
      ***     SOMENTE POOL=OPEN
              IF DSG-TCB-POOL-OPEN(GDA-I)
      ***        PEAK REQUESTS DELAYED BY MAX TCB POOL LIMIT
                 MOVE DSGPEANW(GDA-I) TO GDA-DSGPEANW
              END-IF
      *
              ADD 1 TO GDA-I
           END-PERFORM.
      *
       000599-FIM.
           EXIT.
      *
      *--------------------------
       000600-COLETA-TS  SECTION.
      *--------------------------
      *
           EXEC CICS EXTRACT STATISTICS TSQUEUE
               SET(ADDRESS OF DFHTSGDS)
               NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999012-ERRO-012
           END-IF.
      *
      ***  CURRENT TSMAINLIMIT SETTING (IN BYTES)
           MOVE 'GDA-TSGTSMLM' TO GDA-ERRO-COMPUTE.
           COMPUTE GDA-TSGTSMLM = TSGTSMLM / 1024
              ON SIZE ERROR GO TO 901000-TRATA-ERRO-COMPUTE
           END-COMPUTE.
      *
      ***  CURRENT STORAGE USED FOR TSMAINLIMIT (IN BYTES)
           MOVE 'GDA-TSGTSMUS' TO GDA-ERRO-COMPUTE.
           COMPUTE GDA-TSGTSMUS = TSGTSMUS / 1024
              ON SIZE ERROR GO TO 901000-TRATA-ERRO-COMPUTE
           END-COMPUTE.
      *
       000699-FIM.
           EXIT.
      *
      *------------------------------*
       000700-COLETA-DUMP     SECTION.
      *------------------------------*
      *
           EXEC CICS EXTRACT STATISTICS SYSDUMPCODE
                SET(ADDRESS OF DFHSDGDS)
                NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999013-ERRO-013
           END-IF.
      *
      ***  SYSTEM DUMPS
      ***  SYSTEM DUMPS SUPPRESSED
           MOVE SDGSDREQ TO GDA-SDGSDREQ.
           MOVE SDGSDSUP TO GDA-SDGSDSUP.
      *
           EXEC CICS EXTRACT STATISTICS TRANDUMPCODE
                SET(ADDRESS OF DFHTDGDS)
                NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999014-ERRO-014
           END-IF.
      *
      ***  TRANSACTION DUMPS
      ***  TRANSACTION DUMPS SUPPRESSED
           MOVE SDGTDREQ TO GDA-SDGTDREQ.
           MOVE SDGTDSUP TO GDA-SDGTDSUP.
      *
       000799-FIM.
           EXIT.
      *
      *------------------------------*
       000800-COLETA-DB2CONN  SECTION.
      *------------------------------*
      *
           EXEC CICS EXTRACT STATISTICS DB2CONN
                SET(ADDRESS OF DFHD2GDS)
                NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999015-ERRO-015
           END-IF.
      *
      ***  POOL THREAD LIMIT
      ***  PEAK NUMBER OF POOL THREADS
           MOVE D2G-POOL-THREAD-LIMIT TO GDA-D2G-POOL-THREAD-LIMIT.
           MOVE D2G-POOL-THREAD-HWM   TO GDA-D2G-POOL-THREAD-HWM.
      *
       000899-FIM.
           EXIT.
      *
      *----------------------------------
       000900-COLETA-CONNECTION  SECTION.
      *----------------------------------
      *
           MOVE ZEROS TO GDA-I.
      *
           EXEC CICS INQUIRE CONNECTION START
               NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999016-ERRO-016
           END-IF.
      *
           EXEC CICS INQUIRE CONNECTION(CONNECTION-NAME) NEXT
                ACCESSMETHOD(ACCESS-METHOD)
                CONNTYPE(CONNECTION-TYPE)
                NETNAME(CONNECTION-NETNAME)
                PROTOCOL(PROTOCOL)
                RECEIVECOUNT(RECEIVE-COUNT)
                SENDCOUNT(SEND-COUNT)
                NOHANDLE
           END-EXEC.
      *
           IF  EIBRESP NOT EQUAL ZEROS
           AND EIBRESP NOT EQUAL DFHRESP(END)
               GO TO 999017-ERRO-017
           END-IF.
      *
           PERFORM WITH TEST BEFORE UNTIL EIBRESP = DFHRESP(END)
               EXEC CICS COLLECT STATISTICS CONNECTION(CONNECTION-NAME)
                    SET(ADDRESS OF DFHA14DS)
                    NOHANDLE
               END-EXEC
      *
               IF  EIBRESP NOT EQUAL ZEROS
                   GO TO 999018-ERRO-018
               END-IF
      *
               ADD 1 TO GDA-I
      *
               IF A14ESTAO GREATER 1
               OR A14EALRJ GREATER 1
      ***         FAILED ALLOCATES DUE TO SESSIONS IN USE
      ***         NUMBER OF QUEUELIMIT ALLOCATES REJECTED
                  PERFORM 500000-GRAVA-CONNECTION
               END-IF
      *
               EXEC CICS INQUIRE CONNECTION(CONNECTION-NAME) NEXT
                   ACCESSMETHOD(ACCESS-METHOD)
                   CONNTYPE(CONNECTION-TYPE)
                   NETNAME(CONNECTION-NETNAME)
                   PROTOCOL(PROTOCOL)
                   RECEIVECOUNT(RECEIVE-COUNT)
                   SENDCOUNT(SEND-COUNT)
                   NOHANDLE
               END-EXEC
      *
               IF  EIBRESP NOT EQUAL ZEROS
               AND EIBRESP NOT EQUAL DFHRESP(END)
                   GO TO 999019-ERRO-019
               END-IF
           END-PERFORM.
      *
           EXEC CICS INQUIRE CONNECTION END
               NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999020-ERRO-020
           END-IF.
      *
       009999-FIM.
           EXIT.
      *
      *--------------------------------
       001000-COLETA-TRANS-ID  SECTION.
      *--------------------------------
      *
           MOVE ZEROS TO GDA-I.
      *
           EXEC CICS INQUIRE TRANSACTION START
               NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999021-ERRO-021
           END-IF.
      *
           EXEC CICS INQUIRE TRANSACTION(GDA-NM-TRANSACTION) NEXT
               RESP(GDA-RESP)
               NOHANDLE
           END-EXEC.
      *
           IF  EIBRESP NOT EQUAL ZEROS
           AND EIBRESP NOT EQUAL DFHRESP(END)
               GO TO 999022-ERRO-022
           END-IF
      *
           PERFORM WITH TEST BEFORE UNTIL GDA-RESP = DFHRESP(END)
      *
               EXEC CICS EXTRACT STATISTICS TRANSACTION
                   RESID (GDA-NM-TRANSACTION)
                   SET(ADDRESS OF DFHXMRDS)
                   NOHANDLE
               END-EXEC
      *
               IF EIBRESP NOT EQUAL ZEROS
                  GO TO 999023-ERRO-023
               END-IF
      *
               IF XMRAC > 0
      ***         ATTACH COUNT > 0
                  PERFORM 600000-GRAVA-TRANSACTION
               END-IF
      *
               EXEC CICS INQUIRE TRANSACTION(GDA-NM-TRANSACTION) NEXT
                   RESP(GDA-RESP)
                   NOHANDLE
               END-EXEC
      *
               IF  EIBRESP NOT EQUAL ZEROS
               AND EIBRESP NOT EQUAL DFHRESP(END)
                   GO TO 999024-ERRO-024
               END-IF
           END-PERFORM.
      *
           EXEC CICS INQUIRE TRANSACTION END
               NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999025-ERRO-025
           END-IF.
      *
       001099-FIM.
           EXIT.
      *
      *-------------------------------
       001100-COLETA-STORAGE  SECTION.
      *-------------------------------
      *
           EXEC CICS EXTRACT STATISTICS STORAGE
               SET(ADDRESS OF DFHSMSDS)
               NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999032-ERRO-032
           END-IF.
      *
      ***  Current DSA Limit
           MOVE 'GDA-SMSDSALIMIT' TO GDA-ERRO-COMPUTE.
           COMPUTE GDA-SMSDSALIMIT = SMSDSALIMIT / 1024
              ON SIZE ERROR GO TO 901000-TRATA-ERRO-COMPUTE
           END-COMPUTE.
      *
      ***  CURRENT DSA TOTAL
           MOVE 'GDA-SMSDSATOTAL' TO GDA-ERRO-COMPUTE.
           COMPUTE GDA-SMSDSATOTAL = SMSDSATOTAL / 1024
              ON SIZE ERROR GO TO 901000-TRATA-ERRO-COMPUTE
           END-COMPUTE.
      *
      ***  CURRENT EDSA LIMIT
           MOVE 'GDA-SMSEDSALIMIT' TO GDA-ERRO-COMPUTE.
           COMPUTE GDA-SMSEDSALIMIT = SMSEDSALIMIT / 1024
              ON SIZE ERROR GO TO 901000-TRATA-ERRO-COMPUTE
           END-COMPUTE.
      *
      ***  CURRENT EDSA TOTAL
           MOVE 'GDA-SMSEDSATOTAL' TO GDA-ERRO-COMPUTE.
           COMPUTE GDA-SMSEDSATOTAL = SMSEDSATOTAL / 1024
              ON SIZE ERROR GO TO 901000-TRATA-ERRO-COMPUTE
           END-COMPUTE.
      *
       001199-FIM.
           EXIT.
      *
      *--------------------------------
       200000-GRAVA-ETTC-CICS  SECTION.
      *--------------------------------
      *
           MOVE GDA-APPLID                TO K0030-NM-CICS
           MOVE GDA-DATA                  TO K0030-DT-CLA
           MOVE GDA-HORA                  TO K0030-HR-CLA
           MOVE TOTAL-AUX                 TO K0030-QT-TRAN-EXE
           MOVE GDA-XMGMXT                TO K0030-LIM-MAX-TRAN
           MOVE GDA-XMGTAMXT              TO K0030-LIM-MAX-TRAN-ATGD
           MOVE GDA-XMGTDT                TO K0030-MAX-TRAN-FILA
           MOVE GDA-XMGPAT                TO K0030-MAX-TRAN-EXEA-SMTO
           MOVE GDA-XMGTAT                TO K0030-QT-TRAN-USU-EXEA
           MOVE GDA-DSGEJST-I             TO K0030-TMP-CPU-ESP-END
           MOVE GDA-DSGSRBT-I             TO K0030-TMP-BLOC-REQ-SRVC
           MOVE DSGXSCNS                  TO K0030-NUM-EXMR-EXC
           MOVE GDA-DSGPEANW              TO K0030-MAX-REQ-EPR-ABTO
           MOVE GDA-TSGTSMLM              TO K0030-LIM-MMR-AMZT-PRVR
           MOVE GDA-TSGTSMUS              TO K0030-UTZO-MMR-AMZT-PRVR
           MOVE GDA-SDGSDREQ              TO K0030-QT-DUMP-CRIC
           MOVE GDA-SDGSDSUP              TO K0030-QT-DUMP-OCTR
           MOVE GDA-SDGTDREQ              TO K0030-QT-DUMP-TRAN-CRIC
           MOVE GDA-SDGTDSUP              TO K0030-QT-DUMP-TRAN-OCTR
           MOVE GDA-D2G-POOL-THREAD-LIMIT TO K0030-LIM-TRAN-DB2-POOL
           MOVE GDA-D2G-POOL-THREAD-HWM   TO K0030-MAX-TRAN-DB2-UTZD
           MOVE GDA-XMGTDT                TO K0030-TTL-TRAN-FILA
           MOVE D2G-POOL-THREAD-CURRENT   TO K0030-QT-ATU-TRAN-DB2
           MOVE 0                         TO K0030-QT-TRAN-USU-MTNO
           MOVE GDA-SMSDSALIMIT           TO K0030-QT-DSA-LIM
           MOVE GDA-SMSDSATOTAL           TO K0030-QT-TTL-DSA-LIM
           MOVE GDA-SMSEDSALIMIT          TO K0030-QT-EDSA-LIM
           MOVE GDA-SMSEDSATOTAL          TO K0030-QT-TTL-EDSA-LIM.
      *
           MOVE 30                        TO K0030-CD-MSG
           MOVE K0030-CD-MSG              TO GDA-CD-TIP-MSG-MQ.
           PERFORM 700000-PUT-FILA-MQ.
      *
       200999-FIM.
           EXIT.
      *
      *--------------------------------
       300000-GRAVA-TRANCLASS  SECTION.
      *--------------------------------
      *
           MOVE GDA-APPLID       TO K0031-NM-CICS
           MOVE GDA-NM-TRANCLASS TO K0031-NM-CLS-TRAN
           MOVE GDA-DATA         TO K0031-DT-CLA
           MOVE GDA-HORA         TO K0031-HR-CLA
           MOVE XMCMXT           TO K0031-LIM-MAX-TRAN
           MOVE XMCTAT           TO K0031-TTL-RQSC
           MOVE XMCTQ            TO K0031-TTL-TRAN-FILA
           MOVE GDA-XMCTQTME-N   TO K0031-MED-TMP-FILA
           MOVE XMCPAT           TO K0031-MAX-TRAN-EXEA-SMTO
           MOVE XMCPQT           TO K0031-MAX-TRAN-FILA
           MOVE XMCTAMA          TO K0031-LIM-MAX-TRAN-ATGD
           MOVE XMCCQT           TO K0031-QT-TRAN-FILA-ATU
           MOVE GDA-XMCCQTME-N   TO K0031-MED-TMP-FILA-ATU
           MOVE XMCPI            TO K0031-QT-TRAN-FORA-FILA
      *
           MOVE 31               TO K0031-CD-MSG
           MOVE K0031-CD-MSG     TO GDA-CD-TIP-MSG-MQ.
           PERFORM 700000-PUT-FILA-MQ.
      *
       300099-SAI.
           EXIT.
      *
      *-------------------------------------
       400000-GRAVA-DISPATCHER-TCB  SECTION.
      *-------------------------------------
      *
           MOVE GDA-APPLID                TO K0035-NM-CICS
           MOVE GDA-DSGTCBNM              TO K0035-NM-BLOC-CTL-TRF
           MOVE GDA-DATA                  TO K0035-DT-CLA
           MOVE GDA-HORA                  TO K0035-HR-CLA
           MOVE GDA-DSGTCBCA              TO K0035-QT-ATU-UTZD
           MOVE GDA-DSGTCBPA              TO K0035-MAX-UTZD
           MOVE GDA-DSGNTCBA              TO K0035-TTL-UTZD
           MOVE GDA-DSGTCBAF              TO K0035-QT-ERRO-RQSC
           MOVE GDA-DSGSYSW               TO K0035-QT-EPR-SIS
           MOVE GDA-DSGTWT-N              TO K0035-TMP-TTL-EPR-SIS
           MOVE GDA-DSGTDT-N              TO K0035-TMP-TTL-EFT-UTZD
           MOVE GDA-DSGACT-N              TO K0035-TMP-TTL-DSPD
           MOVE 0                         TO K0035-CD-EST-BLOC-CTL-TR
           MOVE 0                         TO K0035-QT-TMP-EFT-UTZD-MN
           MOVE 0                         TO K0035-QT-TMP-DSPD-MNTO
           MOVE 0                         TO K0035-PC-DPC
           MOVE 0                         TO K0035-PC-UTZO-BLOC-CTL
      *
           MOVE 35                        TO K0035-CD-MSG
           MOVE K0035-CD-MSG              TO GDA-CD-TIP-MSG-MQ.
           PERFORM 700000-PUT-FILA-MQ.
      *
       400099-FIM.
           EXIT.
      *
      *----------------------------------
       500000-GRAVA-CONNECTION  SECTION.
      *----------------------------------
      *
           MOVE GDA-APPLID      TO K0033-NM-CICS
           MOVE GDA-DATA        TO K0033-DT-CLA
           MOVE GDA-HORA        TO K0033-HR-CLA
           MOVE CONNECTION-NAME TO K0033-NM-CNXO
           MOVE A14ESTAO        TO K0033-QT-ERRO-OTR
           MOVE A14EALRJ        TO K0033-QT-ALCC-RJTD
           MOVE A14EALIM        TO K0033-QT-ALCC-LIM-FILA.
      *
           MOVE 33              TO K0033-CD-MSG
           MOVE K0033-CD-MSG    TO GDA-CD-TIP-MSG-MQ.
           PERFORM 700000-PUT-FILA-MQ.
      *
       500099-SAI.
           EXIT.
      *
      *----------------------------------
       600000-GRAVA-TRANSACTION  SECTION.
      *----------------------------------
      *
           MOVE GDA-APPLID         TO K0015-NM-CICS
           MOVE GDA-NM-TRANSACTION TO K0015-NM-TRAN
           MOVE GDA-DATA           TO K0015-DT-CLA
           MOVE GDA-HORA           TO K0015-HR-CLA
           MOVE XMRAC              TO K0015-QT-TRAN-EXEA.
      *
           MOVE 15                 TO K0015-CD-MSG
           MOVE K0015-CD-MSG       TO GDA-CD-TIP-MSG-MQ.
           PERFORM 700000-PUT-FILA-MQ.
      *
       600099-FIM.
           EXIT.
      *
      *----------------------------
       700000-PUT-FILA-MQ  SECTION.
      *----------------------------
      *
           MOVE 'QE.CIC.ESTATISTICA' TO K9011-FILA-DESTINO.
           MOVE  0                   TO K9011-IND-DESTINO.
           MOVE  0                   TO K9011-VLR-PRIORIDADE.
           MOVE  1                   TO K9011-IND-PERSIST.
           MOVE  1                   TO K9011-FORMAT.
           MOVE 'MQM'                TO K9011-USERIDENTIFIER.
      *
           EVALUATE GDA-CD-TIP-MSG-MQ
              WHEN 15
      ***          Dados da tabela ETTC_DRIA_TRAN
                   MOVE LENGTH OF CICK0015-BOOK TO K9011-TAM-DADOS
                   MOVE CICK0015-BOOK           TO K9011-DADOS
              WHEN 30
      ***          Dados da tabela ETTC_CICS
                   MOVE LENGTH OF CICK0030-BOOK TO K9011-TAM-DADOS
                   MOVE CICK0030-BOOK           TO K9011-DADOS
              WHEN 31
      ***          Dados da tabela ETTC_CLS_TRAN
                   MOVE LENGTH OF CICK0031-BOOK TO K9011-TAM-DADOS
                   MOVE CICK0031-BOOK           TO K9011-DADOS
              WHEN 33
      ***          Dados da tabela ETTC_CNXO_CICS
                   MOVE LENGTH OF CICK0033-BOOK TO K9011-TAM-DADOS
                   MOVE CICK0033-BOOK           TO K9011-DADOS
              WHEN 35
      ***          Dados da tabela ETTC_BLOC_CTL_TRAN
                   MOVE LENGTH OF CICK0035-BOOK TO K9011-TAM-DADOS
                   MOVE CICK0035-BOOK           TO K9011-DADOS
           END-EVALUATE.
      *
           EXEC CICS LINK PROGRAM    ( MQSP9011             )
                          COMMAREA   ( K9011-PARM           )
                          LENGTH     ( LENGTH OF K9011-PARM )
                          RESP       ( GDA-EIBRESP          )
                          RESP2      ( GDA-EIBRESP2         )
           END-EXEC.
      *
           IF GDA-EIBRESP NOT EQUAL DFHRESP(NORMAL)
              GO TO 999033-ERRO-033
           END-IF.
      *
           IF K9011-RET-CODE NOT EQUAL '0000'
              GO TO 999034-ERRO-034
           END-IF.
      *
       700099-SAI.
           EXIT.
      *
      *----------------------------------------
       800000-CONVERTE-TOD-TIMESTAMP   SECTION.
      *----------------------------------------
      *
           MOVE 'WATC'               TO  S7053-PARM.
           MOVE X'00'                TO  S7053-FLAG.
      *
           EXEC CICS LINK PROGRAM  ( 'MDPS7053' )
                          COMMAREA ( S7053-AREA )
                          LENGTH   ( LENGTH OF S7053-AREA )
                          NOHANDLE
           END-EXEC.
      *
           IF EIBRESP NOT EQUAL ZEROS
              GO TO 999026-ERRO-026
           END-IF.
      *
           PERFORM 801000-DESFORMATA-HORA.
      *
       800999-FIM.
           EXIT.
      *
      *--------------------------------
       801000-DESFORMATA-HORA  SECTION.
      *--------------------------------
      *
           MOVE 18                       TO IDY
           MOVE ZERO                     TO GDA-TMP-CPU
           PERFORM VARYING IDX FROM 17 BY -1 UNTIL IDX < 1
              IF  S7053-HORA(IDX:1) NUMERIC
                  MOVE S7053-HORA(IDX:1) TO GDA-TMP-CPU(IDY:1)
                  SUBTRACT 1           FROM IDY
              END-IF
           END-PERFORM.
      *
       801999-FIM.
           EXIT.
      *
      *----------------------------------
       900000-GRAVA-ERRO-SYSOUT  SECTION.
      *----------------------------------
      *
           MOVE EIBTRNID TO GDA-TRAN-SYSOUT.
           MOVE GDA-DATA TO GDA-DATA-SYSOUT.
           MOVE GDA-HORA TO GDA-HORA-SYSOUT.
           MOVE CTE-PROG TO GDA-PGM-SYSOUT.
      *
      ***  GRAVA MENSAGENS DE ERRO NA SYSOUT 'CICSSTAT'
      *    --------------------------------------------
           EXEC CICS WRITEQ TD QUEUE   ( 'CSTA' )
                               FROM    ( GDA-CICERRO )
                               LENGTH  ( LENGTH OF GDA-CICERRO )
           END-EXEC.
      *
       900999-FIM.
           EXIT.
      *
      *-----------------------------------
       901000-TRATA-ERRO-COMPUTE  SECTION.
      *-----------------------------------
      *
           STRING 'ERRO NO COMPUTE ' GDA-ERRO-COMPUTE
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING.
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT.
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       901999-FIM.
           EXIT.
      *
      *-------------------------------------
       990000-PROCEDIMENTOS-FINAIS  SECTION.
      *-------------------------------------
      *
           MOVE GDA-APPLID          TO NM-CICS
           MOVE HR-CLA-ATU-EM-SS    TO HR-CLA-ANT-EM-SS
           MOVE GDA-XMGTAT          TO QT-TRAN-USU-MIN-ANT
      *
           IF GDA-IN-TS-EXTT EQUAL 'N'
      ***     Grava TS
              EXEC CICS WRITEQ TS QNAME( GDA-NM-TS-CICS     )
                        FROM   ( GDA-DADOS-TS-CICS          )
                        LENGTH ( LENGTH OF GDA-DADOS-TS-CICS)
                        ITEM   ( GDA-ITEM                   )
                        NOHANDLE
              END-EXEC
      *
              IF EIBRESP NOT EQUAL ZEROS
                 GO TO 999028-ERRO-028
              END-IF
           ELSE
      ***     Atualiza TS
              EXEC CICS WRITEQ TS QNAME( GDA-NM-TS-CICS     )
                        FROM   ( GDA-DADOS-TS-CICS          )
                        LENGTH ( LENGTH OF GDA-DADOS-TS-CICS)
                        ITEM   ( GDA-ITEM                   )
                        REWRITE
                        NOHANDLE
              END-EXEC
      *
              IF EIBRESP NOT EQUAL ZEROS
                 GO TO 999031-ERRO-031
              END-IF
           END-IF.
      *
       990099-SAI.
           EXIT.
      *
      *----------------------
       999000-ERROS  SECTION.
      *----------------------
      *
       999001-ERRO-001.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '001 Erro ao criar a TS "' GDA-NM-TS '".'
                  ' Eibresp: '  GDA-EIBRESP
                  ' Eibresp2: ' GDA-EIBRESP2
              DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999002-ERRO-002.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '002 Erro ao ler a TS "' GDA-NM-TS '".'
                  ' Eibresp: ' GDA-EIBRESP
                  ' Eibresp2: ' GDA-EIBRESP2
              DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999003-ERRO-003.
           MOVE EIBRESP        TO GDA-EIBRESP
           MOVE EIBRESP2       TO GDA-EIBRESP2
           MOVE GDA-INTERVAL   TO GDA-INTERVAL-DISPLAY
      *
           STRING '003 Erro REQID.:' GDA-REQID
                  ' Interval..:' GDA-INTERVAL-DISPLAY
                  ' Transid...:' GDA-TRANSID
                  ' Eibresp...:' GDA-EIBRESP
                  ' Eibresp2..:' GDA-EIBRESP2
                DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999004-ERRO-004.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '004 Erro EXTRACT STATISTICS TRANSACTION. Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO   999000-FINALIZAR-PROGRAMA.
      *
       999005-ERRO-005.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '005 Erro INQUIRE TRANCLASS START. Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO   999000-FINALIZAR-PROGRAMA.
      *
       999006-ERRO-006.
              MOVE EIBRESP  TO GDA-EIBRESP
              MOVE EIBRESP2 TO GDA-EIBRESP2
      *
              STRING '006 Erro INQUIRE TRANCLASS NEXT. Tranclass = '
                     GDA-NM-TRANCLASS ' Eibresp: '
                     GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
                  DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
              END-STRING
      *
              PERFORM 900000-GRAVA-ERRO-SYSOUT
              GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999007-ERRO-007.
              MOVE EIBRESP  TO GDA-EIBRESP
              MOVE EIBRESP2 TO GDA-EIBRESP2
      *
              STRING '007 Erro EXTRACT STATISTICS Tranclass = '
                     GDA-NM-TRANCLASS ' Eibresp: '
                     GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
                  DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
              END-STRING
      *
              PERFORM 900000-GRAVA-ERRO-SYSOUT
              GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999008-ERRO-008.
              MOVE EIBRESP  TO GDA-EIBRESP
              MOVE EIBRESP2 TO GDA-EIBRESP2

              STRING '008 Erro INQUIRE TRANCLASS NEXT. Tranclass = '
                     GDA-NM-TRANCLASS ' Eibresp: '
                     GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
                  DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
              END-STRING

              PERFORM 900000-GRAVA-ERRO-SYSOUT
              GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999009-ERRO-009.
              MOVE EIBRESP  TO GDA-EIBRESP
              MOVE EIBRESP2 TO GDA-EIBRESP2
      *
              STRING '009 Erro INQUIRE TRANCLASS END. Eibresp: '
                     GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
                  DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
              END-STRING
      *
              PERFORM 900000-GRAVA-ERRO-SYSOUT
              GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999010-ERRO-010.
              MOVE EIBRESP  TO GDA-EIBRESP
              MOVE EIBRESP2 TO GDA-EIBRESP2
      *
              STRING '010 Erro EXTRACT STATISTICS DISPATCHER. Eibresp: '
                     GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
                  DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
              END-STRING
      *
              PERFORM 900000-GRAVA-ERRO-SYSOUT
              GO TO 999000-FINALIZAR-PROGRAMA.
      *
      *999011-ERRO-011.
      *
       999012-ERRO-012.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '012 Erro EXTRACT STATISTICS TSQUEUE. Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999013-ERRO-013.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '013 Erro EXTRACT STATISTICS SYSDUMPCODE. Eibresp:'
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999014-ERRO-014.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '014 Erro EXTRACT STATISTICS TRANDUMPCODE. Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999015-ERRO-015.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '015 Erro EXTRACT STATISTICS DB2CONN. Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999016-ERRO-016.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '016 Erro INQUIRE CONNECTION START. Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999017-ERRO-017.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '017 Erro INQUIRE CONNECTION = ' CONNECTION-NAME
                  ' NEXT. Eibresp: ' GDA-EIBRESP
                  ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999018-ERRO-018.
           MOVE EIBRESP TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '018 Erro COLLECT STATISTICS CONNECTION = '
                  CONNECTION-NAME ' Eibresp: ' GDA-EIBRESP
                  ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999019-ERRO-019.
           MOVE EIBRESP TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '019 Erro INQUIRE CONNECTION = ' CONNECTION-NAME
                  ' NEXT. Eibresp: ' GDA-EIBRESP
                  ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999020-ERRO-020.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '020 Erro INQUIRE CONNECTION END. Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999021-ERRO-021.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '021 Erro INQUIRE TRANSACTION START. Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO   999000-FINALIZAR-PROGRAMA.
      *
       999022-ERRO-022.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '022 Erro INQUIRE TRANSACTION NEXT. Transaction='
                  GDA-NM-TRANSACTION ' Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999023-ERRO-023.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '023 Erro EXTRACT STATISTICS Transaction='
                  GDA-NM-TRANSACTION ' Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999024-ERRO-024.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '024 Erro INQUIRE TRANSACTION NEXT. '
              'Transaction= ' GDA-NM-TRANSACTION ' Eibresp: '
               GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999025-ERRO-025.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '025 Erro INQUIRE TRANSACTION END. Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999026-ERRO-026.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '026 Erro na chamada MDPS7053. Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999027-ERRO-027.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '027 Erro ao recriar a TS "' GDA-NM-TS '".'
                  ' Eibresp: '  GDA-EIBRESP
                  ' Eibresp2: ' GDA-EIBRESP2
              DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999028-ERRO-028.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '028 Erro ao criar a TS "' GDA-NM-TS-CICS '".'
                  ' Eibresp: '  GDA-EIBRESP
                  ' Eibresp2: ' GDA-EIBRESP2
              DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999029-ERRO-029.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '029 Erro ao ler a TS "' GDA-NM-TS-CICS '".'
                  ' Eibresp: ' GDA-EIBRESP
                  ' Eibresp2: ' GDA-EIBRESP2
              DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
      *999030-ERRO-030.
      *
       999031-ERRO-031.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '031 Erro ao atualizar TS "' GDA-NM-TS-CICS '".'
                  ' Eibresp: '  GDA-EIBRESP
                  ' Eibresp2: ' GDA-EIBRESP2
              DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999032-ERRO-032.
           MOVE EIBRESP  TO GDA-EIBRESP
           MOVE EIBRESP2 TO GDA-EIBRESP2
      *
           STRING '032 Erro EXTRACT STATISTICS STORAGE. Eibresp: '
                  GDA-EIBRESP ' Eibresp2: ' GDA-EIBRESP2
               DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999033-ERRO-033.
           STRING '033 Erro na chamada MQSP9011.'
                  ' Eibresp: '  GDA-EIBRESP
                  ' Eibresp2: ' GDA-EIBRESP2
                  ' Tipo de mensagem: ' GDA-CD-TIP-MSG-MQ
              DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
       999034-ERRO-034.
           STRING '034 Erro na gravação da fila MQ'
                  ' Retorno: '  K9011-RET-CODE
                  ' Tipo de mensagem: ' GDA-CD-TIP-MSG-MQ
              DELIMITED BY SIZE     INTO GDA-TX-ERRO-LIVRE
           END-STRING
      *
           PERFORM 900000-GRAVA-ERRO-SYSOUT
           GO TO 999000-FINALIZAR-PROGRAMA.
      *
      *-----------------------------------
       999000-FINALIZAR-PROGRAMA  SECTION.
      *-----------------------------------
      *
           IF GDA-TRAN-SYSOUT EQUAL SPACES
              IF GDA-IN-COLETA EQUAL 'S'
                 MOVE GDA-INTERVALO-START TO GDA-INTERVALO-DISPLAY
                 STRING 'COLETA EFETUADA COM SUCESSO. '
                        'PROXIMA COLETA EM '
                         GDA-INTERVALO-DISPLAY(1:2) 'h'
                         GDA-INTERVALO-DISPLAY(3:2) 'm'
                         GDA-INTERVALO-DISPLAY(5:2) 's'
                   DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
                 END-STRING
              ELSE
                 MOVE GDA-INTERVAL        TO GDA-INTERVAL-DISPLAY
                 STRING 'AGUARDANDO NOVA COLETA. '
                        'PROXIMA COLETA EM '
                         GDA-INTERVAL-DISPLAY(1:2) 'h'
                         GDA-INTERVAL-DISPLAY(3:2) 'm'
                         GDA-INTERVAL-DISPLAY(5:2) 's'
                   DELIMITED BY SIZE INTO GDA-TX-ERRO-LIVRE
                 END-STRING
              END-IF
      *
              PERFORM 900000-GRAVA-ERRO-SYSOUT
           END-IF.
      *
           EXEC CICS RETURN
              NOHANDLE
           END-EXEC.
      *
       999999-FIM.
           EXIT.
      *
