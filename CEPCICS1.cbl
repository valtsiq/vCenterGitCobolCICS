       CBL CICS('COBOL3') APOST
      ******************************************************************
      * Valter Siqueira - Systems
      * Laboratoratório de uso particular
      * ----------------------------------------------------------------
      * Sistema .............. CEP
      * Programa.............. CEPCICS1
      * Tipo    .............. Online
      * Finalidade ........... realizar pesquisa de CEP no VSAM
      *                        "CEPVSA01"
      *                        Recebe informacoes pela commarea
      *                        Desenvolvido para atender zCEE
      * DSnames .............. B090290.CEPVSA01
      * JOB def cluster ...... B090290.LIB.JCL(CEPDFCLU)
      * Transacao CICS  ...... KEP0
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CEPCICS1.
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       DATA           DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*

       01 WORKING-ALL-FILLS.
          03 WK-CEPV0001-REC.
             05 WK-CEPV0001-CODE              PIC  X(008).
             05 WK-CEPV0001-UF                PIC  X(002).
             05 WK-CEPV0001-CIDADE            PIC  X(030).
             05 WK-CEPV0001-BAIRRO            PIC  X(030).
             05 WK-CEPV0001-LOGRADOURO        PIC  X(030).
          03 FILLER  PIC X(009) VALUE         '---------'.
          03 WK-KEP0TD-REC.
             05 WK-KEP0TD-REC-CPY             PIC  X(100).
             05 WK-KEP0TD-REC-MSG             PIC  X(032).
      *----------------------------------------------------------------*
      *   GENERIC WORK VARIABLES                                       *
      *----------------------------------------------------------------*
          03 WK-EIBRESP                       PIC S9(9) COMP-5 SYNC.
      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.

       01 DFHCOMMAREA.
          03 LK-CEP               PIC  X(008).
          03 LK-UF                PIC  X(002).
          03 LK-CIDADE            PIC  X(030).
          03 LK-BAIRRO            PIC  X(030).
          03 LK-LOGRADOURO        PIC  X(030).

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.
      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*

      *    move dfhcommarea to wk-cepv0001-rec

           MOVE SPACES TO WK-CEPV0001-REC
           MOVE SPACES TO WK-KEP0TD-REC

           EXEC CICS READ FILE('CEPVSA01')
                          RIDFLD(LK-CEP)
                          KEYLENGTH(8)
                          INTO(WK-CEPV0001-REC)
                          RESP(WK-EIBRESP)
           END-EXEC

           IF WK-EIBRESP NOT EQUAL ZEROS
              MOVE LK-CEP
                   TO WK-CEPV0001-CODE
              MOVE 'CEP não encontrado       '
                   TO WK-CEPV0001-CIDADE
           END-IF
           IF LK-CEP EQUAL '09041160'
              MOVE LK-CEP
                   TO WK-CEPV0001-CODE
              MOVE 'CEP nao encontrado       '
                   TO WK-CEPV0001-CIDADE
              MOVE 'CEP nao encontrado       '
                   TO WK-CEPV0001-BAIRRO
              MOVE 'CEP nao encontrado       '
                   TO WK-CEPV0001-LOGRADOURO
              MOVE SPACES
                   TO WK-CEPV0001-UF
           END-IF
           MOVE SPACES          TO DFHCOMMAREA
           MOVE WK-CEPV0001-REC TO DFHCOMMAREA

           MOVE WK-CEPV0001-REC TO WK-KEP0TD-REC-CPY

           EXEC CICS WRITEQ TD QUEUE('KEP0')
                          FROM (WK-KEP0TD-REC)
                          LENGTH(LENGTH OF WK-KEP0TD-REC)
                          RESP(WK-EIBRESP)
           END-EXEC

           EXEC CICS RETURN
           END-EXEC
           .