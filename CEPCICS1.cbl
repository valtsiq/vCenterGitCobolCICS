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
       identification division.
       program-id.    cepcics1.
       environment    division.
       configuration  section.
       data           division.
       working-storage section.
      *----------------------------------------------------------------*

       01 working-all-fills.
          03 wk-cepv0001-rec.
             05 wk-cepv0001-code              PIC  X(008).
             05 wk-cepv0001-uf                PIC  X(002).
             05 wk-cepv0001-cidade            PIC  X(030).
             05 wk-cepv0001-bairro            PIC  X(030).
             05 wk-cepv0001-logradouro        PIC  X(030).
          03 filler  pic x(009) value         '---------'.
          03 wk-kep0td-rec.
             05 wk-kep0td-rec-cpy             pic  x(100).
             05 wk-kep0td-rec-msg             pic  x(032).
      *----------------------------------------------------------------*
      *   GENERIC WORK VARIABLES                                       *
      *----------------------------------------------------------------*
          03 wk-eibresp                       PIC S9(9) COMP-5 SYNC.
      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       linkage section.

       01 dfhcommarea.
          03 lk-cep               pic  X(008).
          03 lk-uf                PIC  X(002).
          03 lk-cidade            PIC  X(030).
          03 lk-bairro            PIC  X(030).
          03 lk-logradouro        PIC  X(030).

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       procedure division.

      *----------------------------------------------------------------*
       mainline section.
      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*

      *    move dfhcommarea to wk-cepv0001-rec

           move spaces to wk-cepv0001-rec
           move spaces to wk-kep0td-rec

           exec cics read file('CEPVSA01')
                          ridfld(lk-cep)
                          keylength(length of lk-cep)
                          into(wk-cepv0001-rec)
                          resp(wk-eibresp)
           end-exec

           if wk-eibresp not equal zeros
              move lk-cep
                   to wk-cepv0001-code
              move 'CEP não encontrado       '
                   to wk-cepv0001-cidade
           end-if
           move spaces          to dfhcommarea
           move wk-cepv0001-rec to dfhcommarea

           move wk-cepv0001-rec to wk-kep0td-rec-cpy

           exec cics writeq td queue('KEP0')
                          from (wk-kep0td-rec)
                          length(length of wk-kep0td-rec)
                          resp(wk-eibresp)
           end-exec

           exec cics return
           end-exec
           .