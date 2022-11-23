       CBL CICS('COBOL3') APOST
      ******************************************************************
      * Valter Siqueira - Systems
      * Laboratoratório de uso particular
      * ----------------------------------------------------------------
      * Sistema .............. CEP
      * Programa.............. CEPCICS3
      * Tipo    .............. Online
      * Finalidade ........... realizar inclusao de registro no file
      *                        "CEPVSA01"
      *                        Recebe informacoes pela commarea
      *                        Desenvolvido para atender zCEE
      * DSnames .............. B090290.CEPVSA01
      * JOB def cluster ...... B090290.LIB.JCL(CEPDFCLU)
      * Transacao CICS  ...... KEP0
      *
      ******************************************************************
       identification division.
       program-id.    cepcics3.
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
          03 wk-dis-td-erro.
             05 filler pic x(010) value       spaces.
             05 filler pic x(018) value       'Erro na transacao '.
             05 wk-dis-eibtrnid               pic  x(004).
             05 filler pic x(008) value       spaces.
             05 filler pic x(008) value       'eibresp:'.
             05 wk-dis-eibresp                pic  9(004).
             05 filler pic x(018) value       spaces.
             05 filler pic x(009) value       'eibresp2:'.
             05 wk-dis-eibresp2               pic  9(004).
             05 filler pic x(049) value       spaces.


      *----------------------------------------------------------------*
      *   GENERIC WORK VARIABLES                                       *
      *----------------------------------------------------------------*
          03 wk-eibresp                       pic s9(8) comp sync.
          03 wk-eibresp2                      pic s9(8) comp sync.
      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       linkage section.

       01 dfhcommarea.
          03 lk-cep               pic  x(008).
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

           move dfhcommarea to wk-cepv0001-rec

      *    move spaces to wk-cepv0001-rec
      *    move spaces to wk-kep0td-rec

           exec cics write
                file      ( 'CEPVSA01' )
                ridfld    ( wk-cepv0001-code )
                keylength ( 8 )
                length    ( length of wk-cepv0001-rec )
                from      ( wk-cepv0001-rec )
                resp      ( wk-eibresp )
                resp2     ( wk-eibresp2 )
           end-exec

           if wk-eibresp not equal zeros
              move eibtrnid
                   to wk-dis-eibtrnid
              move wk-eibresp
                   to wk-dis-eibresp
              move wk-eibresp2
                   to wk-dis-eibresp2

              exec cics writeq
                   td queue ( 'KEP0' )
                   from     ( wk-dis-td-erro )
                   length   ( length of wk-dis-td-erro )
                   resp     ( wk-eibresp )
              end-exec

              move wk-dis-td-erro
                   to dfhcommarea
           else
              move spaces
                   to dfhcommarea
              move wk-cepv0001-rec
                   to dfhcommarea
              move wk-cepv0001-rec
                   to wk-kep0td-rec-cpy

              exec cics writeq
                   td queue ( 'KEP0' )
                   from     ( wk-kep0td-rec )
                   length   ( length of wk-kep0td-rec )
                   resp     ( wk-eibresp )
              end-exec

           end-if

           exec cics return
           end-exec
           .