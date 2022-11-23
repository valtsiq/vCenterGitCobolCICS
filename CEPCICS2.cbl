       CBL CICS('COBOL3') APOST
      ******************************************************************
      * Valter Siqueira - Systems
      * Laboratoratório de uso particular
      * ----------------------------------------------------------------
      * Sistema .............. CEP
      * Programa.............. CEPCICS2
      * Tipo    .............. Online
      * Finalidade ........... Simular programas que realizam chamadas
      *                        a rotina para consulta de enderecos
      *                        por CEP.
      * Transacao CICS  ...... KEP2
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cepcics2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Work area                                                     *
      *----------------------------------------------------------------*
        01 wk-work.
           03 wk-rec-length        pic s9(4) comp value 240.
           03 wk-sen-length        pic s9(4) comp value 240.
           03 wk-rec-cep.
              05 filler                pic x(0005).
              05 wk-rec-cep-code       pic x(0008).
              05 filler                pic x(0228).
           03 wk-sen-cep.
               05 filler                pic x(0080) value all '-'.
               05 filler.
                 07 filler                  pic x(0015) value
                                            'Codigo ........'.
                 07 wk-sen-cep-code         pic x(0008) value spaces.
                 07 filler                  pic x(0057) value spaces.
              05 filler.
                 07 filler                  pic x(0015) value
                                            'UF ............'.
                 07 wk-sen-cep-uf           pic x(0002) value spaces.
                 07 filler                  pic x(0063) value spaces.
              05 filler.
                 07 filler                  pic x(0015) value
                                            'Cidade ........'.
                 07 wk-sen-cep-cidade       pic x(0030) value spaces.
                 07 filler                  pic x(0035) value spaces.
              05 filler.
                 07 filler                  pic x(0015) value
                                            'Bairro ........'.
                 07 wk-sen-cep-bairro       pic x(0030) value spaces.
                 07 filler                  pic x(0035) value spaces.
              05 filler.
                 07 filler                  pic x(0015) value
                                            'Logradouro ....'.
                 07 wk-sen-cep-logradouro   pic x(0030) value spaces.
                 07 filler                  pic x(0035) value spaces.
              05 filler                pic x(0080) value all '-'.

           03 wk-com-cep.
              05 wk-com-cep-code       pic x(0008).
              05 wk-com-cep-uf         pic x(0002).
              05 wk-com-cep-cidade     pic x(0030).
              05 wk-com-cep-bairro     pic x(0030).
              05 wk-com-cep-logradouro pic x(0030).
           03 wk-resp                  pic s9(009) COMP-5 SYNC.

      *----------------------------------------------------------------*
      *    L I N K A G E   S E C T I O N
      *----------------------------------------------------------------*

       linkage section.
       01 dfhcommarea                  pic x(0010) value
                                       '#commarea#'.

      *----------------------------------------------------------------*
      *p r o c e d u r e s
      *----------------------------------------------------------------*

       procedure division.

      *----------------------------------------------------------------*
       mainline section.
      *----------------------------------------------------------------*
           initialize wk-rec-cep
           initialize wk-com-cep

           exec cics receive into   (wk-rec-cep)
                             length (length of wk-rec-cep)
           end-exec
      * -----------------------------------------------------------
      *    Ligar as areas de memoria que devem ser enviadas no POST
      * -----------------------------------------------------------
           move wk-rec-cep-code     to wk-com-cep-code

           exec cics link program ('CEPCICS1')
                     commarea     (wk-com-cep)
                     length       (length of wk-com-cep)
                     resp         (wk-resp)
           end-exec

           if wk-resp = 0
              move wk-com-cep-code       to wk-sen-cep-code
              move wk-com-cep-uf         to wk-sen-cep-uf
              move wk-com-cep-cidade     to wk-sen-cep-cidade
              move wk-com-cep-bairro     to wk-sen-cep-bairro
              move wk-com-cep-logradouro to wk-sen-cep-logradouro
              exec cics send from(wk-sen-cep)
                        length (length of wk-sen-cep)
                        resp (wk-resp)
                        erase
                        end-exec
           else
              move 'Estamos com problemas' to
                   wk-sen-cep-cidade
              exec cics send from(wk-sen-cep)
                        length (length of wk-sen-cep)
                        resp (wk-resp)
                        erase
                        end-exec

           end-if

           exec cics return
           end-exec
           .