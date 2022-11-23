       CBL CICS('COBOL3') APOST
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XPTO9998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      ***                  BACK-END MAINFRAME (DRV)                  ***
      ***         LAYOUT DE COMUNICACAO CONTROLADOR X DRIVER         ***
      ***         ==========================================         ***
      ***                                                            ***
      ***   RQ02     - BOOK DRIVER RECEIVE DO CONTROLADOR            ***
      ***   BOOK     - DRVWRQ02                                      ***
      ***   WORK     - DRVWRQ02                                      ***
      ***   TAM.REG. - 4096 BYTES                                    ***
      ***                                                            ***
      ***------------------------------------------------------------***
      ***                    DESCRICAO DOS CAMPOS                    ***
      ***------------------------------------------------------------***
      *** CAMPO                         | DESCRICAO                  ***
      ***------------------------------------------------------------***
      ***                      DADOS DE REQUISICAO                   ***
      ***-------------------------------+----------------------------***
      *** RQ02-REQU-COD-TRANS           | CODIGO DA TRANSACAO NO CICS***
      ***-------------------------------+----------------------------***
      *** RQ02-REQU-CD-APLI-CONS        | COD APLICACAO CONSUMIDORA  ***
      ***-------------------------------+----------------------------***
      *** RQ02-REQU-COD-GRP-SVC         | CODIGO DO GRUPO DE SERVICO ***
      ***-------------------------------+----------------------------***
      *** RQ02-REQU-COD-SVC             | CODIGO DO SERVICO          ***
      ***-------------------------------+----------------------------***
      *** RQ02-REQU-MSG-ID              | INDICADOR MENSAGEM (MSG-ID)***
      ***-------------------------------+----------------------------***
      *** RQ02-CONT-TAM-AREA-NEG        | TAMANHO TOTAL AREA DE NEGO-***
      ***                               | CIO EM BYTES               ***
      ***------------------------------------------------------------***
      ***                       DADOS CREDENCIAL                     ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-ID-TP-CRE           | IDENTIFICAO TIPO CREDENCIAL***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-SESSAO              | SESSAO                     ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-DADOS               | DADOS                      ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-COD-ID-MOD-ACESSO-AD| CODIGO DE IDENTIFICACAO DO ***
      ***                               | MODELO DE ACESSO           ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-TP-CLIENTE-ADQ      | TIPO DE CLIENTE            ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-ID-EMPR-ADQ         | IDENTIFICACAO DA EMPRESA   ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-COD-GRU-PERFIL-ADQ  | CODIGO DO GRUPO PERFIL     ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-COD-PERFIL-ADQ      | CODIGO DE USUARIO          ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-COD-USUARIO-VCG     | CODIGO DE USUARIO          ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-BASE-CNPJ-IPJ       | BASE CNPJ                  ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-SHORT-NM-EMPR-IPJ   | SHORT-NAME DA EMPRESA      ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-ID-USUARIO-IPJ      | IDENTIFICACAO DO USUARIO   ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-COD-CLIENTE-DGB     | CODIGO CLIENTE             ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-AGENCIA-DGB         | AGENCIA                    ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-CONTA-DGB           | CONTA                      ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-BASE-CNPJ-OFW       | BASE CNPJ                  ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-SHORT-NM-EMPR-OFW   | SHORT-NAME DA EMPRESA      ***
      ***-------------------------------+----------------------------***
      *** RQ02-CRED-ID-USUARIO-OFW      | IDENTIFICACAO DO USUARIO   ***
      ***------------------------------------------------------------***
      ***                     DADOS DE CONVIVENCIA                   ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-AGENCIA-VCG         | AGENCIA                    ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-CONTA-VCG           | CONTA                      ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-IND-DEBUG-VCG       | INDICADOR DE DEBUG         ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-AGENCIA-IPJ         | AGENCIA                    ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-CONTA-IPJ           | CONTA                      ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-IND-CENTRAL-IPJ     | INDICADOR DE ACESSO CENTRAL***
      ***                               | DE ATENDIMENTO             ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-IND-UTLZ-TOKEN-IPJ  | INDICADOR DE UTILIZACAO DE ***
      ***                               | TOKEN                      ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-END-IP-IPJ          | ENDERECO DE IP             ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-ID-CONT-REQ-IPJ     | INDICADOR DE CONTINUIDADE  ***
      ***                               | DE REQUISICAO              ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-IND-CENTRAL-DGB     | INDICADOR DE ACESSO CENTRAL***
      ***                               | DE ATENDIMENTO             ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-MOEDA-VLR-TRANS-DGB | CODIGO DA MOEDA DO VL TRANS***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-VLR-TRANS-DGB       | VALOR DA TRANSACAO         ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-END-IP-DGB          | ENDERECO DE IP             ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-ID-BROWSER-DGB      | IDENTIFICACAO BROWSER      ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-AGENCIA-OFW         | AGENCIA                    ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-CONTA-OFW           | CONTA                      ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-IND-CENTRAL-OFW     | INDICADOR DE ACESSO CENTRAL***
      ***                               | DE ATENDIMENTO             ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-IND-UTLZ-TOKEN-OFW  | INDICADOR DE UTILIZACAO DE ***
      ***                               | TOKEN                      ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-END-IP-OFW          | ENDERECO DE IP             ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-IND-CORR-BANC-OFW   | INDICADOR DE ACESSO CORRES-***
      ***                               | PONDENTE BANCARIO          ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-VERSAO-OFFICE-OFW   | VERSAO OFFICE              ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-VERSAO-WINDOWS-OFW  | VERSAO WINDOWS             ***
      ***-------------------------------+----------------------------***
      *** RQ02-CONV-ID-CONT-REQ-OFW     | INDICADOR DE CONTINUIDADE  ***
      ***                               | DE REQUISICAO              ***
      ***------------------------------------------------------------***
      ***                        DADOS DE NEGOCIO                    ***
      ***-------------------------------+----------------------------***
      *** RQ02-NEG-DADOS                | DADOS                      ***
      ***-------------------------------+----------------------------***
      ***                     LOG DE MANUTENCAO                      ***
      ***------------------------------------------------------------***
      ***   DATA   | MARCA | RESP            | MOTIVO                ***
      ***----------+-------+-----------------+-----------------------***
      *** 02/01/17 | N/A   | ACCENTURE       | CRIACAO DO COPYBOOK   ***
      ***------------------------------------------------------------***
       01 SAFRA-DRIVER.
        03 DRVWRQ02.
           05        RQ02-AREA-REQU.
             10      RQ02-REQU-CD-TRANS      PIC  X(0004).
             10      RQ02-REQU-CD-APLI-CONS  PIC  X(0003).
             10      RQ02-REQU-CD-GRP-SVC    PIC  X(0003).
             10      RQ02-REQU-CD-SVC        PIC  X(0006).
             10      RQ02-REQU-MSG-ID        PIC  X(0032).
             10      FILLER                  PIC  X(0032).

           05        RQ02-AREA-CONT.
             10      RQ02-CONT-TAM-AREA-NEG  PIC  9(0006).
             10      FILLER                  PIC  X(0014).

           05        RQ02-AREA-CRED.
             10      RQ02-CRED-ID-TP-CRE     PIC  X(0003).
             10      RQ02-CRED-SESSAO        PIC  X(0035).
             10      RQ02-CRED-DADOS         PIC  X(0060).
             10      FILLER                  REDEFINES RQ02-CRED-DADOS.
               15    RQ02-CRED-COD-MOD-ACESSO-ADQ
                                             PIC  X(0011).
               15    RQ02-CRED-TP-CLIENTE-ADQ
                                             PIC  X(0001).
               15    RQ02-CRED-ID-EMPR-ADQ   PIC  X(0009).
               15    RQ02-CRED-COD-GRU-PERFIL-ADQ
                                             PIC  X(0002).
               15    RQ02-CRED-COD-PERFIL-ADQ
                                             PIC  9(0002).
               15    FILLER                  PIC  X(0035).
             10      FILLER                  REDEFINES RQ02-CRED-DADOS.
               15    RQ02-CRED-COD-USUARIO-VCG
                                             PIC  X(0008).
               15    FILLER                  PIC  X(0052).
             10      FILLER                  REDEFINES RQ02-CRED-DADOS.
               15    RQ02-CRED-BASE-CNPJ-IPJ PIC  9(0009).
               15    RQ02-CRED-SHORT-NM-EMPR-IPJ
                                             PIC  X(0015).
               15    RQ02-CRED-ID-USUARIO-IPJ
                                             PIC  X(0008).
               15    FILLER                  PIC  X(0028).
             10      FILLER                  REDEFINES RQ02-CRED-DADOS.
               15    RQ02-CRED-COD-CLIENTE-DGB
                                             PIC  9(0009).
               15    RQ02-CRED-AGENCIA-DGB   PIC  9(0005).
               15    RQ02-CRED-CONTA-DGB     PIC  9(0009).
               15    FILLER                  PIC  X(0037).
             10      FILLER                  REDEFINES RQ02-CRED-DADOS.
               15    RQ02-CRED-BASE-CNPJ-OFW PIC  9(0009).
               15    RQ02-CRED-SHORT-NM-EMPR-OFW
                                             PIC  X(0015).
               15    RQ02-CRED-ID-USUARIO-OFW
                                             PIC  X(0008).
               15    FILLER                  PIC  X(0028).
             10      FILLER                  PIC  X(0012).

           05        RQ02-AREA-CONV.
             10      RQ02-CONV-DADOS         PIC  X(0120).
             10      FILLER                  REDEFINES RQ02-CONV-DADOS.
               15    RQ02-CONV-AGENCIA-VCG   PIC  9(0005).
               15    RQ02-CONV-CONTA-VCG     PIC  9(0009).
               15    RQ02-CONV-IND-DEBUG-VCG PIC  X(0001).
               15    FILLER                  PIC  X(0105).
             10      FILLER                  REDEFINES RQ02-CONV-DADOS.
               15    RQ02-CONV-AGENCIA-IPJ   PIC  9(0005).
               15    RQ02-CONV-CONTA-IPJ     PIC  9(0009).
               15    RQ02-CONV-IND-CENTRAL-IPJ
                                             PIC  X(0001).
               15    RQ02-CONV-IND-UTLZ-TOKEN-IPJ
                                             PIC  X(0001).
               15    RQ02-CONV-END-IP-IPJ    PIC  X(0020).
               15    RQ02-CONV-ID-CONT-REQ-IPJ
                                             PIC  X(0002).
               15    FILLER                  PIC  X(0082).
             10      FILLER                  REDEFINES RQ02-CONV-DADOS.
               15    RQ02-CONV-IND-CENTRAL-DGB
                                             PIC  X(0001).
               15    RQ02-CONV-MOEDA-VLR-TRANS-DGB
                                             PIC  9(0005).
               15    RQ02-CONV-VLR-TRANS-DGB PIC S9(0015)V9(02).
               15    RQ02-CONV-END-IP-DGB    PIC  X(0020).
               15    RQ02-CONV-ID-BROWSER-DGB
                                             PIC  X(0060).
               15    FILLER                  PIC  X(0017).
             10      FILLER                  REDEFINES RQ02-CONV-DADOS.
               15    RQ02-CONV-AGENCIA-OFW   PIC  9(0005).
               15    RQ02-CONV-CONTA-OFW     PIC  9(0009).
               15    RQ02-CONV-IND-CENTRAL-OFW
                                             PIC  X(0001).
               15    RQ02-CONV-IND-UTLZ-TOKEN-OFW
                                             PIC  X(0001).
               15    RQ02-CONV-END-IP-OFW    PIC  X(0020).
               15    RQ02-CONV-IND-CORR-BANC-OFW
                                             PIC  X(0001).
               15    RQ02-CONV-VERSAO-OFFICE-OFW
                                             PIC  X(0010).
               15    RQ02-CONV-VERSAO-WINDOWS-OFW
                                             PIC  X(0010).
               15    RQ02-CONV-ID-CONT-REQ-OFW
                                             PIC  9(0002).
               15    FILLER                  PIC  X(0061).
             10      FILLER                  PIC  X(0020).

           05        FILLER                  PIC  X(0246).
           05        RQ02-AREA-NEG.
             10      RQ02-NEG-DADOS          PIC  X(3500).
           05        FILLER                  PIC  X(28337).
      ***----------+-------+-----------------+-----------------------***
      *** 14/06/20 | N/A   | PoC z/OS Connect                        ***
      ***------------------------------------------------------------***
       01 Book-API.
        03   header.
           05   requisicao.
             10   codigoTransacao                     PIC  X(0004).
             10   codigoAplicacaoConsumidora          PIC  X(0003).
             10   codigoGrupoServico                  PIC  X(0003).
             10   codigoServico                       PIC  X(0006).
             10   messageId                           PIC  X(0032).
             10   FILLER                              PIC  X(0032).
           05   controle.
             10   tamanhoAreaNegocio                  PIC  9(0006).
             10   FILLER                              PIC  X(0014).
           05   credenciais.
             10   identificacaoTipoCredencial         PIC  X(0003).
             10   sessao                              PIC  X(0035).
             10   areaCredenciais.
              13   adquirencia.
               15   codigoIdModeloAcesso                PIC  X(0011).
               15   codigoTipoCliente                   PIC  X(0001).
               15   codigoIdentificacaoEmpresa          PIC  X(0009).
               15   codigoGrupoPerfil                   PIC  X(0002).
               15   CODIGOPERFIL                        PIC  X(0002).
               15   FILLER                              PIC  X(0035).
              13   vcg.
               15   codigoUsuario-vcg                   PIC  X(0008).
               15   FILLER                              PIC  X(0052).
              13   ipj.
               15   baseCgcCnpj-ipj                     PIC  X(0009).
               15   shortnameEmpresa-ipj                PIC  X(0015).
               15   idUsuario-ipj                       PIC  X(0008).
               15   FILLER                              PIC  X(0028).
              13   dgb.
               15   codigoUsuario-dgb                   PIC  X(0009).
               15   agencia-dgb                         PIC  X(0005).
               15   conta-dgb                           PIC  X(0009).
               15   FILLER                              PIC  X(0037).
              13   ofw.
               15   baseCgcCnpj-ofw                     PIC  X(0009).
               15   shortnameEmpresa-ofw                PIC  X(0015).
               15   idUsuario-ofw                       PIC  X(0008).
               15   FILLER                              PIC  X(0028).
             10   FILLER                              PIC  X(0012).
           05   convivencia.
             10   convivenciaVcg.
               15   vcg-agencia                         PIC  X(0005).
               15   vcg-conta                           PIC  X(0009).
               15   vcg-indicadorDebug                  PIC  X(0001).
               15   FILLER                              PIC  X(0105).
             10   convivenciaIpj.
               15   ipj-agencia                         PIC  X(0005).
               15   ipj-conta                           PIC  X(0009).
               15   ipj-indicadorCentralAtendim         PIC  X(0001).
               15   ipj-indicadorUtilizaToken           PIC  X(0001).
               15   ipj-endereco                        PIC  X(0020).
               15   ipj-indicadorContinuacao            PIC  X(0002).
               15    FILLER                             PIC  X(0082).
             10   convivenciaDgb.
               15   dgb-indicadorCentralAtendim         PIC  X(0001).
               15   dgb-codigoMoeda                     PIC  X(0005).
               15   dgb-valorTransacao            PIC S9(0015)V9(02).
               15   dgb-enderecoIP                      PIC  X(0020).
               15   dgb-userAgent                       PIC  X(0060).
               15   FILLER                              PIC  X(0017).
             10   convivenciaOfw.
               15   ofw-agencia                         PIC  X(0005).
               15   ofw-conta                           PIC  X(0009).
               15   ofw-indicadorCentralAtendim         PIC  X(0001).
               15   ofw-indicadorUtilizaToken           PIC  X(0001).
               15   ofw-enderecoIP                      PIC  X(0020).
               15   ofw-indicadorCorrepondenteB         PIC  X(0001).
               15   ofw-versaoOffice                    PIC  X(0010).
               15   ofw-versaoWindows                   PIC  X(0010).
               15   ofw-indicadorContinuacao            PIC  X(0002).
               15   FILLER                              PIC  X(0061).
           05   dados                                   PIC  X(3500).
           05   filler                                  PIC  X(28337).
      ***------------------------------------------------------------***
      *** GENERIC WORK VARIABLES                                     ***
      ***------------------------------------------------------------***
       01 WK-EIB-RCODE                PIC S9(9) COMP-5 SYNC.
      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA             PIC X(32767) VALUE SPACES.

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       MAINLINE SECTION.

           MOVE DFHCOMMAREA TO header

           MOVE requisicao                  TO RQ02-AREA-REQU
           MOVE controle                    TO RQ02-AREA-CONT
           MOVE IdentificacaoTipoCredencial TO RQ02-CRED-ID-TP-CRE
           MOVE sessao                      TO RQ02-CRED-SESSAO

           EVALUATE IdentificacaoTipoCredencial
               WHEN 'ADQ'
                 MOVE adquirencia    to RQ02-CRED-DADOS
               WHEN 'VCG'
                 MOVE vcg            to RQ02-CRED-DADOS
                 MOVE convivenciaVcg to RQ02-CONV-DADOS
               WHEN 'IPJ'
                 MOVE ipj            to RQ02-CRED-DADOS
                 move convivenciaIpj to RQ02-CONV-DADOS
               WHEN 'DGB'
                 MOVE dgb            to RQ02-CRED-DADOS
                 move convivenciaDgb to RQ02-CONV-DADOS
               WHEN 'OFW'
                 MOVE ofw            to RQ02-CRED-DADOS
                 MOVE convivenciaOfw to RQ02-CONV-DADOS
           END-EVALUATE

           MOVE  dados         TO    RQ02-AREA-NEG

           EXEC CICS WRITEQ TS QUEUE('XPTO9998')
                     FROM(dados)
                     LENGTH(3500)
           END-EXEC

           EXEC CICS LINK PROGRAM('XPTO9999')
                          COMMAREA(DRVWRQ02)
                          LENGTH  (32767)
           END-EXEC

           EXEC CICS RETURN
           END-EXEC

           EXIT
           .