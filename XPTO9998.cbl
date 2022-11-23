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
       01 COB-REQUEST.
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
      ***----------+-------+-----------------+-----------------------***
      *** 14/06/20 | N/A   | PoC z/OS Connect                        ***
      ***------------------------------------------------------------***
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * This file contains the generated language structure(s) for
      *  request JSON schema 'drv.json'.
      * This structure was generated using 'DFHJS2LS' at mapping level
      *  '4.1'.
      *
      *
      *      06 header.
      *        09 requisicao.
      *
      * Comments for field 'codigoTransacao':
      * This field represents the value of JSON schema keyword
      *  'header->requisicao->codigoTransacao'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '4'.
      * This field contains a varying length array of characters or
      *  binary data.
      *          12 codigoTransacao-length        PIC S9999 COMP-5
      *  SYNC.
      *          12 codigoTransacao               PIC X(4).
      *
      * Comments for field 'codigoAplicacaoConsumidora':
      * This field represents the value of JSON schema keyword
      *  'header->requisicao->codigoAplicacaoConsumidora'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '3'.
      * This field contains a varying length array of characters or
      *  binary data.
      *          12 codigoAplicacaoConsu-length   PIC S9999 COMP-5
      *  SYNC.
      *          12 codigoAplicacaoConsumidora    PIC X(3).
      *
      * Comments for field 'codigoGrupoServico':
      * This field represents the value of JSON schema keyword
      *  'header->requisicao->codigoGrupoServico'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '3'.
      * This field contains a varying length array of characters or
      *  binary data.
      *          12 codigoGrupoServico-length     PIC S9999 COMP-5
      *  SYNC.
      *          12 codigoGrupoServico            PIC X(3).
      *
      * Comments for field 'codigoServico':
      * This field represents the value of JSON schema keyword
      *  'header->requisicao->codigoServico'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '6'.
      * This field contains a varying length array of characters or
      *  binary data.
      *          12 codigoServico-length          PIC S9999 COMP-5
      *  SYNC.
      *          12 codigoServico                 PIC X(6).
      *
      * Comments for field 'messageId':
      * This field represents the value of JSON schema keyword
      *  'header->requisicao->messageId'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '32'.
      * This field contains a varying length array of characters or
      *  binary data.
      *          12 messageId-length              PIC S9999 COMP-5
      *  SYNC.
      *          12 messageId                     PIC X(32).
      *        09 controle.
      *
      * Comments for field 'tamanhoAreaNegocio':
      * This field represents the value of JSON schema keyword
      *  'header->controle->tamanhoAreaNegocio'.
      * JSON schema type: 'int'.
      * JSON schema keyword 'maximum' value: '99999'.
      * JSON schema keyword 'minimum' value: '-2147483648'.
      *          12 tamanhoAreaNegocio            PIC S9(9) COMP-5
      *  SYNC.
      *        09 credenciais.
      *
      * Comments for field 'identificacaoTipoCredencial':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->identificacaoTipoCredencial'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '3'.
      * This field contains a varying length array of characters or
      *  binary data.
      *          12 identificacaoTipoCre-length   PIC S9999 COMP-5
      *  SYNC.
      *          12 identificacaoTipoCredencial   PIC X(3).
      *
      * Comments for field 'sessao':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->sessao'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '35'.
      * This field contains a varying length array of characters or
      *  binary data.
      *          12 sessao-length                 PIC S9999 COMP-5
      *  SYNC.
      *          12 sessao                        PIC X(35).
      *          12 areaCredenciais.
      *            15 adquirencia.
      *
      * Comments for field 'codigoIdModeloAcesso':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->adquirencia->codigoIdMo
      * deloAcesso'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '11'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 codigoIdModeloAcesso-length   PIC S9999 COMP-5
      *  SYNC.
      *              18 codigoIdModeloAcesso          PIC X(11).
      *
      * Comments for field 'codigoTipoCliente':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->adquirencia->codigoTipo
      * Cliente'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '1'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 codigoTipoCliente-length      PIC S9999 COMP-5
      *  SYNC.
      *              18 codigoTipoCliente             PIC X(1).
      *
      * Comments for field 'codigoIdentificacaoEmpresa':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->adquirencia->codigoIden
      * tificacaoEmpresa'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 codigoIdentificacaoE-length   PIC S9999 COMP-5
      *  SYNC.
      *              18 codigoIdentificacaoEmpresa    PIC X(9).
      *
      * Comments for field 'codigoGrupoPerfil':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->adquirencia->codigoGrup
      * oPerfil'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '2'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 codigoGrupoPerfil-length      PIC S9999 COMP-5
      *  SYNC.
      *              18 codigoGrupoPerfil             PIC X(2).
      *
      * Comments for field 'codigoPerfil':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->adquirencia->codigoPerf
      * il'.
      * JSON schema type: 'integer'.
      * JSON schema keyword 'maximum' value: '99'.
      * JSON schema keyword 'minimum' value: '-32768'.
      *              18 codigoPerfil                  PIC S9999 COMP-5
      *  SYNC.
      *            15 dgb.
      *
      * Comments for field 'codigoUsuario':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->dgb->codigoUsuario'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 codigoUsuario-length          PIC S9999 COMP-5
      *  SYNC.
      *              18 codigoUsuario                 PIC X(9).
      *
      * Comments for field 'agencia':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->dgb->agencia'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '5'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 agencia-length                PIC S9999 COMP-5
      *  SYNC.
      *              18 agencia                       PIC X(5).
      *
      * Comments for field 'conta':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->dgb->conta'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 conta-length                  PIC S9999 COMP-5
      *  SYNC.
      *              18 conta                         PIC X(9).
      *            15 ipj.
      *
      * Comments for field 'baseCgcCnpj':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->ipj->baseCgcCnpj'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 baseCgcCnpj-length            PIC S9999 COMP-5
      *  SYNC.
      *              18 baseCgcCnpj                   PIC X(9).
      *
      * Comments for field 'shortnameEmpresa':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->ipj->shortnameEmpresa'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '15'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 shortnameEmpresa-length       PIC S9999 COMP-5
      *  SYNC.
      *              18 shortnameEmpresa              PIC X(15).
      *
      * Comments for field 'idUsuario':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->ipj->idUsuario'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '8'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 idUsuario-length              PIC S9999 COMP-5
      *  SYNC.
      *              18 idUsuario                     PIC X(8).
      *            15 ofw.
      *
      * Comments for field 'baseCgcCnpj2':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->ofw->baseCgcCnpj'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 baseCgcCnpj2-length           PIC S9999 COMP-5
      *  SYNC.
      *              18 baseCgcCnpj2                  PIC X(9).
      *
      * Comments for field 'shortnameEmpresa2':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->ofw->shortnameEmpresa'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '15'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 shortnameEmpresa2-length      PIC S9999 COMP-5
      *  SYNC.
      *              18 shortnameEmpresa2             PIC X(15).
      *
      * Comments for field 'idUsuario2':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->ofw->idUsuario'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '8'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 idUsuario2-length             PIC S9999 COMP-5
      *  SYNC.
      *              18 idUsuario2                    PIC X(8).
      *            15 vcg.
      *
      * Comments for field 'codigoUsuario2':
      * This field represents the value of JSON schema keyword
      *  'header->credenciais->areaCredenciais->vcg->codigoUsuario'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '8'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 codigoUsuario2-length         PIC S9999 COMP-5
      *  SYNC.
      *              18 codigoUsuario2                PIC X(8).
      *        09 convivencia.
      *          12 convivenciaDgb.
      *
      * Comments for field 'indicadorCentralAtendimento':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaDgb->indicadorCentralAtendime
      * nto'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '1'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 indicadorCentralAten-length   PIC S9999 COMP-5
      *  SYNC.
      *            15 indicadorCentralAtendimento   PIC X(1).
      *
      * Comments for field 'codigoMoeda':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaDgb->codigoMoeda'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '5'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 codigoMoeda-length            PIC S9999 COMP-5
      *  SYNC.
      *            15 codigoMoeda                   PIC X(5).
      *
      * Comments for field 'valorTransacao':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaDgb->valorTransacao'.
      * JSON schema type: 'number'.
      * JSON schema keyword 'format' value: 'double'.
      * JSON schema keyword 'maximum' value: '1.0E17'.
      * JSON schema keyword 'minimum' value: '0.0'.
      * This field contains a "HEXADEC" type floating point number.
      *            15 valorTransacao                COMP-2 SYNC.
      *
      * Comments for field 'enderecoIP':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaDgb->enderecoIP'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '20'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 enderecoIP-length             PIC S9999 COMP-5
      *  SYNC.
      *            15 enderecoIP                    PIC X(20).
      *
      * Comments for field 'userAgent':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaDgb->userAgent'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '60'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 userAgent-length              PIC S9999 COMP-5
      *  SYNC.
      *            15 userAgent                     PIC X(60).
      *          12 convivenciaIpj.
      *
      * Comments for field 'agencia2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->agencia'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '5'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 agencia2-length               PIC S9999 COMP-5
      *  SYNC.
      *            15 agencia2                      PIC X(5).
      *
      * Comments for field 'conta2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->conta'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 conta2-length                 PIC S9999 COMP-5
      *  SYNC.
      *            15 conta2                        PIC X(9).
      *
      * Comments for field 'indicadorCentralAtendimento2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->indicadorCentralAtendime
      * nto'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '1'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 indicadorCentralAten-length2  PIC S9999 COMP-5
      *  SYNC.
      *            15 indicadorCentralAtendimento2  PIC X(1).
      *
      * Comments for field 'indicadorUtilizaToken':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->indicadorUtilizaToken'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '1'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 indicadorUtilizaToke-length   PIC S9999 COMP-5
      *  SYNC.
      *            15 indicadorUtilizaToken         PIC X(1).
      *
      * Comments for field 'enderecoIP2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->enderecoIP'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '20'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 enderecoIP2-length            PIC S9999 COMP-5
      *  SYNC.
      *            15 enderecoIP2                   PIC X(20).
      *
      * Comments for field 'indicadorContinuacao':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->indicadorContinuacao'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '2'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 indicadorContinuacao-length   PIC S9999 COMP-5
      *  SYNC.
      *            15 indicadorContinuacao          PIC X(2).
      *          12 convivenciaOfw.
      *
      * Comments for field 'agencia3':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->agencia'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '5'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 agencia3-length               PIC S9999 COMP-5
      *  SYNC.
      *            15 agencia3                      PIC X(5).
      *
      * Comments for field 'conta3':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->conta'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 conta3-length                 PIC S9999 COMP-5
      *  SYNC.
      *            15 conta3                        PIC X(9).
      *
      * Comments for field 'indicadorCentralAtendimento3':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->indicadorCentralAtendime
      * nto'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '1'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 indicadorCentralAten-length3  PIC S9999 COMP-5
      *  SYNC.
      *            15 indicadorCentralAtendimento3  PIC X(1).
      *
      * Comments for field 'indicadorUtilizaToken2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->indicadorUtilizaToken'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '1'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 indicadorUtilizaToke-length2  PIC S9999 COMP-5
      *  SYNC.
      *            15 indicadorUtilizaToken2        PIC X(1).
      *
      * Comments for field 'enderecoIP3':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->enderecoIP'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '20'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 enderecoIP3-length            PIC S9999 COMP-5
      *  SYNC.
      *            15 enderecoIP3                   PIC X(20).
      *
      * Comments for field 'indicadorCorrepondenteBancar':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->indicadorCorrepondenteBa
      * ncario'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '1'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 indicadorCorreponden-length   PIC S9999 COMP-5
      *  SYNC.
      *            15 indicadorCorrepondenteBancar  PIC X(1).
      *
      * Comments for field 'versaoOffice':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->versaoOffice'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '10'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 versaoOffice-length           PIC S9999 COMP-5
      *  SYNC.
      *            15 versaoOffice                  PIC X(10).
      *
      * Comments for field 'versaoWindows':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->versaoWindows'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '10'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 versaoWindows-length          PIC S9999 COMP-5
      *  SYNC.
      *            15 versaoWindows                 PIC X(10).
      *
      * Comments for field 'indicadorContinuacao2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->indicadorContinuacao'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '2'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 indicadorContinuacao-length2  PIC S9999 COMP-5
      *  SYNC.
      *            15 indicadorContinuacao2         PIC X(2).
      *          12 convivenciaVcg.
      *
      * Comments for field 'agencia4':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaVcg->agencia'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '5'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 agencia4-length               PIC S9999 COMP-5
      *  SYNC.
      *            15 agencia4                      PIC X(5).
      *
      * Comments for field 'conta4':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaVcg->conta'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 conta4-length                 PIC S9999 COMP-5
      *  SYNC.
      *            15 conta4                        PIC X(9).
      *
      * Comments for field 'indicadorDebug':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaVcg->indicadorDebug'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '1'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 indicadorDebug-length         PIC S9999 COMP-5
      *  SYNC.
      *            15 indicadorDebug                PIC X(1).
      *
      * Comments for field 'dados':
      * This field represents the value of JSON schema keyword 'dados'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '3500'.
      * This field contains a varying length array of characters or
      *  binary data.
      *      06 dados-length                  PIC S9999 COMP-5 SYNC.
      *      06 dados                         PIC X(3500).
      *
      *
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         01 Json-Request.
             06 header.
               09 requisicao.
                 12 codigoTransacao-length        PIC S9999 COMP-5
            SYNC.
                 12 codigoTransacao               PIC X(4).
                 12 codigoAplicacaoConsu-length   PIC S9999 COMP-5
            SYNC.
                 12 codigoAplicacaoConsumidora    PIC X(3).
                 12 codigoGrupoServico-length     PIC S9999 COMP-5
            SYNC.
                 12 codigoGrupoServico            PIC X(3).
                 12 codigoServico-length          PIC S9999 COMP-5
            SYNC.
                 12 codigoServico                 PIC X(6).
                 12 messageId-length              PIC S9999 COMP-5
            SYNC.
                 12 messageId                     PIC X(32).
               09 controle.
                 12 tamanhoAreaNegocio            PIC S9(9) COMP-5
            SYNC.
               09 credenciais.
                 12 identificacaoTipoCre-length   PIC S9999 COMP-5
            SYNC.
                 12 identificacaoTipoCredencial   PIC X(3).
                 12 sessao-length                 PIC S9999 COMP-5
            SYNC.
                 12 sessao                        PIC X(35).
                 12 areaCredenciais.
                   15 adquirencia.
                     18 codigoIdModeloAcesso-length   PIC S9999 COMP-5
            SYNC.
                     18 codigoIdModeloAcesso          PIC X(11).
                     18 codigoTipoCliente-length      PIC S9999 COMP-5
            SYNC.
                     18 codigoTipoCliente             PIC X(1).
                     18 codigoIdentificacaoE-length   PIC S9999 COMP-5
            SYNC.
                     18 codigoIdentificacaoEmpresa    PIC X(9).
                     18 codigoGrupoPerfil-length      PIC S9999 COMP-5
            SYNC.
                     18 codigoGrupoPerfil             PIC X(2).
                     18 codigoPerfil                  PIC S9999 COMP-5
            SYNC.
                   15 dgb.
                     18 codigoUsuario-length          PIC S9999 COMP-5
            SYNC.
                     18 codigoUsuario                 PIC X(9).
                     18 agencia-length                PIC S9999 COMP-5
            SYNC.
                     18 agencia                       PIC X(5).
                     18 conta-length                  PIC S9999 COMP-5
            SYNC.
                     18 conta                         PIC X(9).
                   15 ipj.
                     18 baseCgcCnpj-length            PIC S9999 COMP-5
            SYNC.
                     18 baseCgcCnpj                   PIC X(9).
                     18 shortnameEmpresa-length       PIC S9999 COMP-5
            SYNC.
                     18 shortnameEmpresa              PIC X(15).
                     18 idUsuario-length              PIC S9999 COMP-5
            SYNC.
                     18 idUsuario                     PIC X(8).
                   15 ofw.
                     18 baseCgcCnpj2-length           PIC S9999 COMP-5
            SYNC.
                     18 baseCgcCnpj2                  PIC X(9).
                     18 shortnameEmpresa2-length      PIC S9999 COMP-5
            SYNC.
                     18 shortnameEmpresa2             PIC X(15).
                     18 idUsuario2-length             PIC S9999 COMP-5
            SYNC.
                     18 idUsuario2                    PIC X(8).
                   15 vcg.
                     18 codigoUsuario2-length         PIC S9999 COMP-5
            SYNC.
                     18 codigoUsuario2                PIC X(8).
               09 convivencia.
                 12 convivenciaDgb.
                   15 indicadorCentralAten-length   PIC S9999 COMP-5
            SYNC.
                   15 indicadorCentralAtendimento   PIC X(1).
                   15 codigoMoeda-length            PIC S9999 COMP-5
            SYNC.
                   15 codigoMoeda                   PIC X(5).
                   15 valorTransacao                COMP-2 SYNC.
                   15 enderecoIP-length             PIC S9999 COMP-5
            SYNC.
                   15 enderecoIP                    PIC X(20).
                   15 userAgent-length              PIC S9999 COMP-5
            SYNC.
                   15 userAgent                     PIC X(60).
                 12 convivenciaIpj.
                   15 agencia2-length               PIC S9999 COMP-5
            SYNC.
                   15 agencia2                      PIC X(5).
                   15 conta2-length                 PIC S9999 COMP-5
            SYNC.
                   15 conta2                        PIC X(9).
                   15 indicadorCentralAten-length2  PIC S9999 COMP-5
            SYNC.
                   15 indicadorCentralAtendimento2  PIC X(1).
                   15 indicadorUtilizaToke-length   PIC S9999 COMP-5
            SYNC.
                   15 indicadorUtilizaToken         PIC X(1).
                   15 enderecoIP2-length            PIC S9999 COMP-5
            SYNC.
                   15 enderecoIP2                   PIC X(20).
                   15 indicadorContinuacao-length   PIC S9999 COMP-5
            SYNC.
                   15 indicadorContinuacao          PIC X(2).
                 12 convivenciaOfw.
                   15 agencia3-length               PIC S9999 COMP-5
            SYNC.
                   15 agencia3                      PIC X(5).
                   15 conta3-length                 PIC S9999 COMP-5
            SYNC.
                   15 conta3                        PIC X(9).
                   15 indicadorCentralAten-length3  PIC S9999 COMP-5
            SYNC.
                   15 indicadorCentralAtendimento3  PIC X(1).
                   15 indicadorUtilizaToke-length2  PIC S9999 COMP-5
            SYNC.
                   15 indicadorUtilizaToken2        PIC X(1).
                   15 enderecoIP3-length            PIC S9999 COMP-5
            SYNC.
                   15 enderecoIP3                   PIC X(20).
                   15 indicadorCorreponden-length   PIC S9999 COMP-5
            SYNC.
                   15 indicadorCorrepondenteBancar  PIC X(1).
                   15 versaoOffice-length           PIC S9999 COMP-5
            SYNC.
                   15 versaoOffice                  PIC X(10).
                   15 versaoWindows-length          PIC S9999 COMP-5
            SYNC.
                   15 versaoWindows                 PIC X(10).
                   15 indicadorContinuacao-length2  PIC S9999 COMP-5
            SYNC.
                   15 indicadorContinuacao2         PIC X(2).
                 12 convivenciaVcg.
                   15 agencia4-length               PIC S9999 COMP-5
            SYNC.
                   15 agencia4                      PIC X(5).
                   15 conta4-length                 PIC S9999 COMP-5
            SYNC.
                   15 conta4                        PIC X(9).
                   15 indicadorDebug-length         PIC S9999 COMP-5
            SYNC.
                   15 indicadorDebug                PIC X(1).
             06 dados-length                  PIC S9999 COMP-5 SYNC.
             06 dados                         PIC X(3500).

      ***------------------------------------------------------------***
      *** COPY BOOK RESPONSE                                         ***
      ***------------------------------------------------------------***
       01 COB-RESPONSE.
         03          DRVWEV02-BUF01.
           05        EV02-AREA-REQU.
             10      EV02-REQU-MSG-ID        PIC  X(0032).
             10      FILLER                  PIC  X(0018).
           05        EV02-AREA-CONT.
             10      EV02-CONT-ID-MAIS-DADOS PIC  X(0001).
             10      EV02-CONT-TAM-AREA-NEG  PIC  9(0006).
             10      FILLER                  PIC  X(0013).
           05        EV02-AREA-ERRO.
             10      EV02-ERRO-COD-RETORNO   PIC  9(0005).
             10      EV02-ERRO-DESC          PIC  X(0080).
             10      FILLER                  PIC  X(0015).
           05        EV02-AREA-CONV.
             10      EV02-CONV-DADOS         PIC  X(0100).
             10      FILLER                  REDEFINES EV02-CONV-DADOS.
               15    EV02-CONV-TRANS-IPJ     PIC  X(0004).
               15    EV02-CONV-CANAL-IPJ     PIC  X(0003).
               15    EV02-CONV-SERVICO-IPJ   PIC  X(0006).
               15    EV02-CONV-AGENCIA-IPJ   PIC  9(0005).
               15    EV02-CONV-CONTA-IPJ     PIC  9(0009).
               15    EV02-CONV-IND-CONT-IPJ  PIC  X(0002).
               15    EV02-CONV-SHORT-NM-EMPR-IPJ
                                             PIC  X(0015).
               15    EV02-CONV-USER-ID-IPJ   PIC  X(0008).
               15    EV02-CONV-BASE-CGC-IPJ  PIC  9(0009).
               15    EV02-CONV-QTD-ITENS-ENV-IPJ
                                             PIC  9(0004).
               15    EV02-CONV-END-IP-IPJ    PIC  X(0020).
               15    FILLER                  PIC  X(0015).
             10      FILLER                  REDEFINES EV02-CONV-DADOS.
               15    EV02-CONV-QTD-ITENS-ENV-DGB
                                             PIC  9(0004).
               15    FILLER                  PIC  X(0096).
             10      FILLER                  REDEFINES EV02-CONV-DADOS.
               15    EV02-CONV-TRANS-OFW     PIC  X(0004).
               15    EV02-CONV-CANAL-OFW     PIC  X(0003).
               15    EV02-CONV-SERVICO-OFW   PIC  X(0006).
               15    EV02-CONV-AGENCIA-OFW   PIC  9(0005).
               15    EV02-CONV-CONTA-OFW     PIC  9(0009).
               15    EV02-CONV-IND-CONT-OFW  PIC  X(0002).
               15    EV02-CONV-SHORT-NM-EMPR-OFW
                                             PIC  X(0015).
               15    EV02-CONV-USER-ID-OFW   PIC  X(0008).
               15    EV02-CONV-BASE-CGC-OFW  PIC  9(0009).
               15    EV02-CONV-QTD-ITENS-ENV-OFW
                                             PIC  9(0004).
               15    EV02-CONV-END-IP-OFW    PIC  X(0020).
               15    FILLER                  PIC  X(0015).
             10      FILLER                  REDEFINES EV02-CONV-DADOS.
               15    EV02-CONV-QTD-ITENS-ENV-ADQ
                                             PIC  9(0004).
               15    FILLER                  PIC  X(0096).
             10      FILLER                  PIC  X(0010).
           05        FILLER                  PIC  X(0316).
           05        EV02-AREA-NEG.
             10      DRVEV02-NEG-DADOS       PIC  X(3500).
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * This file contains the generated language structure(s) for
      *  response JSON schema 'resp.json'.
      * This structure was generated using 'DFHJS2LS' at mapping level
      *  '4.1'.
      *
      *
      *      06 xheader.
      *        09 xrequisicao.
      *
      * Comments for field 'messageId':
      * This field represents the value of JSON schema keyword
      *  'header->requisicao->messageId'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '32'.
      * This field contains a varying length array of characters or
      *  binary data.
      *          12 xmessageId-length             PIC S9999 COMP-5
      *  SYNC.
      *          12 xmessageId                    PIC X(32).
      *        09 xcontrole.
      *
      * Comments for field 'indicadorMaisDados':
      * This field represents the value of JSON schema keyword
      *  'header->controle->indicadorMaisDados'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '1'.
      * This field contains a varying length array of characters or
      *  binary data.
      *          12 xindicadorMaisDados-length    PIC S9999 COMP-5
      *  SYNC.
      *          12 xindicadorMaisDados           PIC X(1).
      *
      * Comments for field 'tamanhoAreaNegocio':
      * This field represents the value of JSON schema keyword
      *  'header->controle->tamanhoAreaNegocio'.
      * JSON schema type: 'int'.
      * JSON schema keyword 'maximum' value: '999999'.
      * JSON schema keyword 'minimum' value: '-2147483648'.
      *          12 xtamanhoAreaNegocio           PIC S9(9) COMP-5
      *  SYNC.
      *        09 xerro.
      *
      * Comments for field 'codigoRetorno':
      * This field represents the value of JSON schema keyword
      *  'header->erro->codigoRetorno'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '5'.
      * This field contains a varying length array of characters or
      *  binary data.
      *          12 xcodigoRetorno-length         PIC S9999 COMP-5
      *  SYNC.
      *          12 xcodigoRetorno                PIC X(5).
      *
      * Comments for field 'mensagemRetorno':
      * This field represents the value of JSON schema keyword
      *  'header->erro->mensagemRetorno'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '80'.
      * This field contains a varying length array of characters or
      *  binary data.
      *          12 xmensagemRetorno-length       PIC S9999 COMP-5
      *  SYNC.
      *          12 xmensagemRetorno              PIC X(80).
      *        09 xconvivencia.
      *          12 xconvivenciaAdquirencia.
      *
      * Comments for field 'quantidadeOcorrencias':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaAdquirencia->quantidadeOcorre
      * ncias'.
      * JSON schema type: 'integer'.
      * JSON schema keyword 'maximum' value: '9999'.
      * JSON schema keyword 'minimum' value: '-32768'.
      *            15 xquantidadeOcorrencias        PIC S9999 COMP-5
      *  SYNC.
      *          12 xconvivenciaDgb.
      *
      * Comments for field 'quantidadeOcorrencias2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaDgb->quantidadeOcorrencias'.
      * JSON schema type: 'integer'.
      * JSON schema keyword 'maximum' value: '9223372036854775807'.
      * JSON schema keyword 'minimum' value: '-9223372036854775808'.
      *            15 xquantidadeOcorrencias2       PIC S9(18) COMP-5
      *  SYNC.
      *          12 xconvivenciaIpj.
      *
      * Comments for field 'transacao':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->transacao'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '4'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xtransacao-length             PIC S9999 COMP-5
      *  SYNC.
      *            15 xtransacao                    PIC X(4).
      *
      * Comments for field 'canal':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->canal'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '3'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xcanal-length                 PIC S9999 COMP-5
      *  SYNC.
      *            15 xcanal                        PIC X(3).
      *
      * Comments for field 'servico':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->servico'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '6'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xservico-length               PIC S9999 COMP-5
      *  SYNC.
      *            15 xservico                      PIC X(6).
      *
      * Comments for field 'agencia':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->agencia'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '5'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xagencia-length               PIC S9999 COMP-5
      *  SYNC.
      *            15 xagencia                      PIC X(5).
      *
      * Comments for field 'conta':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->conta'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xconta-length                 PIC S9999 COMP-5
      *  SYNC.
      *            15 xconta                        PIC X(9).
      *
      * Comments for field 'indicadorContinuacao':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->indicadorContinuacao'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '2'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xindicadorContinuacao-length  PIC S9999 COMP-5
      *  SYNC.
      *            15 xindicadorContinuacao         PIC X(2).
      *
      * Comments for field 'shortname':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->shortname'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '15'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xshortname-length             PIC S9999 COMP-5
      *  SYNC.
      *            15 xshortname                    PIC X(15).
      *
      * Comments for field 'idUsuario':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->idUsuario'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '8'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xidUsuario-length             PIC S9999 COMP-5
      *  SYNC.
      *            15 xidUsuario                    PIC X(8).
      *
      * Comments for field 'baseCgcCnpj':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->baseCgcCnpj'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xbaseCgcCnpj-length           PIC S9999 COMP-5
      *  SYNC.
      *            15 xbaseCgcCnpj                  PIC X(9).
      *
      * Comments for field 'quantidadeOcorrencias3':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->quantidadeOcorrencias'.
      * JSON schema type: 'integer'.
      * JSON schema keyword 'maximum' value: '9999'.
      * JSON schema keyword 'minimum' value: '-32768'.
      *            15 xquantidadeOcorrencias3       PIC S9999 COMP-5
      *  SYNC.
      *
      * Comments for field 'enderecoIP':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaIpj->enderecoIP'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '20'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xenderecoIP-length            PIC S9999 COMP-5
      *  SYNC.
      *            15 xenderecoIP                   PIC X(20).
      *          12 xconvivenciaOfw.
      *
      * Comments for field 'transacao2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->transacao'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '4'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xtransacao2-length            PIC S9999 COMP-5
      *  SYNC.
      *            15 xtransacao2                   PIC X(4).
      *
      * Comments for field 'canal2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->canal'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '3'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xcanal2-length                PIC S9999 COMP-5
      *  SYNC.
      *            15 xcanal2                       PIC X(3).
      *
      * Comments for field 'servico2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->servico'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '6'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xservico2-length              PIC S9999 COMP-5
      *  SYNC.
      *            15 xservico2                     PIC X(6).
      *
      * Comments for field 'agencia2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->agencia'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '5'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xagencia2-length              PIC S9999 COMP-5
      *  SYNC.
      *            15 xagencia2                     PIC X(5).
      *
      * Comments for field 'conta2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->conta'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xconta2-length                PIC S9999 COMP-5
      *  SYNC.
      *            15 xconta2                       PIC X(9).
      *
      * Comments for field 'indicadorContinuacao2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->indicadorContinuacao'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '2'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xindicadorContinuacao-length2 PIC S9999 COMP-5
      *  SYNC.
      *            15 xindicadorContinuacao2        PIC X(2).
      *
      * Comments for field 'shortname2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->shortname'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '15'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xshortname2-length            PIC S9999 COMP-5
      *  SYNC.
      *            15 xshortname2                   PIC X(15).
      *
      * Comments for field 'idUsuario2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->idUsuario'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '8'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xidUsuario2-length            PIC S9999 COMP-5
      *  SYNC.
      *            15 xidUsuario2                   PIC X(8).
      *
      * Comments for field 'baseCgcCnpj2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->baseCgcCnpj'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '9'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xbaseCgcCnpj2-length          PIC S9999 COMP-5
      *  SYNC.
      *            15 xbaseCgcCnpj2                 PIC X(9).
      *
      * Comments for field 'quantidadeOcorrencias4':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->quantidadeOcorrencias'.
      * JSON schema type: 'integer'.
      * JSON schema keyword 'maximum' value: '4'.
      * JSON schema keyword 'minimum' value: '-32768'.
      *            15 xquantidadeOcorrencias4       PIC S9999 COMP-5
      *  SYNC.
      *
      * Comments for field 'enderecoIP2':
      * This field represents the value of JSON schema keyword
      *  'header->convivencia->convivenciaOfw->enderecoIP'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '20'.
      * This field contains a varying length array of characters or
      *  binary data.
      *            15 xenderecoIP2-length           PIC S9999 COMP-5
      *  SYNC.
      *            15 xenderecoIP2                  PIC X(20).
      *
      * Comments for field 'dados':
      * This field represents the value of JSON schema keyword 'dados'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '3500'.
      * This field contains a varying length array of characters or
      *  binary data.
      *      06 xdados-length                 PIC S9999 COMP-5 SYNC.
      *      06 xdados                        PIC X(3500).
      *
      *
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

       01 Json-Response.
             06 xheader.
               09 xrequisicao.
                 12 xmessageId-length             PIC S9999 COMP-5
            SYNC.
                 12 xmessageId                    PIC X(32).
               09 xcontrole.
                 12 xindicadorMaisDados-length    PIC S9999 COMP-5
            SYNC.
                 12 xindicadorMaisDados           PIC X(1).
                 12 xtamanhoAreaNegocio           PIC S9(9) COMP-5
            SYNC.
               09 xerro.
                 12 xcodigoRetorno-length         PIC S9999 COMP-5
            SYNC.
                 12 xcodigoRetorno                PIC X(5).
                 12 xmensagemRetorno-length       PIC S9999 COMP-5
            SYNC.
                 12 xmensagemRetorno              PIC X(80).
               09 xconvivencia.
                 12 xconvivenciaAdquirencia.
                   15 xquantidadeOcorrencias        PIC S9999 COMP-5
            SYNC.
                 12 xconvivenciaDgb.
                   15 xquantidadeOcorrencias2       PIC S9(18) COMP-5
            SYNC.
                 12 xconvivenciaIpj.
                   15 xtransacao-length             PIC S9999 COMP-5
            SYNC.
                   15 xtransacao                    PIC X(4).
                   15 xcanal-length                 PIC S9999 COMP-5
            SYNC.
                   15 xcanal                        PIC X(3).
                   15 xservico-length               PIC S9999 COMP-5
            SYNC.
                   15 xservico                      PIC X(6).
                   15 xagencia-length               PIC S9999 COMP-5
            SYNC.
                   15 xagencia                      PIC X(5).
                   15 xconta-length                 PIC S9999 COMP-5
            SYNC.
                   15 xconta                        PIC X(9).
                   15 xindicadorContinuacao-length  PIC S9999 COMP-5
            SYNC.
                   15 xindicadorContinuacao         PIC X(2).
                   15 xshortname-length             PIC S9999 COMP-5
            SYNC.
                   15 xshortname                    PIC X(15).
                   15 xidUsuario-length             PIC S9999 COMP-5
            SYNC.
                   15 xidUsuario                    PIC X(8).
                   15 xbaseCgcCnpj-length           PIC S9999 COMP-5
            SYNC.
                   15 xbaseCgcCnpj                  PIC X(9).
                   15 xquantidadeOcorrencias3       PIC S9999 COMP-5
            SYNC.
                   15 xenderecoIP-length            PIC S9999 COMP-5
            SYNC.
                   15 xenderecoIP                   PIC X(20).
                 12 xconvivenciaOfw.
                   15 xtransacao2-length            PIC S9999 COMP-5
            SYNC.
                   15 xtransacao2                   PIC X(4).
                   15 xcanal2-length                PIC S9999 COMP-5
            SYNC.
                   15 xcanal2                       PIC X(3).
                   15 xservico2-length              PIC S9999 COMP-5
            SYNC.
                   15 xservico2                     PIC X(6).
                   15 xagencia2-length              PIC S9999 COMP-5
            SYNC.
                   15 xagencia2                     PIC X(5).
                   15 xconta2-length                PIC S9999 COMP-5
            SYNC.
                   15 xconta2                       PIC X(9).
                   15 xindicadorContinuacao-length2 PIC S9999 COMP-5
            SYNC.
                   15 xindicadorContinuacao2        PIC X(2).
                   15 xshortname2-length            PIC S9999 COMP-5
            SYNC.
                   15 xshortname2                   PIC X(15).
                   15 xidUsuario2-length            PIC S9999 COMP-5
            SYNC.
                   15 xidUsuario2                   PIC X(8).
                   15 xbaseCgcCnpj2-length          PIC S9999 COMP-5
            SYNC.
                   15 xbaseCgcCnpj2                 PIC X(9).
                   15 xquantidadeOcorrencias4       PIC S9999 COMP-5
            SYNC.
                   15 xenderecoIP2-length           PIC S9999 COMP-5
            SYNC.
                   15 xenderecoIP2                  PIC X(20).
             06 xdados-length                 PIC S9999 COMP-5 SYNC.
             06 xdados                        PIC X(3500).

      ***------------------------------------------------------------***
      *** GENERIC WORK VARIABLES                                     ***
      ***------------------------------------------------------------***
       01 WK-EIB-RCODE                PIC S9(9) COMP-5 SYNC.
       01 WK-MOVE-AREA                PIC  S(32767).
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

           MOVE DFHCOMMAREA TO Json-Request
      * --------------------------------------------------------------
           move codigoTransacao             to RQ02-REQU-CD-TRANS
           move codigoAplicacaoConsumidora  to RQ02-REQU-CD-APLI-CONS
           move codigoGrupoServico          to RQ02-REQU-CD-GRP-SVC
           move codigoServico               to RQ02-REQU-CD-SVC
           move messageId                   to RQ02-REQU-MSG-ID
           move tamanhoAreaNegocio          to RQ02-CONT-TAM-AREA-NEG
           move identificacaoTipoCredencial to RQ02-CRED-ID-TP-CRE
           move sessao                      to RQ02-CRED-SESSAO
      * --------------------------------------------------------------
      * Nao ha campo equivalente do JSON ao CRED-DATA, devido ser
      * redefinido, JSON nao possui equivalente, sempre usa string
      * --------------------------------------------------------------
           evaluate identificacaoTipoCredencial
           when 'ADQ'
            move codigoIdModeloAcesso  to RQ02-CRED-COD-MOD-ACESSO-ADQ
            move codigoTipoCliente     to RQ02-CRED-TP-CLIENTE-ADQ
            move codigoIdentificacaoEmpresa
                                       to RQ02-CRED-ID-EMPR-ADQ
            move codigoGrupoPerfil     to RQ02-CRED-COD-GRU-PERFIL-ADQ
            move codigoPerfil          to RQ02-CRED-COD-PERFIL-ADQ
           when 'DGB'
            move codigoUsuario         to RQ02-CRED-COD-CLIENTE-DGB
            move agencia               to RQ02-CRED-AGENCIA-DGB
            move conta                 to RQ02-CRED-CONTA-DGB
      *---------------------------------------------------------------
      *      MOVE CONVIVENCIA
      *---------------------------------------------------------------
            move indicadorCentralAtendimento
                                       to RQ02-CONV-IND-CENTRAL-DGB
            move codigoMoeda           to RQ02-CONV-MOEDA-VLR-TRANS-DGB
            move valorTransacao        to RQ02-CONV-VLR-TRANS-DGB
            move enderecoIP            to RQ02-CONV-END-IP-DGB
            move userAgent             to RQ02-CONV-ID-BROWSER-DGB
           when 'IPJ'
            move baseCgcCnpj          to RQ02-CRED-BASE-CNPJ-IPJ
            move shortnameEmpresa     to RQ02-CRED-SHORT-NM-EMPR-IPJ
            move idUsuario            to RQ02-CRED-ID-USUARIO-IPJ
      *---------------------------------------------------------------
      *      MOVE CONVIVENCIA
      *---------------------------------------------------------------
            move agencia2             to RQ02-CONV-AGENCIA-IPJ
            move conta2               to RQ02-CONV-CONTA-IPJ
            move indicadorCentralAtendimento2
                                      to RQ02-CONV-IND-CENTRAL-IPJ
            move indicadorUtilizaToken
                                      to RQ02-CONV-IND-UTLZ-TOKEN-IPJ
            move enderecoIP2          to RQ02-CONV-END-IP-IPJ
            move indicadorContinuacao to RQ02-CONV-ID-CONT-REQ-IPJ
           when 'OFW'
            move baseCgcCnpj2         to RQ02-CRED-BASE-CNPJ-OFW
            move shortnameEmpresa2    to RQ02-CRED-SHORT-NM-EMPR-OFW
            move idUsuario2           to RQ02-CRED-ID-USUARIO-OFW
      *---------------------------------------------------------------
      *      MOVE CONVIVENCIA
      *---------------------------------------------------------------
            move agencia3             to RQ02-CONV-AGENCIA-OFW
            move conta3               to RQ02-CONV-CONTA-OFW
            move indicadorCentralAtendimento3
                                      to RQ02-CONV-IND-CENTRAL-OFW
            move indicadorUtilizaToken2
                                      to RQ02-CONV-IND-UTLZ-TOKEN-OFW
            move enderecoIP3          to RQ02-CONV-END-IP-OFW
            move indicadorCorrepondenteBancar
                                      to RQ02-CONV-IND-CORR-BANC-OFW
            move versaoOffice         to RQ02-CONV-VERSAO-OFFICE-OFW
            move versaoWindows        to RQ02-CONV-VERSAO-WINDOWS-OFW
            move indicadorContinuacao2
                                      to RQ02-CONV-ID-CONT-REQ-OFW
           when 'VCG'
            move codigoUsuario2       to RQ02-CRED-COD-USUARIO-VCG
      *---------------------------------------------------------------
      *     MOVE CONVIVENCIA
      *---------------------------------------------------------------
            move agencia4             to RQ02-CONV-AGENCIA-VCG
            move conta4               to RQ02-CONV-CONTA-VCG
            move indicadorDebug       to RQ02-CONV-IND-DEBUG-VCG
           when other
            MOVE '???????????'        to RQ02-NEG-DADOS
           end-evaluate
           move dados                 to RQ02-NEG-DADOS

           EXEC CICS WRITEQ TS QUEUE('XPTO9998')
                     FROM(dados)
                     LENGTH(3500)
           END-EXEC

           MOVE DRVWRQ02  TO       WK-MOVE-AREA.
           EXEC CICS LINK PROGRAM  ('XPTO9999')
                          COMMAREA (WK-MOVE-AREA)
                          LENGTH   (32767)
           END-EXEC

           EXEC CICS RETURN
           END-EXEC

           EXIT
           .