       CBL CICS('COBOL3') APOST
      *-------------------------------------------------------------  *
      *
      *  MODULE NAME = CTLGAPIX
      *
      *  DESCRIPTIVE NAME = CICS TS  (Samples) Example Application -
      *                     Catalog Manager Program
      *
      *  TRANSACTION NAME = ktlx
      *
      *  FUNCTION =
      *  execute an API request from catalog manager sample system
      *  passing the ITEM number and receiving details from product
      *
      *  Built by Valter Siqueira, IBM Brazil.
      *           valters@br.ibm.com
      *-------------------------------------------------------------  *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTLGAPIX.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Work area                                                     *
      *----------------------------------------------------------------*
        01 wk-work.
           03 wk-receive-length        PIC S9(4) COMP value 255.
           03 wk-receive.
              05 filler                pic x(004).
              05 wk-screen-str         pic x(003).
              05 filler                pic x(247).

      *---------------------------------------------------------------
      * Inserir o Book de informacoes da API gerado pelo utilitario
      * 'zconbt' abaixo do nivel "01 API-INFO"
      * Nome padrao gerado "APIxxIxx"
      *---------------------------------------------------------------

       01 API-INFO.
           03 BAQ-APINAME                PIC X(255)
              VALUE 'catalog_1.0.0'.
           03 BAQ-APINAME-LEN            PIC S9(9) COMP-5 SYNC
              VALUE 13.
           03 BAQ-APIPATH                PIC X(255)
              VALUE '%2FcatalogManager%2Fitems%2F%7BitemID%7D'.
           03 BAQ-APIPATH-LEN            PIC S9(9) COMP-5 SYNC
              VALUE 40.
           03 BAQ-APIMETHOD              PIC X(255)
              VALUE 'GET'.
           03 BAQ-APIMETHOD-LEN          PIC S9(9) COMP-5 SYNC
              VALUE 3.

      *---------------------------------------------------------------
      * Inserir o Book de "request" da API gerado pelo utilitario
      * 'zconbt' abaixo do nivel "01 REQUEST"
      * Nome padrao gerado "APIxxQxx"
      *---------------------------------------------------------------

       01 REQUEST.
             06 ReqPathParameters.
               09 itemID                        PIC X(256).

      *---------------------------------------------------------------
      * Inserir o Book de "response" da API gerado pelo utilitario
      * 'zconbt' abaixo do nivel "01 RESPONSE"
      * Nome padrao gerado "APIxxPxx"
      *---------------------------------------------------------------

       01 RESPONSE.
             06 RespBody.
               09 DFH0XCMNOperationResponse.
                 12 ca-return-code                PIC 9(2) DISPLAY.
                 12 ca-response-message           PIC X(80).
                 12 ca-inquire-single.
                   15 ca-single-item.
                     18 in-sngl-stock                 PIC 9(4) DISPLAY.
                     18 ca-sngl-description           PIC X(41).
                     18 ca-sngl-item-ref              PIC 9(4) DISPLAY.
                     18 on-sngl-order                 PIC 9(3) DISPLAY.
                     18 ca-sngl-cost                  PIC X(7).
                     18 ca-sngl-department            PIC 9(3) DISPLAY.

      *--------------------------------------------------------------
      * Estrutura de linguagem com informaoes para executar os
      * procedimentos de "request" and " response"
      * IMPORTANTE - valores e tamanhos padrao do STUB, nao devem
      *              ser alterados
      *--------------------------------------------------------------
       01  BAQ-REQUEST-INFO.
         03 BAQ-REQUEST-INFO-COMP-LEVEL  PIC S9(9) COMP-5 SYNC VALUE 2.
         03 BAQ-REQUEST-INFO-USER.
            05 BAQ-OAUTH.
               07 BAQ-OAUTH-USERNAME           PIC X(256).
               07 BAQ-OAUTH-USERNAME-LEN       PIC S9(9) COMP-5 SYNC
                                                 VALUE 0.
               07 BAQ-OAUTH-PASSWORD           PIC X(256).
               07 BAQ-OAUTH-PASSWORD-LEN       PIC S9(9) COMP-5 SYNC
                                                 VALUE 0.
               07 BAQ-OAUTH-CLIENTID           PIC X(256).
               07 BAQ-OAUTH-CLIENTID-LEN       PIC S9(9) COMP-5 SYNC
                                                 VALUE 0.
               07 BAQ-OAUTH-CLIENT-SECRET      PIC X(256).
               07 BAQ-OAUTH-CLIENT-SECRET-LEN  PIC S9(9) COMP-5 SYNC
                                                 VALUE 0.
               07 BAQ-OAUTH-SCOPE-PTR          USAGE POINTER.
               07 BAQ-OAUTH-SCOPE-LEN          PIC S9(9) COMP-5 SYNC
                                                 VALUE 0.
            05 BAQ-AUTHTOKEN.
               07 BAQ-TOKEN-USERNAME           PIC X(256).
               07 BAQ-TOKEN-USERNAME-LEN       PIC S9(9) COMP-5 SYNC
                                                 VALUE 0.
               07 BAQ-TOKEN-PASSWORD           PIC X(256).
               07 BAQ-TOKEN-PASSWORD-LEN       PIC S9(9) COMP-5 SYNC
                                                 VALUE 0.
       01  BAQ-RESPONSE-INFO.
         03 BAQ-RESPONSE-INFO-COMP-LEVEL PIC S9(9) COMP-5 SYNC VALUE 0.
         03 BAQ-STUB-NAME                PIC X(8).
         03 BAQ-RETURN-CODE              PIC S9(9) COMP-5 SYNC.
            88 BAQ-SUCCESS                 VALUE 0.
            88 BAQ-ERROR-IN-API            VALUE 1.
            88 BAQ-ERROR-IN-ZCEE           VALUE 2.
            88 BAQ-ERROR-IN-STUB           VALUE 3.
         03 BAQ-STATUS-CODE              PIC S9(9) COMP-5 SYNC.
         03 BAQ-STATUS-MESSAGE           PIC X(1024).
         03 BAQ-STATUS-MESSAGE-LEN       PIC S9(9) COMP-5 SYNC.

      *----------------------------------------------------------------*
      *   Areas utilizadas pelos ponteiros para a chamada do programa
      *   STUB padrao do zCEE
      *----------------------------------------------------------------*

       01 BAQ-REQUEST-PTR USAGE POINTER.
       01 BAQ-REQUEST-LEN PIC S9(9) COMP-5 SYNC.
       01 BAQ-RESPONSE-PTR USAGE POINTER.
       01 BAQ-RESPONSE-LEN PIC S9(9) COMP-5 SYNC.
       77 COMM-STUB-PGM-NAME PIC X(8) VALUE 'BAQCSTUB'.

      *----------------------------------------------------------------*
      *    L I N K A G E   S E C T I O N
      *----------------------------------------------------------------*

       LINKAGE SECTION.
       01 DFHCOMMAREA             PIC X(256) VALUE SPACES.

      *----------------------------------------------------------------*
      *P R O C E D U R E S
      *----------------------------------------------------------------*

       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.
      *----------------------------------------------------------------*

           initialize response
           initialize request

           exec cics receive into(wk-receive)
                             length(wk-receive-length)
           end-exec

           MOVE wk-screen-str to itemID IN REQUEST.

           SET BAQ-REQUEST-PTR TO ADDRESS OF REQUEST.
           MOVE LENGTH OF REQUEST TO BAQ-REQUEST-LEN.
           SET BAQ-RESPONSE-PTR TO ADDRESS OF RESPONSE.
           MOVE LENGTH OF RESPONSE TO BAQ-RESPONSE-LEN.

           CALL COMM-STUB-PGM-NAME USING
           BY REFERENCE API-INFO
           BY REFERENCE BAQ-REQUEST-INFO
           BY REFERENCE BAQ-REQUEST-PTR
           BY REFERENCE BAQ-REQUEST-LEN
           BY REFERENCE BAQ-RESPONSE-INFO
           BY REFERENCE BAQ-RESPONSE-PTR
           BY REFERENCE BAQ-RESPONSE-LEN.

           EXEC CICS WRITEQ TS QUEUE('CTLGAPIR')
                     FROM(RESPONSE)
                     LENGTH(144)
           END-EXEC
           EXEC CICS SEND
                     FROM(RESPONSE)
                     erase
                     LENGTH(length of response)
           END-EXEC

           exec cics return
           end-exec

           EXIT
           .