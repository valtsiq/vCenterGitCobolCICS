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