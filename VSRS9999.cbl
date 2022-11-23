       CBL CICS('COBOL3') APOST
      *****************************************************************
      *                                                               *
      *  MODULE NAME = DFH0XCMN                                       *
      *                                                               *
      *  DESCRIPTIVE NAME = CICS TS  (Samples) Example Application -  *
      *                     Catalog Manager Program                   *
      *                                                               *
      *                                                               *
      *                                                               *
      *      Licensed Materials - Property of IBM                     *
      *                                                               *
      *      "Restricted Materials of IBM"                            *
      *                                                               *
      *      5655-Y04                                                 *
      *                                                               *
      *      (C) Copyright IBM Corp. 2004, 2005"                      *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *  STATUS = 7.2.0                                               *
      *                                                               *
      *  TRANSACTION NAME = n/a                                       *
      *                                                               *
      *  FUNCTION =                                                   *
      *  This module is the controller for the Catalog application,   *
      *  all requests pass through this module                        *
      *                                                               *
      *-------------------------------------------------------------  *
      *                                                               *
      *  ENTRY POINT = DFH0XCMN                                       *
      *                                                               *
      *-------------------------------------------------------------  *
      *                                                               *
      *  CHANGE ACTIVITY :                                            *
      *                                                               *
      *  $MOD(DFH0XCMN),COMP(PIPELINE),PROD(CICS TS ):                *
      *                                                               *
      *  PN= REASON REL YYMMDD HDXXIII : REMARKS                      *
      * $D0= I07544 640 041126 HDIPCB  : ExampleApp: Outbound support *
      * $P1= D13727 640 050217 HDIPCB  : Minor fixes to the web servic*
      *  $D0= I07544 640 040910 HDIPCB  : EXAMPLE - BASE APPLICATION  *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSRSMAIN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'VSRSMAIN------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC S9(7) COMP-3.
           03 WS-CALEN                 PIC S9(4) COMP.
           03 wk-receive-length        PIC S9(4) COMP.
           03 WK-LEN                   PIC S9(4) COMP VALUE 200.
           03 wk-rec-red               pic x(200).
           03 wk-receive   redefines   wk-rec-red.
              05 filler                pic x(004).
              05 wk-strname            pic x(008).
              05 filler                pic x(188).
      * Variables for time/date processing
       01  ABS-TIME                    PIC S9(8) COMP VALUE +0.
       01  TIME1                       PIC X(8)  VALUE SPACES.
       01  DATE1                       PIC X(10) VALUE SPACES.
      *===============================================================
      *****************************************************************
      *
      *
      * Licensed Materials - Property of IBM
      *
      * Restricted Materials of IBM
      *
      * 5655-CE3
      *
      * (C) Copyright IBM Corp. 2017
      *
      * US Government Users Restricted Rights - Use, duplication or
      * disclosure restricted by GSA ADP Schedule Contract with
      * IBM Corp.
      *****************************************************************
      * This file contains the generated language structure(s) for    *
      * Request and Response Info                                     *
      *****************************************************************
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
      *------------------------------------------------------
       01 API-INFO.
           03 BAQ-APINAME                PIC X(255)
              VALUE 'cicsrs_1.0.0'.
           03 BAQ-APINAME-LEN            PIC S9(9) COMP-5 SYNC
              VALUE 12.
           03 BAQ-APIPATH                PIC X(255)
              VALUE '%2Fcicsrs%2FCicsParmTran200%2F%7BStrName%7D'.
           03 BAQ-APIPATH-LEN            PIC S9(9) COMP-5 SYNC
              VALUE 43.
           03 BAQ-APIMETHOD              PIC X(255)
              VALUE 'POST'.
           03 BAQ-APIMETHOD-LEN          PIC S9(9) COMP-5 SYNC
              VALUE 4.
      *===============================================================
       01 REQUEST.
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * This file contains the generated language structure(s) for
      *  request JSON schema 'postCicsParmTran200_request.json'.
      * This structure was generated using 'DFHJS2LS' at mapping level
      *  '4.3'.
      *
      *
      *      06 ReqPathParameters.
      *
      * Comments for field 'StrName':
      * This field represents the value of JSON schema keyword
      *  'ReqPathParameters->StrName'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '200'.
      * This field contains a varying length array of characters or
      *  binary data.
      *        09 StrName-length                PIC S9999 COMP-5 SYNC.
      *        09 StrName                       PIC X(200).
      *
      *
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             06 ReqPathParameters.
               09 StrName-length                PIC S9999 COMP-5 SYNC.
               09 StrName                       PIC X(200).

      *===============================================================
       01 RESPONSE.
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * This file contains the generated language structure(s) for
      *  response JSON schema 'postCicsParmTran200_200_response.json'.
      * This structure was generated using 'DFHJS2LS' at mapping level
      *  '4.3'.
      *
      *
      *      06 RespBody.
      *
      *
      * JSON schema keyword 'RespBody->WK_CICS_RESO' is optional. The
      *  number of instances present is indicated in field
      *  'WK-CICS-RESO2-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *        09 WK-CICS-RESO2-num             PIC S9(9) COMP-5 SYNC.
      *
      *
      *        09 WK-CICS-RESO.
      *
      *
      * JSON schema keyword 'RespBody->WK_CICS_RESO->WK_RESO_05' is
      *  optional. The number of instances present is indicated in
      *  field 'WK-RESO-052-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *          12 WK-RESO-052-num               PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *          12 WK-RESO-05.
      *
      *
      * JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_RESO_SERV_CODE'
      *  is optional. The number of instances present is indicated in
      *  field 'WK-CICS-RESO-SERV-CODE-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *            15 WK-CICS-RESO-SERV-CODE-num    PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *            15 WK-CICS-RESO-SERV-CODE.
      *
      * Comments for field 'WK-CICS-RESO-SERV-CODE2':
      * This field represents the value of JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_RESO_SERV_CODE'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '4'.
      * This field contains a varying length array of characters or
      *  binary data.
      *              18 WK-CICS-RESO-SERV-CO-length   PIC S9999 COMP-5
      *  SYNC.
      *              18 WK-CICS-RESO-SERV-CODE2       PIC X(4).
      *
      *
      * JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_REDF' is
      *  optional. The number of instances present is indicated in
      *  field 'WK-CICS-REDF2-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *            15 WK-CICS-REDF2-num             PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *            15 WK-CICS-REDF.
      *
      *
      * JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_REDF->WK_CICS_TRA
      * N_NAME' is optional. The number of instances present is
      *  indicated in field 'WK-CICS-TRAN-NAME-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *              18 WK-CICS-TRAN-NAME-num         PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *              18 WK-CICS-TRAN-NAME.
      *
      * Comments for field 'WK-CICS-TRAN-NAME2':
      * This field represents the value of JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_REDF->WK_CICS_TRA
      * N_NAME'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '4'.
      * This field contains a varying length array of characters or
      *  binary data.
      *                21 WK-CICS-TRAN-NAME2-length     PIC S9999
      *  COMP-5 SYNC.
      *                21 WK-CICS-TRAN-NAME2            PIC X(4).
      *
      *
      * JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_REDF->FILL_0' is
      *  optional. The number of instances present is indicated in
      *  field 'FILL-0-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *              18 FILL-0-num                    PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *              18 FILL-0.
      *
      * Comments for field 'FILL-02':
      * This field represents the value of JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_REDF->FILL_0'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '4'.
      * This field contains a varying length array of characters or
      *  binary data.
      *                21 FILL-02-length                PIC S9999
      *  COMP-5 SYNC.
      *                21 FILL-02                       PIC X(4).
      *
      *
      * JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN' is
      *  optional. The number of instances present is indicated in
      *  field 'WK-CICS-TRAN2-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *            15 WK-CICS-TRAN2-num             PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *            15 WK-CICS-TRAN.
      *
      *
      * JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_PROG' is optional. The number of instances present is
      *  indicated in field 'WK-CICS-TRAN-PROG-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *              18 WK-CICS-TRAN-PROG-num         PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *              18 WK-CICS-TRAN-PROG.
      *
      * Comments for field 'WK-CICS-TRAN-PROG2':
      * This field represents the value of JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_PROG'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '8'.
      * This field contains a varying length array of characters or
      *  binary data.
      *                21 WK-CICS-TRAN-PROG2-length     PIC S9999
      *  COMP-5 SYNC.
      *                21 WK-CICS-TRAN-PROG2            PIC X(8).
      *
      *
      * JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_PROF' is optional. The number of instances present is
      *  indicated in field 'WK-CICS-TRAN-PROF-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *              18 WK-CICS-TRAN-PROF-num         PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *              18 WK-CICS-TRAN-PROF.
      *
      * Comments for field 'WK-CICS-TRAN-PROF2':
      * This field represents the value of JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_PROF'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '8'.
      * This field contains a varying length array of characters or
      *  binary data.
      *                21 WK-CICS-TRAN-PROF2-length     PIC S9999
      *  COMP-5 SYNC.
      *                21 WK-CICS-TRAN-PROF2            PIC X(8).
      *
      *
      * JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_USCH' is optional. The number of instances present is
      *  indicated in field 'WK-CICS-TRAN-USCH-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *              18 WK-CICS-TRAN-USCH-num         PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *              18 WK-CICS-TRAN-USCH.
      *
      * Comments for field 'WK-CICS-TRAN-USCH2':
      * This field represents the value of JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_USCH'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '8'.
      * This field contains a varying length array of characters or
      *  binary data.
      *                21 WK-CICS-TRAN-USCH2-length     PIC S9999
      *  COMP-5 SYNC.
      *                21 WK-CICS-TRAN-USCH2            PIC X(8).
      *
      *
      * JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_USIN' is optional. The number of instances present is
      *  indicated in field 'WK-CICS-TRAN-USIN-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *              18 WK-CICS-TRAN-USIN-num         PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *              18 WK-CICS-TRAN-USIN.
      *
      * Comments for field 'WK-CICS-TRAN-USIN2':
      * This field represents the value of JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_USIN'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '8'.
      * This field contains a varying length array of characters or
      *  binary data.
      *                21 WK-CICS-TRAN-USIN2-length     PIC S9999
      *  COMP-5 SYNC.
      *                21 WK-CICS-TRAN-USIN2            PIC X(8).
      *
      *
      * JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_TCLA' is optional. The number of instances present is
      *  indicated in field 'WK-CICS-TRAN-TCLA-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *              18 WK-CICS-TRAN-TCLA-num         PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *              18 WK-CICS-TRAN-TCLA.
      *
      * Comments for field 'WK-CICS-TRAN-TCLA2':
      * This field represents the value of JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_TCLA'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '8'.
      * This field contains a varying length array of characters or
      *  binary data.
      *                21 WK-CICS-TRAN-TCLA2-length     PIC S9999
      *  COMP-5 SYNC.
      *                21 WK-CICS-TRAN-TCLA2            PIC X(8).
      *
      *
      * JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_FILL' is optional. The number of instances present is
      *  indicated in field 'WK-CICS-TRAN-FILL-num'.
      * There should be at least '0' instance(s).
      * There should be at most '1' instance(s).
      *              18 WK-CICS-TRAN-FILL-num         PIC S9(9) COMP-5
      *  SYNC.
      *
      *
      *              18 WK-CICS-TRAN-FILL.
      *
      * Comments for field 'WK-CICS-TRAN-FILL2':
      * This field represents the value of JSON schema keyword
      *  'RespBody->WK_CICS_RESO->WK_RESO_05->WK_CICS_TRAN->WK_CICS_TRA
      * N_FILL'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '148'.
      * This field contains a varying length array of characters or
      *  binary data.
      *                21 WK-CICS-TRAN-FILL2-length     PIC S9999
      *  COMP-5 SYNC.
      *                21 WK-CICS-TRAN-FILL2            PIC X(148).
      *
      *
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

             06 RespBody.

               09 WK-CICS-RESO2-num             PIC S9(9) COMP-5 SYNC.

               09 WK-CICS-RESO.

                 12 WK-RESO-052-num               PIC S9(9) COMP-5
            SYNC.

                 12 WK-RESO-05.

                   15 WK-CICS-RESO-SERV-CODE-num    PIC S9(9) COMP-5
            SYNC.

                   15 WK-CICS-RESO-SERV-CODE.
                     18 WK-CICS-RESO-SERV-CO-length   PIC S9999 COMP-5
            SYNC.
                     18 WK-CICS-RESO-SERV-CODE2       PIC X(4).

                   15 WK-CICS-REDF2-num             PIC S9(9) COMP-5
            SYNC.

                   15 WK-CICS-REDF.

                     18 WK-CICS-TRAN-NAME-num         PIC S9(9) COMP-5
            SYNC.

                     18 WK-CICS-TRAN-NAME.
                       21 WK-CICS-TRAN-NAME2-length     PIC S9999
            COMP-5 SYNC.
                       21 WK-CICS-TRAN-NAME2            PIC X(4).

                     18 FILL-0-num                    PIC S9(9) COMP-5
            SYNC.

                     18 FILL-0.
                       21 FILL-02-length                PIC S9999
            COMP-5 SYNC.
                       21 FILL-02                       PIC X(4).

                   15 WK-CICS-TRAN2-num             PIC S9(9) COMP-5
            SYNC.

                   15 WK-CICS-TRAN.

                     18 WK-CICS-TRAN-PROG-num         PIC S9(9) COMP-5
            SYNC.

                     18 WK-CICS-TRAN-PROG.
                       21 WK-CICS-TRAN-PROG2-length     PIC S9999
            COMP-5 SYNC.
                       21 WK-CICS-TRAN-PROG2            PIC X(8).

                     18 WK-CICS-TRAN-PROF-num         PIC S9(9) COMP-5
            SYNC.

                     18 WK-CICS-TRAN-PROF.
                       21 WK-CICS-TRAN-PROF2-length     PIC S9999
            COMP-5 SYNC.
                       21 WK-CICS-TRAN-PROF2            PIC X(8).

                     18 WK-CICS-TRAN-USCH-num         PIC S9(9) COMP-5
            SYNC.

                     18 WK-CICS-TRAN-USCH.
                       21 WK-CICS-TRAN-USCH2-length     PIC S9999
            COMP-5 SYNC.
                       21 WK-CICS-TRAN-USCH2            PIC X(8).

                     18 WK-CICS-TRAN-USIN-num         PIC S9(9) COMP-5
            SYNC.

                     18 WK-CICS-TRAN-USIN.
                       21 WK-CICS-TRAN-USIN2-length     PIC S9999
            COMP-5 SYNC.
                       21 WK-CICS-TRAN-USIN2            PIC X(8).

                     18 WK-CICS-TRAN-TCLA-num         PIC S9(9) COMP-5
            SYNC.

                     18 WK-CICS-TRAN-TCLA.
                       21 WK-CICS-TRAN-TCLA2-length     PIC S9999
            COMP-5 SYNC.
                       21 WK-CICS-TRAN-TCLA2            PIC X(8).

                     18 WK-CICS-TRAN-FILL-num         PIC S9(9) COMP-5
            SYNC.

                     18 WK-CICS-TRAN-FILL.
                       21 WK-CICS-TRAN-FILL2-length     PIC S9999
            COMP-5 SYNC.
                       21 WK-CICS-TRAN-FILL2            PIC X(148).

      *----------------------------------------------------------------*
       01 BAQ-REQUEST-PTR USAGE POINTER.
       01 BAQ-REQUEST-LEN PIC S9(9) COMP-5 SYNC.
       01 BAQ-RESPONSE-PTR USAGE POINTER.
       01 BAQ-RESPONSE-LEN PIC S9(9) COMP-5 SYNC.
       77 COMM-STUB-PGM-NAME PIC X(8) VALUE 'BAQCSTUB'.

      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA             PIC X(200) VALUE SPACES.

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.

      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*
           exec cics receive into(wk-receive)
                             length(wk-receive-length)
           end-exec

           move 200        to StrName-length
           MOVE wk-strname to StrName  IN REQUEST.

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
      *
           EXEC CICS WRITEQ TS QUEUE('VSRS9999')
                     FROM(RESPONSE)
                     LENGTH(200)
           END-EXEC

           exec cics return
           end-exec

           EXIT
           .