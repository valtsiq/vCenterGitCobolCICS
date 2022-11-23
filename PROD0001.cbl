       CBL CICS('COBOL3') APOST
      *****************************************************************
      * PoT para Tivit / Cielo
      *****************************************************************
       identification division.
       program-id.    prod0001.
       environment    division.
       configuration  section.
       data           division.
       working-storage section.
      *----------------------------------------------------------------*
       01 wk-commarea-out.
      *--> COPY 400 BYTES  -   - PROG  OACCZOS -
      **************************************************************
      *       AREA COMUM PARA SER UTILIZADO PARA COMMAREA DE       *
      *       PROGRAMAS NA ESTRUTURA REST/JSON                     *
      *                                                            *
      **************************************************************
           05      ACC-SEG-01-AREA.
             10    ACC-SEG-01-TRAN      PIC X(04) VALUE 'ACMZ'.
             10    ACC-SEG-01-USUA      PIC X(03) VALUE '999'.
             10    ACC-SEG-01-NOME      PIC X(10) VALUE 'OCNETZSO'.
             10    ACC-SEG-01-CHAV      PIC X(14) VALUE 'OCNET24Z'.
             10    ACC-SEG-01-OPCA      PIC X(04) VALUE '0111'.
             10    ACC-SEG-01-CONS      PIC X(01) VALUE '1'.
             10    ACC-SEG-01-LENGJSON  PIC X(05) VALUE '00400'.
             10    ACC-SEG-01-PORT      PIC X(04) VALUE '1789'.
             10    ACC-SEG-01-N-IP      PIC X(08) VALUE '0AC8C8E1'.
             10    ACC-SEG-01-INSTBCO   PIC X(04).
             10    ACC-SEG-01-TAMPGM    PIC X(05) VALUE '00300'.
             10    ACC-SEG-01-BR15      PIC X(15).
             10    ACC-SEG-01-RETO      PIC 9(03).
             10    ACC-SEG-01-MEN1      PIC X(20).
      ****************************************************************
      *      AREA DE COMUNICACAO DO  MODULO  OACCM1                  *
      ****************************************************************
      *
         05        ACCM1-CODUSU              PIC X(003) VALUE '999'.
         05        ACCM1-ANO                 PIC X(004) VALUE '2017'.
      *
         05        ACCM1-MES01               PIC X(003) value 'jan'.
         05        ACCM1-VAL01        PIC X(012) value '010101010101'.
         05        ACCM1-MES02               PIC X(003) value 'fev'.
         05        ACCM1-VAL02        PIC X(012) value '020202020202'.
         05        ACCM1-MES03               PIC X(003) value 'mar'.
         05        ACCM1-VAL03        PIC X(012) value '030303030303'.
         05        ACCM1-MES04               PIC X(003) value 'apr'.
         05        ACCM1-VAL04        PIC X(012) value '040404040404'.
         05        ACCM1-MES05               PIC X(003) value 'mai'.
         05        ACCM1-VAL05        PIC X(012) value '050505050505'.
         05        ACCM1-MES06               PIC X(003) value 'jun'.
         05        ACCM1-VAL06        PIC X(012) value '060606060606'.
         05        ACCM1-MES07               PIC X(003) value 'jul'.
         05        ACCM1-VAL07        PIC X(012) value '070707070707'.
         05        ACCM1-MES08               PIC X(003) value 'ago'.
         05        ACCM1-VAL08        PIC X(012) value '080808080808'.
         05        ACCM1-MES09               PIC X(003) value 'set'.
         05        ACCM1-VAL09        PIC X(012) value '090909090909'.
         05        ACCM1-MES10               PIC X(003) value 'out'.
         05        ACCM1-VAL10        PIC X(012) value '101010101010'.
         05        ACCM1-MES11               PIC X(003) value 'nov'.
         05        ACCM1-VAL11        PIC X(012) value '111111111111'.
         05        ACCM1-MES12               PIC X(003) value 'dec'.
         05        ACCM1-VAL12        PIC X(012) value '121212121212'.
      *
         05        ACCM1-FILLER       PIC X(032).
      *
         05        ACCM1-CODRET       PIC 9(002) value zeroes.
         05        ACCM1-MSGRET       PIC X(079) value 'Tudo certinho'.
      *
      *
       01 wk-commarea-in.
          03  w-bin-code         PIC  x(0400) VALUE SPACES.

      *----------------------------------------------------------------*
      *   GENERIC WORK VARIABLES                                       *
      *----------------------------------------------------------------*
       01 w-eibresp                   PIC S9(9) COMP-5 SYNC.
      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       linkage section.

       01 dfhcommarea                         pic x(400).

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       procedure division.

      *----------------------------------------------------------------*
       mainline section.

      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*

           move dfhcommarea to wk-commarea-in.
           move wk-commarea-out to dfhcommarea.

           exec cics return
           end-exec

           exit
           .