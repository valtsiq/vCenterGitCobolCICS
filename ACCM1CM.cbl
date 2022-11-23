000100*--> COPY 400 BYTES  -   - PROG  OACCZOS -
000200**************************************************************
000400*       AREA COMUM PARA SER UTILIZADO PARA COMMAREA DE       *
000600*       PROGRAMAS NA ESTRUTURA REST/JSON                     *
000800*                                                            *
001000**************************************************************
007480 01         ACC-SEG-01-AREA-TOT.
001200     05      ACC-SEG-01-AREA.
007540       10    ACC-SEG-01-TRAN      PIC X(04) VALUE 'ACMZ'.
007550       10    ACC-SEG-01-USUA      PIC X(03) VALUE '999'.
007560       10    ACC-SEG-01-NOME      PIC X(10) VALUE 'OCNETZSO'.
007570       10    ACC-SEG-01-CHAV      PIC X(14) VALUE 'OCNET24Z'.
007580       10    ACC-SEG-01-OPCA      PIC X(04) VALUE '0111'.
007630       10    ACC-SEG-01-CONS      PIC X(01) VALUE '1'.
007640       10    ACC-SEG-01-LENGJSON  PIC X(05) VALUE '00400'.
007670       10    ACC-SEG-01-PORT      PIC X(04) VALUE '1789'.
007680       10    ACC-SEG-01-N-IP      PIC X(08) VALUE '0AC8C8E1'.
007690       10    ACC-SEG-01-INSTBCO   PIC X(04).
007700       10    ACC-SEG-01-TAMPGM    PIC X(05) VALUE '00300'.
007730       10    ACC-SEG-01-BR15      PIC X(15).
007750       10    ACC-SEG-01-RETO      PIC 9(03).
007760       10    ACC-SEG-01-MEN1      PIC X(20).
      ****************************************************************
      *      AREA DE COMUNICACAO DO  MODULO  OACCM1                  *
      ****************************************************************
      *
         05        ACCM1-CODUSU              PIC X(003) VALUE '999'.
         05        ACCM1-ANO                 PIC X(004) VALUE '2017'.
      *
         05        ACCM1-MES01               PIC X(003).
         05        ACCM1-VAL01               PIC X(012).
         05        ACCM1-MES02               PIC X(003).
         05        ACCM1-VAL02               PIC X(012).
         05        ACCM1-MES03               PIC X(003).
         05        ACCM1-VAL03               PIC X(012).
         05        ACCM1-MES04               PIC X(003).
         05        ACCM1-VAL04               PIC X(012).
         05        ACCM1-MES05               PIC X(003).
         05        ACCM1-VAL05               PIC X(012).
         05        ACCM1-MES06               PIC X(003).
         05        ACCM1-VAL06               PIC X(012).
         05        ACCM1-MES07               PIC X(003).
         05        ACCM1-VAL07               PIC X(012).
         05        ACCM1-MES08               PIC X(003).
         05        ACCM1-VAL08               PIC X(012).
         05        ACCM1-MES09               PIC X(003).
         05        ACCM1-VAL09               PIC X(012).
         05        ACCM1-MES10               PIC X(003).
         05        ACCM1-VAL10               PIC X(012).
         05        ACCM1-MES11               PIC X(003).
         05        ACCM1-VAL11               PIC X(012).
         05        ACCM1-MES12               PIC X(003).
         05        ACCM1-VAL12               PIC X(012).
      *
         05        ACCM1-FILLER              PIC X(032).
      *
         05        ACCM1-CODRET              PIC 9(002).
         05        ACCM1-MSGRET              PIC X(079).
      *
      *