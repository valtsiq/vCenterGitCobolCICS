       CBL CICS('COBOL3') APOST
      *****************************************************************
      * PoT para Tivit / Cielo
      *****************************************************************
       identification division.
       program-id.    ciebin02.
       environment    division.
       configuration  section.
       data           division.
       working-storage section.
      *----------------------------------------------------------------*

       01 w-commarea.
          03  w-bin-code         PIC  x(0006) VALUE SPACES.
          03  w-bin-data         PIC  x(0094) VALUE SPACES.

      *----------------------------------------------------------------*
      *   GENERIC WORK VARIABLES                                       *
      *----------------------------------------------------------------*
       01 w-eibresp                   PIC S9(9) COMP-5 SYNC.
      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       linkage section.

       01 dfhcommarea                         pic x(100).

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       procedure division.

      *----------------------------------------------------------------*
       mainline section.

      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*

           move dfhcommarea to w-commarea

           MOVE spaces to w-bin-data

           exec cics read file('CIELOBIN')
                          ridfld(w-bin-code)
                          keylength(6)
                          into(w-commarea)
                          resp(w-eibresp)
           end-exec

           if w-eibresp not equal zeros
              move 'Codigo BIN nao registrado       '
                   to w-bin-data
           end-if

           move w-commarea to dfhcommarea

           exec cics return
           end-exec

           exit
           .