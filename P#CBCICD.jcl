//*********************************************************************
//*                                                                   *
//*                                                                   *
//*                                                                   *
//*     Licensed Materials - Property of IBM                          *
//*                                                                   *
//*     5655-Y04                                                      *
//*                                                                   *
//*     (C) Copyright IBM Corp. 1994, 2014 All Rights Reserved.       *
//*                                                                   *
//*                                                                   *
//*                                                                   *
//*                                                                   *
//*   STATUS = 7.2.0                                                  *
//*                                                                   *
//* CHANGE ACTIVITY :                                                 *
//*                                                                   *
//*   $MOD(DFHYITVL),COMP(SYSGEN),PROD(CICS TS ):                     *
//*                                                                   *
//*  PN= REASON REL YYMMDD HDXXIII : REMARKS                          *
//* $01= A84313 640 040604 HDBGNRB : Migrate PQ84313 from SPA R630    *
//* $L0= Base   410 94     HD3SCWG : Base                             *
//* $P1= D08934 630 030919 HD3SCWG : DFHEILID now in SDFHSAMP         *
//* $P2= D12714 640 041230 HD6KRAH : Compiler level                   *
//*      D85873 690 140226 HDLHJJH : Compiler level 5.1 and 4.2       *
//*     R144470 720 171101 HDFVGMB : Remove LIB as a compile option   *
//*                                                                   *
//*********************************************************************
//DFHYITVL PROC SUFFIX=1$,           Suffix for translator module
//       PGNAME=,
//       INDEX='DFH550.CICS',          HLQ for CICS libraries
//       PROGLIB='USER.CICS.SDFHLOAD', Name of o/p library
//       DSCTLIB='DFH550.CICS.SDFHCOB',  Private macro/dsect
//       SRCELIB='USER.CICS.CBSOURCE',   FONTELIB DEFAULT
//       COMPREP='APA1402.CAZLISTA',     PDS FOR COMPILE REPORT SAVE
//       AD370HLQ='IGY630',          QUALIFIER(S) FOR COBOL COMPILER
//       LE370HLQ='CEE',             Qualifier(s) for LE/390 libraries
//       OUTC=A,                     Class for print output
//       REG=200M,                   Region size for all steps
//       LNKPARM='LIST,XREF,TEST',   LINK EDIT PARAMETERS
//       STUB='DFHEILID',            Lked INC. for DFHELII
//       LIB='SDFHSAMP',             Library
//       WORK=SYSDA                  Unit for work datasets
//*-------------------------------------------------------
//TRN    EXEC PGM=DFHECP&SUFFIX,
//            PARM='COBOL3,SP',
//            REGION=&REG
//STEPLIB  DD DSN=&INDEX..SDFHLOAD,DISP=SHR
//SYSIN    DD DISP=SHR,DSN=&SRCELIB(&PGNAME)
//SYSPRINT DD SYSOUT=&OUTC
//SYSPUNCH DD DSN=&&SYSCIN,
//            DISP=(,PASS),UNIT=&WORK,
//            DCB=BLKSIZE=400,
//            SPACE=(400,(400,100))
//*-------------------------------------------------------
//COB    EXEC PGM=IGYCRCTL,REGION=&REG,
//       PARM='NODYNAM,OBJECT,RENT,APOST,MAP,XREF,LIST,SOURCE'
//STEPLIB  DD DSN=&AD370HLQ..SIGYCOMP,DISP=SHR
//SYSLIB   DD DSN=&DSCTLIB,DISP=SHR
//         DD DSN=&INDEX..SDFHCOB,DISP=SHR
//         DD DSN=&INDEX..SDFHMAC,DISP=SHR
//         DD DSN=&INDEX..SDFHSAMP,DISP=SHR
//SYSPRINT DD DISP=SHR,DSN=&COMPREP(&PGNAME)
//SYSIN    DD DSN=&&SYSCIN,DISP=(OLD,DELETE)
//SYSLIN   DD DSN=&&LOADSET,DISP=(MOD,PASS),
//            UNIT=&WORK,SPACE=(80,(250,100))
//SYSUT1   DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT2   DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT3   DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT4   DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT5   DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT6   DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT7   DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT8   DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT9   DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT10  DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT11  DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT12  DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT13  DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT14  DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSUT15  DD UNIT=&WORK,SPACE=(460,(350,100))
//SYSMDECK DD UNIT=&WORK,SPACE=(460,(350,100))
//*-------------------------------------------------------
//IEBGENER EXEC PGM=IEBGENER,REGION=4096K
//SYSUT1   DD DISP=SHR,DSN=&COMPREP(&PGNAME)
//SYSUT2   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
//*-------------------------------------------------------
//COPYLINK EXEC PGM=IEBGENER,COND=(7,LT,COB)
//SYSUT1   DD DSN=&INDEX..&LIB(&STUB),DISP=SHR
//SYSUT2   DD DSN=&&COPYLINK,DISP=(NEW,PASS),
//            DCB=(LRECL=80,BLKSIZE=400,RECFM=FB),
//            UNIT=&WORK,SPACE=(400,(20,20))
//SYSPRINT DD SYSOUT=&OUTC
//SYSIN    DD DUMMY
//*-------------------------------------------------------
//LKED     EXEC PGM=IEWL,REGION=&REG,
//         PARM='&LNKPARM',COND=(5,LT,COB)
//SYSLIB   DD DSN=&INDEX..SDFHLOAD,DISP=SHR
//         DD DSN=&LE370HLQ..SCEELKED,DISP=SHR
//SYSLMOD  DD DSN=&PROGLIB(&PGNAME),DISP=SHR
//SYSUT1   DD UNIT=&WORK,DCB=BLKSIZE=1024,
//            SPACE=(1024,(200,20))
//SYSPRINT DD SYSOUT=&OUTC
//SYSLIN   DD DSN=&&COPYLINK,DISP=(OLD,DELETE)
//         DD DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD DDNAME=SYSIN