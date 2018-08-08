C ##############################################################################                    
C #     PROGRAM BUFRUPPRAIR_IF3                                                #
C #                                                                            #
C #      A BUFR INPUT DATA FILE CONTAINS A SERIES OF "MESSAGES" (WHICH ARE     #
C #        VARIABLE LENGTH RECORDS), EACH CONTAINING AT LEAST ONE BUFR         #
C #        "SUB-MESSAGE" (REPORT).  THIS PROGRAM BREAKS THESE OPEN AND PRINTS  #
C #        OUT THE REPORTS, WITH OPTIONS PROVIDED BY THE USER'S CONFIGURATION  #
C #        FILE.                                                               #
C #      2018-04-24 GSP change to bypass filename string parsing               #
C #        which was broken by /glade2 switchover. Brittle, risky code.        #
C #        This is an interim measure before partitioning changes.             #
C #      2018-05-07 GSP change for partitioning based on 2018-04-24 version    #
C #        of bufrupprairfd.f                                                  #
C ##############################################################################
C
        CHARACTER*1 DODIAG        ! SET BY DSS STAFF (NOT BY USERS) TO OBTAIN
C                                 !   EXECUTION OR PERFORMANCE DIAGNOSTICS
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        PARAMETER  ( IIUNIT=11 )  ! BUFR INPUT FILE UNIT
        CHARACTER*128  DIRIN,  DEFIN
        INTEGER        INHALE
        DATA DEFIN(01:10)   /'../bufrobs'/  ! DEFAULT INPUT DIRECTORY
C
C       USERS CAN CHANGE THE DEFAULT INPUT DIRECTORY THROUGH THE CONFIGURATION
C         FILE BY GIVING THEIR COMPLETE PATHNAME TO WHATEVER.
C
        PARAMETER  ( INKSTN=1    )   ! MAXIMUM NUMBER OF INPUT FILES
        PARAMETER  ( LENINMX=192 )   ! MAXIMUM LENGTH OF INPUT BASE FILE NAMES
C        CHARACTER*64 INFILES(INKSTN)! 64 WOULD PICK UP ".le" EXTENSION AND MORE
        CHARACTER*192 INFILES(INKSTN)! 64 WOULD PICK UP ".le" EXTENSION AND MORE         
        CHARACTER*192 INFILE        ! STRING MUST HOLD DIRIN STRING (<=128) PLUS 
C                                   !   INFILES(N) STRING (<=64)          
        CHARACTER*192 NOFILE 
C
C       BUFR INPUT DATA FILE NAMES (GIVEN IN THE USER'S CONFIGURATION FILE) 
C         MUST BE BETWEEN 7 AND LENINMX CHARACTERS LONG, PREFERABLY IN THIS FORM:
C
C           123456789012345678901234567890123
C           gdas.adpupa.t00z.20100323.bufr.le
C           gdas.aircft.t00z.20100323.bufr.le
C           gdas.satwnd.t00z.20100323.bufr
C           gdas.aircar.t00z.20100323.bufr
C
C         THEY MAY OPTIONALLY INCLUDE THE SUFFIX .le (INDICATING LITTLE ENDIAN
C         FORMAT)
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        PARAMETER  ( IPUNIT=21 )  ! PRINT (AKA DUMP OR OUTPUT) FILE UNIT 
        CHARACTER*128  DIROUT, DEFOUT
        INTEGER      EXHALE
C        DATA  DEFOUT(01:10) /'../textobs'/  ! DEFAULT OUTPUT DIRECTORY
C        DATA  DEFOUT(0:1) /' '/  ! DEFAULT OUTPUT DIRECTORY
C
C       USERS CAN CHANGE THE DEFAULT OUTPUT DIRECTORY THROUGH THE CONFIGURATION
C         FILE BY GIVING THEIR COMPLETE PATHNAME TO WHATEVER.
C
C       THERE IS NO PROVISION FOR A LIST OF PRINT FILES, BECAUSE THEY ARE
C         BUILT FROM THE INPUT FILENAMES DURING EXECUTION - THEREBY PRESERVING
C         A CONVENIENT ONE TO ONE RELATIONSHIP
C
        CHARACTER*6   NAMETAG, NAMTAGL
        CHARACTER*4   DASTAG, DASTAGL, TMTAG
        CHARACTER*10  DATETAG
        CHARACTER*250 PRTFILE
C        CHARACTER*192 PRTFILE
C
C       PRINT BASE FILENAMES WILL HAVE THIS FORM:
C           123456789012345678901234567890
C           ADPUPA.2010032300print
C           AIRCFT.2010032300print
C           SATWND.2010032300print
C           AIRCAR.2010032300print
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       COMMAND-LINE ARGUMENT DEFINING FULL PATH TO THE INPUT
C       CONFIGURATION FILE

        CHARACTER*80 argv1, argv2, argv3, argv4, CONFILE
        CHARACTER*6   RINDEX
        CHARACTER*80 infilepath
        CHARACTER*30 filename
        INTEGER IP, len_inpath

        INTEGER NARG              ! NUMBER OF COMMAND-LINE ARGUMENTS
        PARAMETER  ( ICUNIT=8 )   ! CONFIGURATION INPUT FILE

C        CHARACTER*32 CONFILE
C        DATA CONFILE /'bufrupprair_config              '/
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        PARAMETER  ( IDUNIT=7 )   ! DIAGNOSTIC OUTPUT FILE
        CHARACTER*32 DIGFILE
        DATA DIGFILE /'bufrupprair_diagnostics         '/
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        PARAMETER  ( IXUNIT=9 )   ! BUFR TABLE EXAMPLE FILE (NOT USED)
C         ONLY THE MOST GIFTED AND EXPERIENCED NCEP SOFTWARE DEVELOPERS WOULD 
C           WANT TO MAKE USE OF THIS.
C         USERS SHOULD IGNORE, BECAUSE IT WILL HAVE NO SUPPORT
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       VALUES FOR DATA PROCESSING ARRAY DIMENSIONS.  
C         USERS SHOULD NOT MAKE CHANGES TO THESE.
C
        PARAMETER (MXMN=16)
C       PARAMETER (MXREPL=240)
        PARAMETER (MXREPL=1000)
        PARAMETER (MXBF=16000 )
        PARAMETER (NEMLIM=100)
C
C       THIS COMMON BLOCK IS CONNECTED TO BUFRLIB IN MYSTERIOUS WAYS.  USERS
C         USERS SHOULD NOT MAKE CHANGES TO THIS.
C
        COMMON /BITBUF/ MAXBYT,IBIT,IBAY(5000),MBYT(32),MBAY(5000,32)
C
        CHARACTER*100  CSTRING          ! FOR READING ENTRIES FROM THE
C                                       !   CONFIGURATION FILE
        INTEGER INDEX, IDX              ! CSTRING INDEX NUMBERING
C
        CHARACTER*1    DEFAULT, IDOH, IDOHDR
C
        CHARACTER*8    CSUBSET    ! HAS THE CODED RECORD TYPE AND OBSERVATION TYPE
        CHARACTER*6    RECTYPE    ! DECODED RECORD TYPE
        CHARACTER*8    OBSTYPE    ! DECODED OBSERVATION TYPE
        CHARACTER*8    A8RPID
        DATA A8RPID / '        ' /
C
        CHARACTER*6    RECGET(20)       ! FOR LIST OF RECORDS TO GET
        CHARACTER*1    IRECDO
C
C       RECORD AND REPORT COUNTERS
C
        INTEGER      RECORDS, RECSREJ, RECSACC, RECREPS, REPORTS
        INTEGER      REPSACC, REPSREJ
C
        CHARACTER*1    IFILTER
        CHARACTER*8    OBSGET(50)
        CHARACTER*1    IOBSDO
C
        CHARACTER*1    DATEDO
        CHARACTER*1    DATEOK
        CHARACTER*6    RRLEV
C
        CHARACTER*1    KELCEL               ! 'k'/'c'  KELVIN / CELSIUS 
C
        CHARACTER*8    NEMLIST(NEMLIM)  ! FOR EXTRA PARAMETERS
        CHARACTER*1    NMDO
        CHARACTER*10   XN      ! FOR BLANK FILLING IN MNEMONIC USAGE
        DATA XN      / '          ' /
        CHARACTER*1    USEACID, HAVACID ! FOR SPECIAL AIRCRAFT ID USAGE
        DATA USEACID / 'y' /   ! IF 'y', THEN WHEN A8RPID IS MISSING, 
C                              !   AND ACID IS AVAILABLE, USE ACID
C                              ! CHANGE THIS TO 'n' TO PRESERVE A8RPID
        CHARACTER*1    LLDO
        CHARACTER*1    LLWRAP
C
        CHARACTER*1    LLRDO
C
        CHARACTER*1    WMODO
        CHARACTER*5    WMOLIST(100)
        CHARACTER*1    WBBDO
        CHARACTER*2    WBBLIST(100)
C
        CHARACTER*1    IELEVDO
C
        INTEGER        PLEVL, PLEVH
        CHARACTER*1    PLEVDO
C
        CHARACTER*1    ACK
C
C       NEXT TWO STRING SIZES LIMITED BY BUFR LIBRARY TO 80 CHARACTERS.
C         SEE ROUTINE string.f
C
C       MAXIMUM NUMBER OF PARAMETERS RETURNED BY UFBINT FOR A MNEMONIC REQUEST
C         (OF UP TO 80 CHARACTERS) IS 80 / 5 = 16 (MXMN).  COMMON MNEMONICS ARE
C         USUALLY 4 CHARACTERS, OTHERS CAN BE AS LONG AS 8.  SEE:
C         http://www.emc.ncep.noaa.gov/mmb/data_processing/bufrtab_tableb.htm
C         ONE OR MORE OF THE PARAMETERS MAY BE "REPLICATED"
C
        CHARACTER*80 QIDENT               ! REPORT IDENTIFICATION TABLE B MNEMONICS
C
        CHARACTER*80 QBPARM               ! BASIC PARAMETER TABLE B MNEMONICS
C
C       FOLLOWING Q STRINGS NOT USED, AS OF 2010.09.03
C
        CHARACTER*80 QADPUPA              ! ADDITIONAL BASIC PARAMETERS FOR ADPUPA
        CHARACTER*80 QAIRCFT              ! ADDITIONAL BASIC PARAMETERS FOR AIRCFT
        CHARACTER*80 QSATWND              ! ADDITIONAL BASIC PARAMETERS FOR SATWND
        CHARACTER*80 QAIRCAR              ! ADDITIONAL BASIC PARAMETERS FOR AIRCAR
C
        REAL*8       R8IDENT(MXMN,MXREPL) ! ARRAY TO RECEIVE DATA REQUESTED IN QIDENT
        REAL*8       R8BPARM(MXMN,MXREPL) ! ARRAY TO RECEIVE DATA REQUESTED IN QBPARM
        REAL*8       R8XPARM(MXMN,MXREPL) ! ARRAY TO RECEIVE DATA REQUESTED IN NEMLIST
C
        CHARACTER*1 ISBPARM, ISXPARM
C
C       A STRING OF MNEMONICS PROVIDED TO UFBINT CAN NOT INVOLVE MORE THAN ONE
C         "REPLICATION GROUP."  FROM "GUIDE TO WMO TABLE DRIVEN CODE FORMS:"
C           REPLICATION IS THE REPEATING OF A SINGLE PARAMETER OR A GROUP OF
C           PARAMETERS SOME NUMBER OF TIMES, AS IN A TEMP OR PILOT REPORT
C           WITH MANY LEVELS.
C
C       THE BUFRLIB ROUTINE PARUSER WILL COMPLAIN ABOUT INPUT STRING STORE
C         NODES (MNEMONICS), WHEN REPLICATION GETS BROKEN SOMEHOW.
C
        REAL*8 R8CLAT, R8CLON
        REAL*8 R8PRLC(MXREPL), R8PSAL(MXREPL)
        REAL*8 R8GP10(MXREPL), R8GP07(MXREPL), R8FLVL(MXREPL)
C
        REAL*8       R8VSIG(MXREPL)
        REAL         RVSIG(10)
        DATA RVSIG  /   1.0,   2.0,     4.0,    8.0,   16.0, 
     +                 32.0,  64.0,   128.0,  256.0,  512.0 /
        INTEGER      I8VSIG
        CHARACTER*4  A8VSIG(MXREPL)
        CHARACTER*4  VSIGCODE(10)
        DATA VSIGCODE /'   1', 'WXPR', 'TXPR', 'MAXW', 'TROP',
     +                 'MANL', 'SFC ', ' 128', ' 256', ' 512'/
        REAL*8 R8TMDB(MXREPL), R8TMDP(MXREPL), R8REHU(MXREPL)
        REAL*8 R8WDIR(MXREPL), R8WSPD(MXREPL)
C
        REAL*8 R8BIG
        DATA R8BIG / 9999999999.0 /
C
        CHARACTER*8  CHSTR
C
        REAL*8       EXTRA(100,MXREPL)  ! FOR EXTRA PARAMETERS (BEYOND THE BASICS)
C
        CHARACTER*2  MINUTE*2
C
        REAL LATR(100), LONR(100)
        INTEGER RADR
        INTEGER RECDATE, I8DATE, IBEGDAT, IENDDAT
C
        INTEGER   I500, I30     
        DATA I500, I30 / 500, 30 /
C
C       DO NOT CHANGE THESE NEXT TWO DECLARATIONS (THE INTEGER AND THE
C         EQUIVALENCE)
C
        INTEGER IBFMSG(MXBF/4), LN, CODE, Y, Z, JJ, IARGC, N
        CHARACTER    CBFMSG*(MXBF)
C
        EQUIVALENCE (CBFMSG(1:4),IBFMSG(1))     ! NEITHER ARE USED ANYWHERE?
C
        CHARACTER*1200 DUMPHED(3)
C
        CHARACTER*1 DOHEAD, PARSEOK
C
C ##############################################################################
C #     END OF DECLARATIONS  ############## BEGIN INSTRUCTIONS #################
C ##############################################################################
C
        DIRIN(001:032)   = '                                '
        DIRIN(033:064)   = DIRIN(001:032) 
        DIRIN(065:128)   = DIRIN(001:064)
C
        NOFILE(001:064)  = DIRIN(001:064)
        INFILE(001:192)  = DIRIN(001:128)//NOFILE(001:064)
C
        DIROUT(001:128)  = DIRIN(001:128)
        PRTFILE(001:192) = DIROUT(001:128)//NOFILE(001:064)
C
        DIRIN(001:010)   = DEFIN(001:010)       ! INITIALIZE  INPUT DIRECTORY WITH DEFAULT
        DIROUT(000:001)  = DEFOUT(000:001)      ! INITIALIZE OUTPUT DIRECTORY WITH DEFAULT
C
C       THESE NEXT TWO STRING SIZES ARE LIMITED BY THE BUFR
C         LIBRARY - SEE ROUTINE string.f
C
        QIDENT(001:025) = 'WMOB WMOS RPID CLAT CLON '
        QIDENT(026:050) = 'SELV YEAR MNTH DAYS HOUR '
        QIDENT(051:075) = 'MINU                     '
        QIDENT(076:080) = '     '
C
        QBPARM(001:025) = '     PRLC PSAL GP10 GP07 '
        QBPARM(026:050) = 'FLVL VSIG WDIR WSPD TMDB '
        QBPARM(051:075) = 'TMDP REHU                '
        QBPARM(076:080) = '     '
C
C       SET THE HEADER STRINGS (FOR THE DEFAULT MODE - LATER RESET IF
C         NOT IN DEFAULT MODE)
C       ================================================================
        DUMPHED(1)(001:028) = ' REC      OBS       REPORT T'
        DUMPHED(1)(029:068) = 'IME   STN WMO     LATI-   LONGI-   STN  '  
        DUMPHED(1)(069:108) = '  SEQ  VSIG   PRES    PSAL     GEOPOT   '
        DUMPHED(1)(109:148) = '   GP07     FLVL   AIR     DEW-  REL    '
        DUMPHED(1)(149:188) = ' WIND    WIND     |                     '
        IHDEND = 171
        DUMPHED(1)(189:200) = '             '
C
        DUMPHED(1)(201:240) = '                                        '
        DUMPHED(1)(241:280) = DUMPHED(1)(201:240)
        DUMPHED(1)(281:360) = DUMPHED(1)(201:280)
        DUMPHED(1)(361:520) = DUMPHED(1)(201:360)
        DUMPHED(1)(521:840) = DUMPHED(1)(201:520)
        DUMPHED(1)(841:1200)= DUMPHED(1)(201:560)
C
        DUMPHED(2)(001:028) = ' TYPE     TYPE      YYYYMMDD'
        DUMPHED(2)(029:068) = 'HHMM  / OTHER ID  TUDE     TUDE    ELEV '
        DUMPHED(2)(069:108) = '   NO  CODE   (MB)    (MB)     (M2/S2)  '
        DUMPHED(2)(109:148) = ' (M2/S2)    (M)    TEMP   POINT  HUM    '
        DUMPHED(2)(149:188) = ' DIR    SPD(M/S)  |                     '
        IHDEND = 171
        DUMPHED(2)(189:200) = '                  '
C
        DUMPHED(2)(201:240) = '                                        '
        DUMPHED(2)(241:280) = DUMPHED(2)(201:240)
        DUMPHED(2)(281:360) = DUMPHED(2)(201:280)
        DUMPHED(2)(361:520) = DUMPHED(2)(201:360)
        DUMPHED(2)(521:840) = DUMPHED(2)(201:520)
        DUMPHED(2)(841:1200)= DUMPHED(2)(201:560)
C       ================================================================
C
        FILL9  = 999.9
        FILL99 = -9999.90
        XN = '          '
C
C       N = IARGC()     ! GNU FORTRAN: NUMBER OF ARGUMENTS PASSED ON THE
C                       !   COMMAND LINE
C
        DO N = 1, INKSTN
          INFILES(N) = NOFILE(001:192)
        ENDDO
        INK = 0
C
C       WRITE (*,*) 'initialize default configuration'
C
C ##############################################################################
C #     INITIALIZE SELECTION CONFIGURATION AND SET DEFAULTS                    #
C ##############################################################################
C
        DODIAG = 'n'
        DEFAULT = 'y'           ! WHEN SET TO 'n', PROGRAM WILL GET ONLY THE
C                               !   PARAMETERS SPECIFIED BY THE USER. E.G. JUST
C                               !   ONE, LIKE TEMPERATURE
        IDOHDR = 'n'            ! HEADER PRINTOUT CONTROL
        IDOH = 'n'              ! HEADER PRINTOUT CONTROL
C-------
        IRECBEG = 1
        IRECEND = 10
        DO N = 1, 20
          RECGET(N) = '        '
        ENDDO
        NREC = 0
        IRECDO = 'n'
C
        OBSGET(1) = '        '  ! INDEX 026   n/a                 BUFR (report) types to get
        IOBSBEG = 1
        IOBSEND = 8
        DO N = 2, 50
          OBSGET(N) = '        '
        ENDDO
        NOBS = 0
        IOBSDO = 'n'
C-------
        IBEGDAT = 99            ! INDEX 031   YEAR_MNTH_DAY_HOUR  beginning date
        IENDDAT = 99            ! INDEX 031   YEAR_MNTH_DAY_HOUR  ending date
        DATEDO = 'n'
C-------
        DO N = 1, NEMLIM
          NEMLIST(N) = '        ' ! INDEX 041   e.g. PMSL           list of extra mnemonics (parameters)
C                       see  http://www.emc.ncep.noaa.gov/mmb/data_processing/bufrtab_tableb.htm
        ENDDO
        NML = 0
        INBEG = 1
        INEND = INBEG + 14
        NMDO = 'n'
C-------
        LATS = -90              ! INDEX 051   LAT                 southern latitude of a lat-lon box
        LATN = 90               ! INDEX 051   LAT                 northern latitude of a lat-lon box
        LONW = -180             ! INDEX 051   LON                 western longitude of a lat-lon box
        LONE = 180              ! INDEX 051   LON                 eastern longitude of a lat-lon box
        LLDO = 'n'
C-------
        RADR = 0                ! INDEX 061   n/a                 radius of all circles, kilometers
        DO N = 1, 100
          LATR(N) = 99.99       ! INDEX 061   LAT                 latitude  for a circle locus
          LONR(N) = 999.99      ! INDEX 061   LON                 longitude for a circle locus
        ENDDO
        NIR = 0
        IRBEG = 1
        IREND = IRBEG + 3
        LLRDO = 'n'
C-------
        DO N = 1, 100
          WMOLIST(N) = '     '  ! INDEX 071   RPID                list of stations (WMO numbers)
          WBBLIST(N) = '  '     ! INDEX 076   RPID                list of stations (WMO numbers)
        ENDDO
        NWMO = 0
        NWBB = 0
        IWBEG = 1
        IWEND = IWBEG + 9
        WMODO = 'n'
        WBBDO = 'n'
C-------
        IELEVL = -1000          ! INDEX 081   SELV                lowest  station elevation, meters
        IELEVH = 12000          ! INDEX 081   SELV                highest station elevation, meters
        IELEVDO = 'n'
C-------
        PLEVL = 1100
        PLEVH = 1
        PLEVDO = 'n'
C-------
C
        KELCEL = 'c'            ! INDEX 0     
C
C ##############################################################################
C #     OPEN AND READ THE CONFIGURATION FILE                                   #
C ##############################################################################
C
        NARG=IARGC()
        IF (NARG .LT. 4) THEN
          WRITE (*,*) 'Enter 4 fields separated by spaces'
          WRITE (*,*) 'rindex input_dir/ filename config_file'
          STOP 'Insufficient input given'
        ELSE
          CALL GETARG(1,argv1)
            RINDEX = argv1
          CALL GETARG(2,argv2)
            infilepath = argv2
          CALL GETARG(3,argv3)
            filename = argv3
          CALL GETARG(4,argv4)
            CONFILE = argv4
            
          DO 15, IP = LEN(infilepath), 1, -1 
            IF(infilepath(IP:IP) .NE. ' ') GO TO 20 
 15       CONTINUE 
 20       len_inpath = IP

          WRITE (*,*) 'rindex infilepath filename config_file'
          WRITE (*,*) RINDEX, infilepath(1:len_inpath), filename, CONFILE
          WRITE (*,*) RINDEX//"_"//infilepath(1:len_inpath)//"_"
     +                //filename//"_"//CONFILE
        ENDIF

        OPEN (ICUNIT, FILE=CONFILE)
C
C        WRITE (*,*) 'opening configuration file'
C
        IFILTER = 'n'
C
        DO IC = 1, 14000        ! BEGIN  CONFIGURATION FILE READS
C 
          IF (DODIAG.EQ.'y')  THEN
            WRITE (*,*)  'read line ',ic,' from the configuration file'
          ENDIF
C
          READ (ICUNIT,8020,IOSTAT=IOS)  CSTRING  ! READ NEXT ENTRY IN CONFIGURATION FILE
8020      FORMAT (A)
          IF (IOS.NE.0)  EXIT
          READ (CSTRING,'(I3)')  INDEX
C
C         WRITE (*,*)  '  just read the index ',index,' off of the line'
C 
          IF (INDEX.EQ.999)  THEN
            EXIT
          ENDIF
          IDX = INDEX / 10
          IF (IDX.EQ. 0)  THEN
C
C           FOR NCAR/CISL/DSS USAGE ONLY, FOR DIAGNOSTIC RUNS
C
            READ (CSTRING,'(4X,A1)')  DODIAG
            CYCLE
          ENDIF
          IF (IDX.EQ. 1)  THEN
C
C           PROVIDE A LIST OF INPUT FILE NAMES (MANDATORY)
C             AND PREFERRED INPUT AND/OR OUTPUT DIRECTORIES,
C
            IF (INDEX.EQ.15)  THEN
              READ (CSTRING,'(4X,A128)')  DIRIN(001:128)
            ENDIF
            IF (INDEX.EQ.16)  THEN
              READ (CSTRING,'(4X,A128)')  DIROUT(001:128)
            ENDIF
            IF (INDEX.EQ.11)  THEN
              INK = INK + 1
              IF (INK.LE.INKSTN)  THEN
c                 write (*,*)  'ink ',ink,'  inkstn ',inkstn,
c    +                         'leninmx ',leninmx
                READ (CSTRING,'(4X,A192)')  INFILES(INK)(1:LENINMX)
                DO N = 1, LENINMX
                  IF (INFILES(INK)(N:N).EQ.' ')  THEN
                    LENINFN = N - 1
                    IF (LENINFN.LT.7)  THEN
                      INK = INK - 1
                      CYCLE
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
            CYCLE
          ENDIF
          IF (IDX.EQ. 2)  THEN
C
C           SELECT RECORD AND/OR REPORT TYPES
C
            IF (INDEX.EQ.21)  THEN
              READ (CSTRING,'(4X,10(A6,1X))')
     +        (RECGET(N),N=IRECBEG,IRECEND)
              IF (IRECEND.LT.20)  THEN
                IRECBEG = IRECBEG + 10
                IRECEND = IRECBEG + 9
                IRECDO = 'y'
                IFILTER = 'y'
              ENDIF
            ENDIF
            IF (INDEX.EQ.26)  THEN
              READ (CSTRING,'(4X,8(A8,1X))')
     +        (OBSGET(N),N=IOBSBEG,IOBSEND)
              IF (IOBSEND.LT.48)  THEN
                IOBSBEG = IOBSBEG + 8
                IOBSEND = IOBSBEG + 7
                IOBSDO = 'y'
                IFILTER = 'y'
              ELSE
                IOBSBEG = 49
                IOBSEND = 50
              ENDIF
            ENDIF
            CYCLE
          ENDIF
          IF (IDX.EQ. 3)  THEN
C
C           SELECT A RANGE OF DATES TO EXTRACT
C
            READ (CSTRING,'(4X,I10,2X,I10)',IOSTAT=IOS) IBEGDAT, IENDDAT  ! YYYYMMDDHH
            IF (IOS.NE.0)  CYCLE
            IF (IBEGDAT.GE.1970010100.AND.IENDDAT.GE.IBEGDAT)  THEN
              DATEDO = 'y'
              IFILTER = 'y'
            ENDIF
            CYCLE
          ENDIF
          IF (IDX.EQ. 4)  THEN
C
C           SPECIFY THE MNEMONICS FOR ADDITIONAL PARAMETERS TO SELECT
C
            IF (INDEX.EQ.41.AND.CSTRING(5:5).EQ.'n')  THEN
              DEFAULT = 'n'
C
C             MUST RESET THE HEADER STRINGS
C
        DUMPHED(1)(001:028) = ' REC      OBS       REPORT T'
        DUMPHED(1)(029:068) = 'IME   STATION   LATI-   LONGI-   ELE-   '
        DUMPHED(1)(069:103) = ' SEQ  VERT                              '
        DUMPHED(1)(104:143) = '                                        '
        DUMPHED(1)(144:183) = '                                        '
        DUMPHED(1)(184:200) = '                 '
C
        DUMPHED(2)(001:028) = ' TYPE     TYPE      YYYYMMDD'
        DUMPHED(2)(029:068) = 'HHMM  BBSSS     TUDE     TUDE   VATION  '
        DUMPHED(2)(069:103) = '  NO   USE                              '
        DUMPHED(2)(104:143) = '                                        '
        DUMPHED(2)(144:183) = '                                        '
        DUMPHED(2)(184:200) = '                 '
C
              IHDEND =  81
              CYCLE
            ENDIF
            IF (INDEX.EQ.41.AND.CSTRING(5:5).EQ.'y')  THEN
              CYCLE
            ENDIF
            NPT = 0
            DO LK = 5, 79       ! LOOK (SCAN) FOR MNEMONICS -
              IF (CSTRING(LK:LK).NE.' ')  THEN
                NPT = NPT + 1           ! ANOTHER CHARACTER IN THIS MNEMONIC
                IF (NPT.EQ.1)  NML = NML + 1    ! ANOTHER MNEMONIC FOR OUR LIST
                IF (NML.GT.NEMLIM)  THEN
                  CYCLE
                ENDIF
                IF (NPT.LE.8)  THEN     ! ADD THIS CHARACTER TO THE NMLth MNEMONIC
                  NEMLIST(NML)(NPT:NPT) = CSTRING(LK:LK)
                ENDIF
              ELSE
                IF (NPT.GT.0)  THEN
                  I = IHDEND
                  M = NML
                  N = NPT
C                 IF (N.LE.9)  THEN       ! PUT THIS MNEMONIC IN THE HEADER
                  IF (N.LE.7)  THEN       ! PUT THIS MNEMONIC IN THE HEADER
C                   DUMPHED(2)(I+1:I+10) = XN(1:10-N)//NEMLIST(M)(1:N)
                    DUMPHED(2)(I+1:I+8)  = XN(1:8-N)//NEMLIST(M)(1:N)
                  ELSE
C                   DUMPHED(2)(I+1:I+10) =             NEMLIST(M)(1:N)
                    DUMPHED(2)(I+1:I+8)  =             NEMLIST(M)(1:8)
                  ENDIF
C                 IHDEND = IHDEND + 12
                  IHDEND = IHDEND + 10
                  NPT = 0
                ENDIF
              ENDIF
            ENDDO
C
C           IDEALLY WE SHOULD VERIFY THE USER'S MNEMONICS WITH THE
C             NCEP BUFR TABLES
C
            IF (NML.GT.0)  THEN
              NMDO = 'y'
              IFILTER = 'y'
            ENDIF
            CYCLE
          ENDIF
          IF (IDX.EQ. 5)  THEN
C
C           SELECT REPORTS FROM A LATITUDE-LONGITUDE WINDOW
C
            IF (CSTRING(05:10).EQ.'      '.OR.
     +          CSTRING(11:16).EQ.'      '.OR.
     +          CSTRING(17:22).EQ.'      '.OR.
     +          CSTRING(23:28).EQ.'      ')  THEN
              CYCLE
            ENDIF
            READ (CSTRING,'(4X,4I6)',IOSTAT=IOS)  LATS, LATN, LONW, LONE
            IF (IOS.NE.0)  CYCLE
            IF (LATS.NE.-90.OR.LATN.NE.90.
     +       OR.LONW.NE.-180.OR.LONE.NE.180)  THEN
              LLDO = 'y'
              IFILTER = 'y'
            ENDIF
C
C           WHEN THE "WINDOW" INCLUDES THE D.L., WE NEED TO DEAL WITH THE
C             DATE LINE, WHERE, SCANNING EASTWARD, THE SIGN OF THE LONGITUDE
C             FLIPS FROM POSITIVE TO NEGATIVE, I.E. FROM 179.9 TO -179.9
C           FIRST WE NEED TO RECOGNIZE THAT WE ARE DEALING WITH SUCH A FLIP,
C             OR WRAP-AROUND, WHICH OCCURS WHEN LONE < LONW
C
            IF (LONW.LT.LONE)  THEN
              LLWRAP = 'n'
            ELSE
              LLWRAP = 'y'
            ENDIF
            CYCLE
          ENDIF
          IF (IDX.EQ. 6)  THEN
C
C           SELECT REPORTS WITHIN A CIRCLE (RADIUS) OF A LOCATION
C
            IF (INDEX.EQ.61.AND.CSTRING(1:4).NE.'   ')  THEN
              READ (CSTRING,'(4X,I3)',IOSTAT=IOS)  RADR
              IF (IOS.NE.0)  CYCLE
              IF (RADR.LT.5.OR.RADR.GT.999)  THEN     ! TOO SMALL OR TOO BIG
                RADR = 0
                CYCLE
              ENDIF
            ENDIF
            IF (INDEX.EQ.62.AND.RADR.NE.0)  THEN
              DO  K = 5, 53, 16
                IF (CSTRING(K  :K+ 7).EQ.'        '.OR.
     +              CSTRING(K+8:K+15).EQ.'        ')  THEN
                  CYCLE
                ENDIF
                READ (CSTRING(K:K+15),'(2F8.2)',IOSTAT=IOS)
     +            XLATR, XLONR
                IF (IOS.NE.0)  CYCLE
                IF (XLATR.GE. -90.AND.XLATR.LE. 90.AND.
     +              XLONR.GE.-180.AND.XLONR.LE.180)  THEN
                  NIR = NIR + 1
                  LATR(NIR) = XLATR
                  LONR(NIR) = XLONR
                  LLRDO = 'y'
                  IFILTER = 'y'
                ENDIF
              ENDDO
              CYCLE
            ENDIF
          ENDIF
          IF (IDX.EQ. 7)  THEN
C
C           SELECT ADPSFC OR ADPUPA STATIONS BY WMO NUMBER OR WMO BLOCK
C
            IF (INDEX.EQ.71)  THEN
              READ (CSTRING,'(4X,10(A5,1X))')
     +          (WMOLIST(N),N=IWBEG,IWEND)
              IF (IWEND.LE.90)  THEN
                IWBEG = IWBEG + 10
                IWEND = IWBEG + 9
                WMODO = 'y'
                IFILTER = 'y'
              ENDIF
              CYCLE
            ENDIF
            IF (INDEX.EQ.76)  THEN
              READ (CSTRING,'(4X,10(A2,1X))')
     +          (WBBLIST(N),N=IWBEG,IWEND)
              IF (IWEND.LE.90)  THEN
                IWBEG = IWBEG + 10
                IWEND = IWBEG + 9
                WBBDO = 'y'
                IFILTER = 'y'
              ENDIF
              CYCLE
            ENDIF
          ENDIF
          IF (IDX.EQ. 8)  THEN
C
C           SELECT OBSERVATION PLATFORM LEVEL (STATION ELEVATION OR
C             PRESSURE LEVEL OF A SOUNDING)
C
            IF (INDEX.EQ.81)  THEN
              IF (CSTRING(05:10).NE.'      '.AND.
     +            CSTRING(11:16).NE.'      ')  THEN
                READ (CSTRING,'(4X,2I6)',IOSTAT=IOS)  IELEVL, IELEVH
                IF (IOS.NE.0)  CYCLE
                IF (IELEVL.GE.-1000.AND.IELEVH.LE.12000)  THEN
                  IELEVDO = 'y'
                  IFILTER = 'y'
                ENDIF
              ENDIF
            ENDIF
            IF (INDEX.EQ.86)  THEN
              IF (CSTRING(05:10).NE.'      '.AND.
     +            CSTRING(11:16).NE.'      ')  THEN
                READ (CSTRING,'(4X,2I6)',IOSTAT=IOS)  PLEVL, PLEVH
                IF (IOS.NE.0)  CYCLE
                IF (PLEVL.LE.1100.AND.PLEVH.GT.0.AND.
     +              PLEVH.LE.PLEVL)  THEN
                  PLEVDO = 'y'
                  IFILTER = 'y'
                ENDIF
              ENDIF
            ENDIF
            CYCLE
          ENDIF
          IF (IDX.EQ. 9)  THEN
C
C           VARIOUS UNIT CHANGES, ETC.
C 
            IF (INDEX.EQ.91)  THEN    ! TEMPERATURES IN KELVIN OR CELSIUS
              READ (CSTRING,'(4X,A1)',IOSTAT=IOS)  KELCEL
              IF (IOS.NE.0)  CYCLE
              IF (KELCEL.NE.'c'.AND.KELCEL.NE.'k')  THEN
                KELCEL = 'c'
              ENDIF
            ENDIF
            CYCLE
          ENDIF
C
C         IF (IDX.EQ.10)  THEN
C           CYCLE
C         ENDIF
C
        ENDDO                   ! END OF CONFIGURATION FILE READS
C
        CLOSE (ICUNIT )
C ##############################################################################
C
        IF (IRECDO.EQ.'y')  THEN
          DO N = 1, 20
            IF (RECGET(N).EQ.'      ')  THEN
              NREC = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NREC = 0
        ENDIF
        IF (IOBSDO.EQ.'y')  THEN
          DO N = 1, 50
            IF (OBSGET(N).EQ.'        ')  THEN
              NOBS = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NOBS = 0
        ENDIF
        IF (WMODO.EQ.'y')  THEN
          DO N = 1, 100
            IF (WMOLIST(N).EQ.'     ')  THEN
              NWMO = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NWMO = 0
        ENDIF
        IF (WBBDO.EQ.'y')  THEN
          DO N = 1, 100
            IF (WBBLIST(N).EQ.'  ')  THEN
              NWBB = N - 1
              EXIT
            ENDIF
          ENDDO
        ELSE
          NWBB = 0
        ENDIF
C
        IF (WMODO.EQ.'y'.OR.WBBDO.EQ.'y')  THEN
          IF (IOBSDO.EQ.'n')  THEN
            IOBSDO = 'y'
            NOBS = 2
            OBSGET(1) = 'RAOBF   '
            OBSGET(2) = 'PIBAL   '
C
C           STUFF BEING PUT IN OBSGET(3) AND OBSGET(4) IS JUST
C             FOR THE CONFIGURATION PRINTOUT, AND NOTHING ELSE.
C             I.E., SELECTING WMO NUMBERS OR BLOCKS WORKS ONLY
C             FOR THE ADPUPA REPORT TYPES RAOBF AND PIBAL
C
            OBSGET(3) = '  forced'
            IF (WMODO.EQ.'y')  OBSGET(4) = 'by WMODO'
            IF (WBBDO.EQ.'y')  OBSGET(4) = 'by WBBDO'
            OBSGET(5) = '        '
          ENDIF
        ENDIF
C
        WRITE (*,*)
        WRITE (*,*)  'CONFIGURATION FILE HAS BEEN ACCEPTED:'

C       write (*,*)  'files to be read'
C       write (*,7777)  (infiles(iii),iii=1,10)
7777    format (10(/,1x,a64))

C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +     PRINT THE CONFIGURATION TO THE USER'S SCREEN                           +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        WRITE (*,9070)
     +      IRECDO, NREC, RECGET(1),  RECGET(2),  RECGET(3),  RECGET(4),
     +      IOBSDO, NOBS, OBSGET(1),  OBSGET(2),  OBSGET(3),  OBSGET(4),
     +      DATEDO, IBEGDAT, IENDDAT,
     +      NMDO,  NML, (NEMLIST(NM),NM=1,20),
     +      LLDO, LATS, LATN, LONW, LONE,
     +      LLRDO, RADR,
     +      NIR, LATR(1), LONR(1), LATR(2), LONR(2), LATR(3), LONR(3),
     +      IELEVDO, IELEVL, IELEVH,
     +       PLEVDO,  PLEVL,  PLEVH, 
     +      WMODO, NWMO, (WMOLIST(NZ),NZ=1,20),
     +      WBBDO, NWBB, (WBBLIST(NZ),NZ=1,10)
C
        IF (DODIAG.EQ.'y'.OR.DODIAG.EQ.'x')  THEN
C
C ##############################################################################
C #       OPEN THE DIAGNOSTIC FILE, WHEN NEEDED                                #
C ##############################################################################
C
          OPEN (IDUNIT, FILE=DIGFILE)    ! IF YOU WRITE TO IDUNIT WITHOUT 
C                                        ! PREVIOUSLY DOING THIS OPEN, THEN
C                                        ! IT WILL CREATE A fort.7 OUTPUT FILE
C                                        ! IN THE bufrobs DIRECTORY
          WRITE (IDUNIT,9064)  DIGFILE
9064      FORMAT (/,1X,'DIAGNOSTIC FILE ',A32,' OPENED')
C
          WRITE (IDUNIT,9065)  CONFILE
9065      FORMAT (/,1X,'CONFIGURATION FILE ',A32,' OPENED',
     +            /,3X,'CHOICES FOLLOW')
C
          WRITE (*,*)
          WRITE (*,*)  'DIAGNOSTIC FILE HAS BEEN OPENED'
C
        ENDIF
C
C ##############################################################################
C
        IF (DODIAG.EQ.'y')  THEN
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +       PRINT THE CONFIGURATION IN THE DIAGNOSTIC FILE                       +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
          WRITE (IDUNIT,9070)
     +      IRECDO, NREC, RECGET(1),  RECGET(2),  RECGET(3),  RECGET(4),
     +      IOBSDO, NOBS, OBSGET(1),  OBSGET(2),  OBSGET(3),  OBSGET(4),
     +      DATEDO, IBEGDAT, IENDDAT,
     +      NMDO,  NML, (NEMLIST(NM),NM=1,20),
     +      LLDO, LATS, LATN, LONW, LONE,
     +      LLRDO, RADR,
     +      NIR, LATR(1), LONR(1), LATR(2), LONR(2), LATR(3), LONR(3),
     +      IELEVDO, IELEVL, IELEVH,
     +       PLEVDO,  PLEVL,  PLEVH,
     +      WMODO, NWMO, (WMOLIST(NZ),NZ=1,20),
     +      WBBDO, NWBB, (WBBLIST(NZ),NZ=1,10)
9070      FORMAT (/,
     +      ' -------------------------------------------------------',/
     +      '    FILTER   USE  CRITERIA',/
     +      ' RECORD TYPE  ',A1,'   RECGET  (1- 4 OF',
     +            I3,')', 4(2X,A6),/
     +      '    OBS TYPE  ',A1,'   OBSGET  (1- 4 OF',
     +            I3,')',4(2X,A8),/
     +      '   DATE/TIME  ',A1,'   IBEGDAT ',I10,'  IENDDAT ',I10,/
     +      '   MNEMONICS  ',A1,'   NEMLIST (1-20 OF',
     +            I3,')',10(2X,A8),/35X,10(2X,A8),/
     +      '   LAT-LON    ',A1,'   LATS',I4,'  LATN',I4,
     +                          '  LONW',I5,'  LONE',I5,/
     +      '   CIRCLES    ',A1,'   RADR ',I6,/
     +      '                  LATR, LONR (1- 3 OF',
     +                          I3,') ',3(1X,F6.2,', ',F7.2,1X),/
     +      '  ELEVATION   ',A1,'   IELEVL ',I5,'  IELEVH ',I5,/
     +      ' PRESS LEVELS ',A1,'    PLEVL ',I5,'   PLEVH ',I5,/
     +      'WMO STATIONS  ',A1,'   WMOLIST (1-20 OF',I3,') ',
     +                          10(A5,1X),/36X,10(A5,1X),/
     +      'WMO BLOCKS    ',A1,'   WBBLIST (1-10 OF',I3,') ',
     +                          10(A2,1X)/,
     +      ' -------------------------------------------------------')
        ENDIF
C       
C       /glade/data02/dsswork/baseball/datasets/ds351.0/bufr_configdecode_ADPUPA/bufrobs
C       1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
C                1         2         3         4         5         6         7         8         9        10
C       gdas.adpupa.t00z.20100901.bufr.le
c
        DO  III = 128, 1, -1
          IF (DIRIN(III:III).NE.' '.AND.DIRIN(III:III).NE.'/')  THEN
            DIRIN(III+1:III+1) = '/'
            INHALE = III + 1
            EXIT
          ENDIF
        ENDDO
        DO  III = 128, 1, -1
          IF (DIROUT(III:III).NE.' '.AND.DIROUT(III:III).NE.'/')  THEN
            DIROUT(III+1:III+1) = '/'
            EXHALE = III + 1
            EXIT
          ENDIF
        ENDDO
C
C ##############################################################################
C #                                                                            #
C #     TOP OF MAIN LOOP ON INPUT FILES                                        #
C #                                                                            #
        KNK = 0                                                                #
        DOFILS: DO              ! LOOP TO READ BUFR DATA FILES                 #
C #                                                                            #
C ##############################################################################
C
        KNK = KNK + 1
        IF (KNK.GT.INKSTN)  EXIT DOFILS

        INFILE = infilepath(1:len_inpath)//filename
        WRITE (*,*)  'DODIAG, INFILE: ', DODIAG, " ", INFILE
 
C        IF (KNK.GT.INK)  EXIT DOFILS
C        INFILE = DIRIN(1:INHALE)//INFILES(KNK)
C        INFILE = INFILES(KNK)
C
C ##############################################################################
C #     OPEN THE BUFR DATA FILE, WHICH IS PACKED BINARY                        #
C ##############################################################################
C
        OPEN (IIUNIT, FILE=INFILE, FORM='UNFORMATTED' )
        IF (DODIAG.EQ.'y')  THEN
C         WRITE (IDUNIT,9090)  KNK, INK, INFILE(1:INHALE+LENINMX)
          WRITE (IDUNIT,9090)  KNK, INK, INFILE
9090      FORMAT (/,1X,'BUFR DATA INPUT FILE ',I5,' OF ',I5,' OPENED ',
     +      A128)
        ENDIF
        WRITE (*,*)
        WRITE (*,*)  'BUFR DATA INPUT FILE ',KNK,' OF ',INK,' OPENED ',
     +    INFILE
C    +    INFILE(1:INHALE+LENINMX)
C
C       ASSOCIATE THE TABLES FILE WITH THE MESSAGES FILE, AND IDENTIFY
C       THE LATTER TO THE BUFRLIB SOFTWARE.
C      
        CALL OPENBF (IIUNIT,'IN',11)
C
C       OPENBF WILL BALK TRYING TO OPEN A REGULAR BUFR FILE ON A LITTLE-ENDIAN 
C         MACHINE.  IT WILL REPORT THAT THE STRING "BUFR" CAN NOT BE FOUND.  THE 
C         USER WILL NEED TO CONVERT THE FILE TO LITTLE-ENDIAN.
C       THE FILES DSS HAS ALREADY CONVERTED WILL HAVE THE SUFFIX '.le'
C
        PARSEOK = 'y'
        LENEND = 0
        DO N = 1, LENINMX
          IF (LENEND.EQ.0.AND.INFILES(KNK)(N:N).EQ.' ')  LENEND = N - 1
          IF (N.GE. 6.AND.N.LE.11)  THEN
            IF (INFILES(KNK)(N:N).EQ.' ')  PARSEOK = 'n'
          ENDIF
          IF (N.GE.14.AND.N.LE.15)  THEN
            IF (INFILES(KNK)(N:N).EQ.' ')  PARSEOK = 'n'
          ENDIF
          IF (N.GE.18.AND.N.LE.25)  THEN
            IF (INFILES(KNK)(N:N).EQ.' ')  PARSEOK = 'n'
          ENDIF
          IF (PARSEOK.NE.'y')  EXIT
        ENDDO
        IF (PARSEOK.EQ.'y')  THEN
          NAMTAGL(01:06) = INFILES(KNK)(42:47)
          DASTAGL(01:04) = INFILES(KNK)(37:40)
          CALL LOW2UP (NAMTAGL,NAMETAG,6)
          CALL LOW2UP (DASTAGL,DASTAG,4)
          DATETAG(01:08) = INFILES(KNK)(54:61)
          DATETAG(09:10) = INFILES(KNK)(50:51)
          IF (INFILES(KNK)(63:64) .EQ. 'tm') THEN
            TMTAG(01:04) = 'TM'//INFILES(KNK)(65:66)
            PRTFILE = DASTAG//'.'//NAMETAG//'.'
     +                //DATETAG//'.'//TMTAG//'.txt'
          ELSE
            PRTFILE = DASTAG//'.'//NAMETAG//'.'
     +                //DATETAG//'.txt'
          ENDIF
        ELSE
          PRTFILE = INFILES(KNK)(30:LENEND)//'_print'
        ENDIF

C       override file string parsing
C       PRTFILE = './'//INFILES(KNK)(48:LENEND)//'_'//RINDEX//'.txt'

        PRTFILE = './'//filename//'_'//RINDEX//'.txt'
        WRITE (*,*)  'PRTFILE ', PRTFILE
C
C ##############################################################################
C #     OPEN BUFR PRINTOUT ("DUMP") FILE                                       #
C ##############################################################################
C
        OPEN(IPUNIT,FILE=PRTFILE,STATUS='UNKNOWN',FORM='FORMATTED')
        IF (DODIAG.EQ.'y')  THEN
          WRITE (IDUNIT,9100)  PRTFILE
9100      FORMAT (/,1X,'BUFR REPORT PRINTOUT FILE OPENED ',
     +      A100)
        ENDIF
        WRITE (*,*)
        WRITE (*,*)  'BUFR REPORT PRINTOUT FILE OPENED ',
     +    PRTFILE
C
C       SPECIFY THAT WE WOULD LIKE ROUTINE READNS TO RETURN RECDATE VALUES WITH
C         10 DIGITS (I.E. YYYYMMDDHH ), WHICH IS THE MAXIMUM BECAUSE MINUTES ARE
C         NOT AVAILABLE.
C
        CALL DATELEN (10)   ! IS THIS IN THE BUFR LIBRARY??
C
C       OPEN YOUR OWN ("EXTERNAL") BUFR TABLES FILE, AS AN ALTERNATIVE TO THE
C         TABLE IN THE DATA FILES.
C
C       OPEN (IXUNIT, FILE='BUFRTAB.EXAMPLE' )
C
        LN = 0          ! NEVER REDEFINED, UNLESS IT'S THROUGH THE EQIVALENCE
C
C ##### INITIALIZE BUFFER RECORD AND BUFR REPORT COUNTERS
C
        RECORDS = 0     ! TOTAL BUFR RECORDS FOUND (READNS CALLS)
        RECSREJ = 0     ! TOTAL BUFR RECORDS REJECTED
C
        RECREPS = 0     ! TOTAL BUFR REPORTS IN CURRENT RECORD
        REPORTS = 0     ! TOTAL BUFR REPORTS OPENED
        REPSACC = 0     ! TOTAL BUFR REPORTS ACCEPTED
        REPSREJ = 0     ! TOTAL BUFR REPORTS REJECTED
C
C ##############################################################################
C #     LOOP TO READ BUFR RECORDS FROM THE DATA FILE                           #
C ##############################################################################
C
        DORECS: DO                      ! LOOP TO READ BUFR RECORDS (MESSAGES)
C
C         READ THE NEXT BUFR MESSAGE ("RECORD") FROM THE FILE
C
c         write (*,*)  'A iiunit ',iiunit,', csubset ',csubset,
c     +      ', recdate',recdate,', istatus ',istatus 

          CALL READNS(IIUNIT,CSUBSET,RECDATE,ISTATUS)

c         write (*,*)  'B iiunit ',iiunit,', csubset ',csubset,
c     +      ', recdate',recdate,', istatus ',istatus 
C
          IF  (ISTATUS.NE. 0 )  THEN    ! END OF DATA FILE
            CALL CLOSBF (IIUNIT)
            CLOSE (IIUNIT)
            EXIT DORECS         ! GO PRINT STATISTICS FOR THIS FILE'S PROCESSING
          ENDIF
C
C          IUPBS1 is obsolete in BUFRLIB 10.2.3. Only needed here for
C          internal diagnostics, and unknown what it really provides so
C          commenting out. SEE ALSO, commented out below references to
C          'CODE' as well  <manross@ucar.edu> 20131008
C          CODE = IUPBS1(MBAY,33)
C
          RECORDS = RECORDS + 1
c
c         write (*,*)  'hello, RECORDS ',RECORDS
c
          MODREC = MOD(RECORDS,I500)
          IF (DODIAG.EQ.'y')  THEN
            IF (RECORDS.LE.50.OR.MODREC.EQ.1)  THEN
C              WRITE (IDUNIT,9120)  RECORDS, RECDATE, CSUBSET, CODE
              WRITE (IDUNIT,9120)  RECORDS, RECDATE, CSUBSET
9120          FORMAT (/,1X,111('#'),
     +          /,1X,'BUFR RECORD ',I8,' WITH RECDATE ',I10,
C     +          ' OPENED,  CSUBSET ',A8,'  CODE',I8)
     +          ' OPENED,  CSUBSET ',A8)
            ENDIF
          ENDIF
C
C         THE RETURNED RECDATE (DAY/TIME) AND CSUBSET APPLY TO
C           ALL BUFR REPORTS IN THE RECORD
C
C         GET (DECODE) A VERBOSE RECORD TYPE AND REPORT TYPE
C           FROM CSUBSET, THEN CHECK WHETHER WE WANT THIS 
C           PARTICULAR RECORD TYPE'S DATA
C
          CALL GETRO (CSUBSET,RECTYPE,OBSTYPE)
C
          IF (IRECDO.EQ.'y')  THEN
            CALL CKREC (RECORDS,RECTYPE,RECGET,NREC,
     +        IDUNIT,DODIAG,ACK)
            IF (ACK.EQ.'n')  THEN
              RECSREJ = RECSREJ + 1
              CYCLE DORECS              ! REJECT UNINTERESTING RECORD
            ENDIF                       !          (RECORD TYPE)
          ENDIF
C
C ---------
C
C         CHECK WHETHER WE WANT DATA FOR THIS DATE AND TIME
C
c         write (*,*)  'hello, check date'
c
          IF (DATEDO.EQ.'y')  THEN
            RRLEV = 'RECORD'
            CALL CKDATE (RRLEV,RECORDS,RECDATE,IBEGDAT,IENDDAT,
     +        IDUNIT,DODIAG,ACK)
            IF (ACK.EQ.'n')  THEN
              RECSREJ = RECSREJ + 1
              CYCLE DORECS              ! REJECT UNINTERESTING RECORD
            ENDIF                       !          (DATE/TIME)
          ENDIF
C
C ---------
C
C         CHECK WHETHER WE WANT THIS PARTICULAR OBSERVATION TYPE'S 
C           DATA.  NOTE: ALTHOUGH UFBINT BREAKS OUT THE REPORTS IN
C           THE INNER LOOP (NAMED DOREPS), WE ALREADY OBTAINED THE 
C           REPORT TYPE (FROM CSUBSET) WHICH APPLIES TO ALL REPORTS
C           IN THIS RECORD. - AND CAN NOT GET IT WITH UFBINT. SO WE
C           CAN FILTER HERE.
C
c         write (*,*)  'hello, check obstype'
c
          IF (IOBSDO.EQ.'y')  THEN
            CALL CKOBS (RECORDS,OBSTYPE,OBSGET,NOBS,
     +        IDUNIT,DODIAG,ACK)
            IF (ACK.EQ.'n')  THEN
              RECSREJ = RECSREJ + 1
              CYCLE DORECS              ! REJECT UNINTERESTING RECORD
            ENDIF                       !          (OBSERVATION TYPE)
          ENDIF
C
C ---------
C
          IRNO = 0               ! MAKE AN INDEX FOR CERTAIN ARRAYS AND
C                                !   ACTIONS IN THE DOREPS LOOP WHICH 
C                                !   FOLLOWS
          IF (RECTYPE.EQ.'ADPUPA')  IRNO = 1
          IF (RECTYPE.EQ.'AIRCFT')  IRNO = 2
          IF (RECTYPE.EQ.'SATWND')  IRNO = 3
          IF (RECTYPE.EQ.'AIRCAR')  IRNO = 4
C
          IF (DODIAG.EQ.'y')  THEN
            WRITE (*,*)  RECORDS
            WRITE (IDUNIT,9977)  RECORDS
9977        FORMAT (9X,'RECORD ',I9,', START MAIN UFBINT LOOP')
          ENDIF
C
C ##############################################################################
C #       LOOP TO READ BUFR REPORTS FROM THE RECORD, AND DO THE FILTERING      #
C ##############################################################################
C
          DOREPS: DO                            ! LOOP TO READ BUFR REPORTS (SUB-MESSAGES)
C
C           AT THIS POINT, WE HAVE A BUFR MESSAGE (RECORD) OPEN WITHIN THE
C             INTERNAL ARRAYS OF BUFRLIB.  NEXT GO THROUGH ITS CONTENTS, WHICH
C             WILL INCLUDE ONE OR MORE SUB-MESSAGES (REPORTS)
C
C           SUBROUTINE UFBINT EXTRACTS THE DESIRED PARAMETERS FROM A REPORT, AS
C             REQUESTED BY THE MNEMONICS GIVEN IN THE QIDENT, QBPARM AND
C             NEMLIST STRINGS.  THE "PATTERN" OF MNEMONICS MUST RESPECT SOME
C             GROUPING OR "REPLICATION" RULES.  THESE RULES GET COMPLICATED
C             BEYOND THE BASICS, SO THE OPTIONAL EXTRA (NEMLIST) PARAMETERS
C             ARE EXTRACTED ONE AT A TIME.  THAT GETS SLOW.
C
C               CALL UFBINT (IIUNIT,R8IDENT,MXMN,MXREPL,NREPL,QIDENT)
C               CALL UFBINT (IIUNIT,R8BPARM,MXMN,MXREPL,NREPL,QBPARM)
C               CALL UFBINT (IIUNIT,R8XPARM,MXMN,MXREPL,NREPL,NEMLIST)
C
C           THE DESIRED PARAMETERS ARE RETURNED IN THE R8IDENT, R8BPARM AND
C             R8XPARM ARRAYS.  THESE WILL BE MOVED TO VARIABLES WHOSE NAMES HAVE
C             THE FORM X8NEMO, WHERE X CAN BE I (INTEGER), R (REAL), OR A
C             (CHARACTER).  THE 'NEMO' IS THE MNEMONIC (TRUNCATED TO FOUR
C             CHARACTERS WHEN NECESSARY) THAT WAS USED IN THE QIDENT, QBPARM,
C             OR NEMLIST STRINGS TO TELL UFBINT WHAT VARIABLES TO GET.
C
C           UFBINT RETURNS NREPL REPLICATIONS (PERHAPS LEVELS) OF A MAXIMUM OF MXREPL
C           UFBINT RETURNS UP TO MXMN PARAMETERS (AT EACH NREPL), CORRESPONDING TO
C             THE MAXIMUM NUMBER OF MNEMONICS IN QIDENT
C
C           THE MAXIMUM NUMBER OF PARAMETERS RETURNED BY UFBINT FOR A MNEMONIC
C             REQUEST STRING OF UP TO 80 CHARACTERS IS  80 / 5 = 16 (MXMN)
C             THERE CAN BE UP TO MXREPL REPLICATIONS
C
C ----------
C           (RE)INITIALIZE PARAMETER ARRAYS FOR THIS NEXT REPORT
C
c         write (*,*)  'hello, (re)initialize parameter arrays'
c
            DO  MZ = 1, MXMN
                R8IDENT(MZ,1)  = FILL9
              DO  LZ = 1, MXREPL
                R8BPARM(MZ,LZ) = FILL9
                R8XPARM(MZ,LZ) = FILL9
              ENDDO
            ENDDO
C
C ++++++++++
C           THE FOLLOWING UFBINT CALL GETS A REPORT'S IDENTIFICATION.  THIS
C             INCLUDES STATION NUMBER OR CALL SIGN, LOCATION, ELEVATION, DATE
C             AND TIME.
C
C           IN THE BUR TABLES, THE UNITS ARE SOMETIMES GIVEN AS "CCITT IA5 (T, G)"
C             THIS IS "A character coding standard (ComitE Consultatif International
C             Telegraphique et Telephonique, International Alphabet No. 5), functionally
C             equivalent to ASCII. All character data within BUFR and CREX messages
C             are coded according to this standard."  WHEN THIS IS USED, EFFECTED
C             REAL*8 VARIABLES RETURNED FROM A UFBINT CALL SHOULD BE WRITTEN TO A
C             CHARACTER VARIABLE WITH AN 'A' FORMAT DESCRIPTOR.  EXAMPLES ARE THE
C             REPORT ID AND AIRCRAFT ID.
C
C           MXREPL HAS BEEN SET TO ONE BECAUSE THERE SHOULD NOT BE ANY
C             REPLICATION OF ID PARAMETERS.
C ++++++++++
C
            CALL UFBINT (IIUNIT,R8IDENT,MXMN,MXREPL,NREPL,QIDENT)
C
            REPORTS = REPORTS + 1
            RECREPS = RECREPS + 1
            IDUMP = MOD(REPORTS,I500)
C
            Z = 1
C
C           TOSS, AS SOON AS POSSIBLE, REPORTS WHICH ARE MISSING ABSOLUTELY
C             ESSENTIAL INFORMATION.  THIS AVOIDS UNNECESSARY PROCESSING
C             OF SUBSEQUENT INFORMATION IN A REPORT
C
            DO  ITOSS = 4, 10
              IF (ITOSS.EQ.6)  CYCLE
C
C             BAD LATITUDE, LONGITUDE, YEAR, MONTH, DAY, HOUR
C                 4         5          7     8      9    10
C
              IF (R8IDENT(ITOSS,Z).GT.R8BIG)  GO TO 290 ! MISSING VALUE
            ENDDO
            R8CLAT = R8IDENT(4,Z)
            R8CLON = R8IDENT(5,Z)

            IF (LLDO.EQ.'y')  THEN
              CALL CKLL (RECORDS,RECREPS,R8CLAT,R8CLON,LATS,LATN,
     +                   LONW,LONE,LLWRAP,IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
            ENDIF
            IF (LLRDO.EQ.'y')  THEN
              CALL CKRAD (RECORDS,RECREPS,R8CLAT,R8CLON,RADR,LATR,LONR,
     +                   NIR,IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
            ENDIF
C
c           write (*,*)  'hello bone-head'
C           IF (R8IDENT(1,Z).GT.R8BIG)  R8IDENT(1,Z) = 99.
C           I8WMOB = R8IDENT(1,Z)               ! DO NOT NEED THIS
C
C           IF (R8IDENT(2,Z).GT.R8BIG)  R8IDENT(2,Z) = 999.
C           I8WMOS = R8IDENT(2,Z)               ! DO NOT NEED THIS
C
            WRITE (A8RPID,9125)  R8IDENT(3,Z)   ! R8IDENT(3,Z) IS CHARACTER
9125        FORMAT (A8)                         ! AND THIS ACTION YIELDS A 
C                                               ! LEFT-JUSTIFIED NUMBER OR STRING
C
c           write (*,*)  'hello again bone-head'
c           write (*,*)  'a8rpid .',a8rpid,'.'
C
            IF (OBSTYPE.EQ.'RAOBF   '.OR.OBSTYPE.EQ.'PIBAL   ')  THEN    
c                                               ! OTHER UPPER AIR RECORD TYPES DO
C                                               ! NOT HAVE WMO NUMBER IDENTIFIERS
              IF (DODIAG.EQ.'y')  THEN
                WRITE (*,*) 'CALL CKWMO ',WMODO,' OR CKWBB ',WBBDO
              ENDIF
              IF (WMODO.EQ.'y')  THEN
                CALL CKWMO (RECORDS,RECREPS,A8RPID,WMOLIST,NWMO,
     +                    IDUNIT,DODIAG,ACK)
                IF (ACK.EQ.'n')  THEN
                  GO TO 290                     ! REJECT UNINTERESTING REPORT
                ENDIF
              ENDIF
              IF (WBBDO.EQ.'y')  THEN
                CALL CKWBB (RECORDS,RECREPS,A8RPID,WBBLIST,NWBB,
     +                    IDUNIT,DODIAG,ACK)
                IF (ACK.EQ.'n')  GO TO 290      ! REJECT UNINTERESTING REPORT
              ENDIF
            ENDIF
C
            IF (R8IDENT(6,Z).GT.R8BIG)  R8IDENT(6,Z)   = 99999.
            I8SELV = R8IDENT(6,Z)
C
            IF (IELEVDO.EQ.'y')  THEN
              CALL CKELEV (RECORDS,RECREPS,I8SELV,IELEVL,IELEVH,
     +          IDUNIT,DODIAG,ACK)
              IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
            ENDIF
C
            I8YEAR = R8IDENT(7,Z)
            I8MNTH = R8IDENT(8,Z)
            I8DAYS = R8IDENT(9,Z)
            I8HOUR = R8IDENT(10,Z)
            I8DATE = I8YEAR*1000000 + I8MNTH*10000 + I8DAYS*100 + I8HOUR
C
C           IF (DATEDO.EQ.'y')  THEN
C             RRLEV = 'REPORT'
C             CALL CKDATE (RRLEV,REPORTS,I8DATE,IBEGDAT,IENDDAT,
C    +          IDUNIT,DODIAG,ACK)
C             IF (ACK.EQ.'n')  GO TO 290        ! REJECT UNINTERESTING REPORT
C           ENDIF
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +         DONE FILTERING                                                     +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
            REPSACC = REPSACC + 1
C
            IF (R8IDENT(11,Z).GT.R8BIG)  R8IDENT(11,Z)   = 99.
            I8MINU = R8IDENT(11,Z)
C
C       2010.07.01 - LET'S NOT RETRIEVE OR SHOW THE CORRECTION FLAG
C
C           IF (R8IDENT(12,Z).GT.R8BIG)  R8IDENT(12,Z)   = 99.
C           I8CORN = R8IDENT(12,Z)
C
C ++++++++++
C           THE FOLLOWING UFBINT CALL GETS A REPORT'S BASIC METEOROLOGICAL
C             PARAMETERS - WHEN A USER HAS NOT TURNED THIS DEFAULT OFF.
C
C           MXREPL IS THE MAXIMUM (ESTIMATED) NUMBER OF REPLICATIONS WHICH
C             MIGHT BE RETURNED, WHILE NREPL IS THE NUMBER RETURNED.
C ++++++++++
C
            ISBPARM = 'n'
C           IF (DEFAULT.EQ.'y')  THEN   ! TURNED OFF 20101129, PER DOUG'S SUGGESTION
              CALL UFBINT (IIUNIT,R8BPARM,MXMN,MXREPL,NREPL,QBPARM)
              IF (NREPL.GT.0.AND.NREPL.LE.MXREPL)  THEN
                DO NL = 1, NREPL
                  IF (R8BPARM(1,NL).GT.R8BIG)   R8BPARM(1,NL) = 999990.0
                  R8PRLC(NL) = R8BPARM(1,NL)
C 
                  IF (R8BPARM(2,NL).GT.R8BIG)   R8BPARM(2,NL) = 999990.0
                  R8PSAL(NL) = R8BPARM(2,NL)
C 
                  IF (R8BPARM(3,NL).GT.R8BIG)   R8BPARM(3,NL)  = FILL9
                  R8GP10(NL) = R8BPARM(3,NL)
C
                  IF (R8BPARM(4,NL).GT.R8BIG)   R8BPARM(4,NL)  = FILL9
                  R8GP07(NL) = R8BPARM(4,NL)
C
                  IF (R8BPARM(5,NL).GT.R8BIG)   R8BPARM(5,NL)  = FILL9
                  R8FLVL(NL) = R8BPARM(5,NL)
C
                  IF (R8BPARM(6,NL).GT.R8BIG)   R8BPARM(6,NL)  = FILL9
                  R8VSIG(NL) = R8BPARM(6,NL)
C
                  IF (R8BPARM(7,NL).GT.R8BIG)   R8BPARM(7,NL)  = FILL9
                  R8WDIR(NL) = R8BPARM(7,NL)
C
                  IF (R8BPARM(8,NL).GT.R8BIG)   R8BPARM(8,NL)  = FILL9
                  R8WSPD(NL) = R8BPARM(8,NL)
C
                  IF (R8BPARM(9,NL).GT.R8BIG)   R8BPARM(9,NL)  = FILL9
                  R8TMDB(NL) = R8BPARM(9,NL)
C  
                  IF (R8BPARM(10,NL).GT.R8BIG)  R8BPARM(10,NL) = FILL9
                  R8TMDP(NL) = R8BPARM(10,NL)
C  
                  IF (R8BPARM(11,NL).GT.R8BIG)  R8BPARM(11,NL) = FILL9
                  R8REHU(NL) = R8BPARM(11,NL)
C
C                 IF (R8BPARM(12,NL).GT.R8BIG)  R8BPARM(12,NL) = FILL9
C                 R8   (NL)  = R8BPARM(12,NL)
C             
C ++++++++++  
C           
C                 DO A FEW CONVERSIONS, MAINLY UNITS; AND MAPPINGS
C
                  R8PRLC(NL) = R8PRLC(NL) / 100.
                  R8PSAL(NL) = R8PSAL(NL) / 100.
C
C                 NOTE: THE VSIG CODE IS NOT USED CONSISTENTLY, NOTABLY
C                   FOR THE WIND.  THE FOLLOWING TRIES TO MAKE IT BETTER
C
                  I8VSIG = R8VSIG(NL)
                  WRITE (A8VSIG(NL),'(I4)')  I8VSIG
                  IF (RECTYPE.EQ.'AIRCFT')  THEN
                    A8VSIG(NL) = 'FLVL'
                  ENDIF
                  IF (RECTYPE.EQ.'SATWND')  THEN
                    A8VSIG(NL) = 'WXPR'
                  ENDIF
                  DO IV = 2, 7 
                    IF (R8VSIG(NL).NE.RVSIG(IV))  CYCLE
                    A8VSIG(NL) = VSIGCODE(IV)      
                    IF (RECTYPE.EQ.'ADPUPA')  THEN
                      IF (R8GP07(NL).NE.FILL9)  THEN
                        A8VSIG(NL) = 'WXHT'
                      ENDIF
                    ENDIF
                    EXIT
                  ENDDO
C
                  IF (KELCEL.EQ.'c')  THEN
C
C                   WE CAN CONVERT TEMPERATURES IN THE BASIC PARAMETERS, 
C                     BUT WILL MISS THOSE IN THE EXTRA PARAMETERS
C
                    IF (R8TMDB(NL).NE.FILL9)  THEN
                      R8TMDB(NL) = R8TMDB(NL) - 273.16
                    ENDIF
                    IF (R8TMDP(NL).NE.FILL9)  THEN
                      R8TMDP(NL) = R8TMDP(NL) - 273.16
                    ENDIF
C                   IF (R8    (NL).NE.FILL9)  THEN
C                     R8    (NL) = R8    (NL) - 273.16
C                   ENDIF
                    ISBPARM = 'y'
                  ENDIF
                ENDDO
              ENDIF
C           ENDIF                       ! TURNED OFF 20101129, PER DOUG'S SUGGESTION 
C             
C ++++++++++  
C           THE FOLLOWING LOOP ON UFBINT CALLS GETS A SELECTION OF EXTRA
C             METEOROLOGICAL PARAMETERS, WHEN SPECIFIED BY THE USER (WHEN
C             NML .NE. 0)
C           WHEN DEFAULT = 'n' THIS IS THE ONLY WAY TO GET ANY PARAMETERS.
C           MXREPL IS THE MAXIMUM (ESTIMATED) NUMBER OF REPLICATIONS WHICH
C             MIGHT BE RETURNED, WHILE NREPL IS THE NUMBER RETURNED.
C ++++++++++  
C
            IF (DODIAG.EQ.'y')  THEN
              WRITE (*,*)  '========================================',
     +                     '========================================'
              WRITE (*,*)  'REPSACC ',REPSACC, ', RECTYPE ',RECTYPE,
     +          ', OBSTYPE ',OBSTYPE, ', A8RPID ',A8RPID
            ENDIF
            ISXPARM = 'n'
            IF (NML.GT.0)  THEN
              DO KICK = 1, 100
                DO MICK = 1, MRREPL
                  EXTRA(KICK,MICK) = FILL99
                ENDDO
              ENDDO
              DO MM = 1, NML
C               EXTRA(MM,NL) = R8XPARM(1,NL)  ! MOVED BELOW, 20101129, PER DOUG'S SUGGESTION 
C               
C               NEMLIST(MM) HAS THE MNEMONICS - NML OF THEM
C                 
                CALL UFBINT (IIUNIT,R8XPARM,MXMN,MXREPL,NXREPL,
     +            NEMLIST(MM))
                IF (DODIAG.EQ.'y')  THEN
                  WRITE (*,*)  'NML ',NML, ', MM ',MM,
     +            ', NXREPL ',NXREPL
                ENDIF
                IF (NXREPL.GT.0.AND.NXREPL.LE.MXREPL)  THEN
                  DO NL = 1, NXREPL
                    HAVACID = 'n'
                    IF (USEACID.EQ.'y')  THEN
                      IF (NEMLIST(MM)(1:4).EQ.'ACID')  THEN
                        HAVACID = 'y'   ! PARAMETER ACID AVAILABLE 
C                                       !   TO REPLACE MISSING A8RPID
                      ENDIF
                    ENDIF
                    WRITE (CHSTR(1:8),9265)  R8XPARM(1,NL)
9265                FORMAT (A8)
                    CALL VALIDCH (CHSTR,8,ACK)   ! IS PARAMETER CHARACTER?
                    IF (ACK.EQ.'n')  THEN        ! PARAMETER IS NUMERIC
                      IF (R8XPARM(1,NL).GT.R8BIG) R8XPARM(1,NL) = FILL99
                    ELSE                         ! PARAMETER IS CHARACTER
                      IF (USEACID.EQ.'y'.AND.HAVACID.EQ.'y')  THEN
                        IF (OBSTYPE.EQ.'ACARS   ')  THEN
                          A8RPID(1:8) = CHSTR(1:8)
                        ENDIF
                      ENDIF
                    ENDIF
C           write (*,*)  'hello again bone-head [0] :: ', R8BPARM(MM,NL)
C           write (*,*)  'R8XPARM (',NL,') :: ',R8XPARM(1,NL),'.'
                    EXTRA(MM,NL) = R8XPARM(1,NL)    ! MOVED FROM ABOVE, 20101129, PER DOUG'S SUGGESTION
                    IF (DODIAG.EQ.'y')  THEN
                      WRITE (*,*)  'NREPL ',NREPL, ', MM ',MM,
     +                 ', NL ',NL, ', R8XPARM(1,NL) ',R8XPARM(1,NL),
     +                 ', EXTRA(MM,NL) ',EXTRA(MM,NL)
                    ENDIF
                  ENDDO
                  ISXPARM = 'y'
                ENDIF
              ENDDO
            ENDIF
C           
            IRECDAT = RECDATE
            WRITE (MINUTE,FMT='(I2)') I8MINU
            DO K = 1, 2
              IF (MINUTE(K:K).EQ.' ') THEN
                MINUTE(K:K) = '0'
              ENDIF
            ENDDO
C           
            IDOHDR = 'n'
C            IF (RECTYPE.EQ.'ADPUPA')   IDOHDR = 'y'
            IF (REPSACC.EQ.1)          IDOHDR = 'y'
C            IF (MOD(REPSACC,I30).EQ.1)  IDOHDR = 'y'
C           
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +         PRINT THIS REPORT                                                  +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C           
            IDOH = 'y'
            DO NL = 1, NREPL
              IF (R8PRLC(NL).NE.9999.9)  THEN
                IF (RECTYPE.NE.'AIRCFT')  THEN
                  IF (PLEVDO.EQ.'y')  THEN
                    IF (R8PRLC(NL).GT.PLEVL)  CYCLE
                    IF (R8PRLC(NL).LT.PLEVH)  CYCLE
                  ENDIF
                ENDIF
              ENDIF
              IF (IDOH.EQ.'y'.AND.IDOHDR.EQ.'y')  THEN
                WRITE (IPUNIT,9270)  DUMPHED(1)(001:IHDEND),
     +                               DUMPHED(2)(001:IHDEND)
9270            FORMAT (/a/a)
                IDOHDR = 'n'
                IDOH = 'n'
              ENDIF
              IF (DEFAULT.EQ.'y')  THEN
                WRITE (IPUNIT,9280)
     &          RECTYPE,          OBSTYPE,
     &          I8DATE, MINUTE, A8RPID, R8CLAT, R8CLON, I8SELV, 
     &          NL, A8VSIG(NL),
     &          R8PRLC(NL), R8PSAL(NL), 
     &          R8GP10(NL), R8GP07(NL), R8FLVL(NL),
     &          R8TMDB(NL), R8TMDP(NL), R8REHU(NL), 
     &          R8WDIR(NL), R8WSPD(NL),
     &          (EXTRA(MM,NL),MM=1,NML)
9280            FORMAT (
     +          1X,A6,1X,    2X,A8,
     +          2X,I10,A2,2X,A8,2X,F7.2,2X,F7.2,2X,I5,
     +          2X,I4,2X,A4,
     +          2X,F6.1,2X,F6.1,
     +          2X,F8.1,2X,F8.1,2X,F8.1,
     +          2X,F5.1,2X,F5.1,2X,F5.1,
     +          2X,F6.1,2X,F6.1,4X,'|',
     +          2X,100F10.2)          ! WAS 12.2
              ENDIF
C           
              IF (DEFAULT.EQ.'n')  THEN
                WRITE (IPUNIT,9285)
C    &          RECTYPE, IRECDAT, OBSTYPE,
     &          RECTYPE,          OBSTYPE,
     &          I8DATE, MINUTE, A8RPID, R8CLAT, R8CLON, I8SELV,
     &          NL, A8VSIG(NL),
     &          (EXTRA(MM,NL),MM=1,NML)
9285            FORMAT (
C    +          1X,A6,2X,I10,3X,A8,
     +          1X,A6,1X,    2X,A8,
     +          2X,I10,A2,2X,A8,2X,F7.2,2X,F7.2,2X,I5,
     +          2X,I4,2X,A4,
     +          2X,100F10.2)          ! WAS 12.2
              ENDIF
            ENDDO
C           
            GO TO 300
 290        CONTINUE
C            
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +         WE HAVE SKIPPED DOWN TO HERE TO REJECT A REPORT                    +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C           
            REPSREJ = REPSREJ + 1
C           
 300        CONTINUE
C           
C           SEE IF THERE IS ANOTHER SUB-MESSAGE ("REPORT") IN THIS MESSAGE ("RECORD")
C             NOTE THAT IN THE BUFR LIBRARY COMMENTS, NCEP USES THE TERM "SUBSET"
C             INSTEAD OF SUB-MESSAGE - CLEAR AS MUD, EH?
C           
            CALL READSB  ( IIUNIT, IERRSB )
C           
            IF  (IERRSB.NE.0)  THEN
              EXIT DOREPS
            ENDIF
          ENDDO DOREPS          ! END OF READING A BUFR REPORT (SUB-MESSAGE)
          RECREPS = 0
C         
          DO KICK = 1, 100
            DO MICK = 1, MRREPL
              EXTRA(KICK,MICK) = FILL99
            ENDDO
          ENDDO
        ENDDO DORECS            ! END OF READING A BUFR RECORD (MESSAGE)
C       
        IF (DODIAG.EQ.'y')  THEN
          WRITE(IDUNIT,9800)
9800      FORMAT (//,' ************* INPUT FILE DONE ')
        ENDIF
        RECSACC = RECORDS - RECSREJ
C       
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +     PRINT THE CONFIGURATION AT THE END OF THE PRINTOUT ("DUMP") FILE       +
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       
C        WRITE (IPUNIT,9880)  CONFILE
9880    FORMAT (/,1X,'CONFIGURATION FILE ',A32,
     +          /,3X,'CHOICES FOLLOW')
C        WRITE (IPUNIT,9070)
C     +      IRECDO, NREC, RECGET(1),  RECGET(2),  RECGET(3),  RECGET(4),
C     +      IOBSDO, NOBS, OBSGET(1),  OBSGET(2),  OBSGET(3),  OBSGET(4),
C     +      DATEDO, IBEGDAT, IENDDAT,
C     +      NMDO,  NML, (NEMLIST(NM),NM=1,20),
C     +      LLDO, LATS, LATN, LONW, LONE,
C     +      LLRDO, RADR,
C     +      NIR, LATR(1), LONR(1), LATR(2), LONR(2), LATR(3), LONR(3),
C     +      IELEVDO, IELEVL, IELEVH,
C     +       PLEVDO,  PLEVL,  PLEVH,
C     +      WMODO, NWMO, (WMOLIST(NZ),NZ=1,20),
C     +      WBBDO, NWBB, (WBBLIST(NZ),NZ=1,10)
C
        IF (DODIAG.EQ.'y')  THEN
          WRITE (IDUNIT,9890)  INFILE, RECORDS, RECSREJ, RECSACC,
     +       REPORTS, REPSACC, REPSREJ
9890      FORMAT (/,1X,'BUFR EXTRACTION STATISTICS',
     +              1X,'FOR FILE INFILE  ',A128,/,
     +              5X,'RECORDS    FOUND ',I12,/,
     +              5X,'RECORDS REJECTED ',I12,/,
     +              5X,'RECORDS ACCEPTED ',I12,//,
     +              5X,'REPORTS   OPENED ',I12,
     +                    ' (FROM ACCEPTED RECORDS)',/
     +             ,5X,'REPORTS ACCEPTED ',I12,/,
     +              5X,'REPORTS REJECTED ',I12)
C
        ENDIF
C        WRITE (IPUNIT,9890)  INFILE, RECORDS, RECSREJ, RECSACC,
C     +     REPORTS, REPSACC, REPSREJ
        CLOSE   ( IPUNIT )
C
C ##############################################################################
C
        ENDDO DOFILS            ! END OF READING A BUFR DATA FILE
C
C ##############################################################################
C
        WRITE (*,*)
        WRITE (*,*)  'ALL ',INK,' BUFR DATA FILES PROCESSED'
        WRITE (*,*)
C
        IF (DODIAG.EQ.'y')  THEN        
          WRITE(IDUNIT,9900)  INK
9900      FORMAT (//,' **** ALL ',I5,' BUFR DATA FILES PROCESSED *****',
     +            //,' ************ DONE ************')
                ENDIF
C        STOP 99999
        END
C
C       ########################################################################
C
        SUBROUTINE GETRO (CSUBSET,RECTYPE,OBSTYPE)
C
C       GET AND MAP RECORD TYPE AND OBSERVATION (REPORT) TYPE
C
C       CSUBSET--   see  http://www.emc.ncep.noaa.gov/mmb/data_processing/data_dumping.doc/table_1.htm
C
        CHARACTER*8  CSUBSET    ! HAS THE CODED RECORD TYPE AND OBSERVATION TYPE
        CHARACTER*6  RECTYPE    ! UNCODED RECORD TYPE
        CHARACTER*8  OBSTYPE    ! UNCODED OBSERVATION TYPE
C
        CHARACTER*6    AUPAF(10)
C       DATA AUPAF(1)  /'ADPSFC'/    ! SURFACE LAND
C       DATA AUPAF(2)  /'SFCSHP'/    ! SURFACE SEA
        DATA AUPAF(3)  /'ADPUPA'/    ! VERTICAL SOUNDINGS (NOT SATELLITE)
        DATA AUPAF(4)  /'SATSDG'/    ! VERTICAL SOUNDINGS (SATELLITE) (DSS NAME NOT NCEP'S)
        DATA AUPAF(5)  /'AIRCFT'/    ! SINGLE LEVEL UPPER AIR (NOT SATELLITE)
        DATA AUPAF(6)  /'SATWND'/    ! SINGLE LEVEL UPPER AIR (SATELLITE)
        DATA AUPAF(7)  /'      '/    !
        DATA AUPAF(8)  /'      '/    !
        DATA AUPAF(9)  /'      '/    !
        DATA AUPAF(10) /'      '/    !
C
        CHARACTER*8 OBSTYP(10,200)
        DATA OBSTYP /2000*'        '/
C
        SAVE
C
        OBSTYP(3,001) = 'RAOBF   '     !  RAWINSONDE FIXED LAND
        OBSTYP(3,002) = 'RAOBM   '     !  RAWINSONDE MOBILE LAND
        OBSTYP(3,003) = 'RAOBS   '     !  RAWINSONDE SHIP
        OBSTYP(3,004) = 'DROPW   '     !  DROPWINSONDE
        OBSTYP(3,005) = 'PIBAL   '     !  PIBAL
        OBSTYP(3,006) = '        ' 
        OBSTYP(3,007) = 'PRFLR   '     !  PROFILER WINDS (NPN)
        OBSTYP(3,008) = 'NXRDW   '     !  NEXRAD WINDS (VAD DECODED)
        OBSTYP(3,009) = 'PRFLRP  '     !  PROFILER WINDS PILOT (PIBAL) BULLETINS
        OBSTYP(3,010) = 'PRFLRM  '     !  PROFILER WINDS (NPN AND MAP)
        OBSTYP(3,011) = 'PRFLRB  '     !  PROFILER WINDS (MAP)
        OBSTYP(3,012) = 'RASS    '     !  RASS TEMPERATURES (NPN AND MAP)
        OBSTYP(3,013) = 'PRFLRJ  '     !  PROFILER WINDS (JMA)
        OBSTYP(3,014) = 'PRFLRH  '     !  PROFILER WINDS (HONG KONG)
        OBSTYP(3,015) = '        '     !
        OBSTYP(3,016) = 'PRFLRE  '     !  PROFILER WINDS (EUROPE)
        OBSTYP(3,017) = 'NXRDW2  '     !  NEXRAD WINDS (VAD DECODED LEVEL II)
        OBSTYP(3,018) = '        ' 
        OBSTYP(3,019) = '        ' 
        OBSTYP(3,020) = '        ' 
C
        OBSTYP(4,002) = 'GEOSTH  '     !  GOES/NESDIS 1X1 FOV CLOUD
        OBSTYP(4,003) = 'GEOST1  '     !  GOES/NESDIS 1X1 FOV BRIGHTNESS TEMPERATURES
        OBSTYP(4,010) = 'GPSRO   '     !  RADIO OCCULTATION
        OBSTYP(4,104) = 'ATOVS   '     !  POES/NESDIS ATOVS BRIGHTNESS TEMPERATURES
C
        OBSTYP(5,001) = 'AIREP   '     !  AIRCRAFT AIREP (MANUAL)
        OBSTYP(5,002) = 'PIREP   '     !  AIRCRAFT PIREP (MANUAL)
        OBSTYP(5,003) = 'AMDAR   '     !  AIRCRAFT ASDAR/ACARS (AUTOMATED)       RESTRICTED
        OBSTYP(5,004) = 'ACARS   '     !  AIRCRAFT ARINC MCDRS/ACARS (AUTOMATED) RESTRICTED
        OBSTYP(5,005) = 'RECCO   '     !  AIRCRAFT RECONNAISSANCE
        OBSTYP(5,011) = 'KAMDAR  '     !  AIRCRAFT KOREAN ASDAR/ACARS
        OBSTYP(5,014) = 'FAMDAR  '     !  AIRCRAFT FRENCH ASDAR/ACARS
C
C                                      ! FOLLOWING SEEM TO BE SATELLITE WINDS
        OBSTYP(6,010) = 'INFUS   '     !  GOES/NESDIS INFRARED DERIVED CLOUD MOTION
        OBSTYP(6,011) = 'H2IUS   '     !  GOES/NESDIS WATER VAPOR IMAGER DERIVED CLOUD MOTION
C                                      !    AKA SATELLITE WINDS
        OBSTYP(6,012) = 'VISUS   '     !  GOES/NESDIS VISIBLE DERIVED CLOUD MOTION 
        OBSTYP(6,014) = 'H2SUS   '     !  GOES/NESDIS WATER VAPOR SOUNDER DERIVED CLOUD MOTION
        OBSTYP(6,044) = 'INFJA   '     !  GMS/MTSAT/JMA INFRARED DERIVED CLOUD MOTION
        OBSTYP(6,045) = 'VISJA   '     !  GMS/MTSAT/JMA VISIBLE DERIVED CLOUD MOTION
        OBSTYP(6,046) = 'H20JA   '     !  GMS/MTSAT/JMA WATER VAPOR IMAGER DERIVED CLOUD MOTION
        OBSTYP(6,064) = 'INFEU   '     !  METEOSAT/EUMETSAT INFRARED DERIVED CLOUD MOTION
        OBSTYP(6,065) = 'VISEU   '     !  METEOSAT/EUMETSAT VISIBLE DERIVED CLOUD MOTION
        OBSTYP(6,066) = 'H20EU   '     !  METEOSAT/EUMETSAT WATER VAPOR IMAGER DERIVED CLOUD MOTION
        OBSTYP(6,070) = 'INFMO   '     !  AQUA/TERRA MODIS INFRARED DERIVED CLOUD MOTION
        OBSTYP(6,071) = 'H20MO   '     !  AQUA/TERRA MODIS WATER VAPOR IMAGER DERIVED CLOUD MOTION
        OBSTYP(6,080) = 'INFAV   '     !  NOAA/METOP AVHRR INFRARED DERIVED CLOUD MOTION
C       OBSTYP(6,0  ) = '        '     !  
C
        READ (CSUBSET(3:5),'(I3)')  J
        J = J + 1                      ! BUFR RECORD TYPE POINTER
        RECTYPE = AUPAF(J)
C
        READ (CSUBSET(6:8),'(I3)')  K
C
        IF (OBSTYP(J,K).NE.'        ')  THEN
          OBSTYPE = OBSTYP(J,K)        ! GRAB IT
        ELSE
          OBSTYPE(1:8) = CSUBSET(1:8)  ! DEFAULT IS THE CODE (WHEN UNRECOGNIZED)
        ENDIF
C
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKREC (RECORDS,RECTYP,RECGET,NREC,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK RECORD TYPE, E.G. ADPSFC, SFCSHP ...
C
        INTEGER RECORDS
        CHARACTER*6 RECTYP, RECGET(20)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO NR = 1, NREC
          IF (RECTYP.EQ.RECGET(NR))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'y')  THEN
            WRITE (IDUNIT,7706) RECORDS, RECTYP
7706        FORMAT (1X,'CKREC:  RECORD ',I9,', ACCEPTED, RECTYP ',
     +       A6)
          ELSE
            WRITE (IDUNIT,7705) RECORDS, RECTYP
7705        FORMAT (1X,'CKREC:  RECORD ',I9,', REJECTED, RECTYP ',
     +       A6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKOBS (RECORDS,OBSTYP,OBSGET,NOBS,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK OBSERVATION (REPORT) TYPE, E.G. SYNOP, METAR ...
C
        INTEGER RECORDS
        CHARACTER*8 OBSTYP, OBSGET(50)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO NO = 1, NOBS
          IF (OBSTYP.EQ.OBSGET(NO))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'y')  THEN
            WRITE (IDUNIT,7706) RECORDS, OBSTYP
7706        FORMAT (1X,'CKOBS:  RECORD ',I9,
     +       ', ACCEPTED, OBSTYP ',A6)
          ELSE
            WRITE (IDUNIT,7705) RECORDS, OBSTYP
7705        FORMAT (1X,'CKOBS:  RECORD ',I9,
     +       ', REJECTED, OBSTYP ',A6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKDATE (RRLEV,RR,IDATE,IBEG,IEND,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK RECORD OR REPORT DATE
C
        CHARACTER*6 RRLEV       ! WILL CONTAIN EITHER 'RECORD' OR 'REPORT'
        INTEGER RR, IDATE, IBEG, IEND
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'y'
        IF (IDATE.LT.IBEG.OR.IDATE.GT.IEND)  ACK = 'n'
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7730)  RRLEV, RR, IDATE, IBEG, IEND
7730        FORMAT (/1X,'CKDATE: ',A6,1X,I9,', REJECTED, DATE ',I10,
     +       ' BEGIN ',I10,', END ',I10)
          ELSE
            WRITE (IDUNIT,7731)  RRLEV, RR, IDATE, IBEG, IEND
7731        FORMAT (/1X,'CKDATE: ',A6,1X,I9,', ACCEPTED, DATE ',I10,
     +       ' BEGIN ',I10,', END ',I10)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKLL(RECORDS,RR,LAT,LON,LATS,LATN,LONW,LONE,LLWRAP,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK GEOGRAPHIC WINDOW (LATITUDE - LONGITUDE)
C
        INTEGER RECORDS, RR
        REAL*8 LAT, LON
        CHARACTER*1 LLWRAP
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'y'
        IF (LAT.LT.LATS.OR.LAT.GT.LATN)  THEN
          IF (DODIAG.EQ.'y')  THEN
            WRITE (IDUNIT,7750)  RECORDS, RR, LAT, LON,
     &      LATS, LATN, LONW, LONE
7750        FORMAT ( 1X,'CKLL:   RECORD ',I9,', REPORT ',I9,
     +      ', REJECTED,  ',F7.1,F8.1,',  LATS',I6,' LATN',I6,
     +      ' LONW',I6,' LONE',I6)
          ENDIF
          GO TO 90   ! TO REJECT REPORT
        ENDIF
        IF (LLWRAP.EQ.'n')  THEN
          IF (LON.LT.LONW.OR.LON.GT.LONE)  THEN
            IF (DODIAG.EQ.'y')  THEN
              WRITE (IDUNIT,7750)  RECORDS, RR, LAT, LON,
     &        LATS, LATN, LONW, LONE
            ENDIF
            GO TO 90   ! TO REJECT REPORT
          ENDIF
        ENDIF
        IF (LLWRAP.EQ.'y')  THEN
          IF (LON.GE.0.0)  THEN
            IF (LON.LT.LONW)  THEN
              IF (DODIAG.EQ.'y')  THEN
                WRITE (IDUNIT,7750)  RECORDS, LAT, LON,
     &          LATS, LATN, LONW, LONE
              ENDIF
              GO TO 90   ! TO REJECT REPORT
            ENDIF
          ELSE
            IF (LON.GT.LONE)  THEN
              IF (DODIAG.EQ.'y')  THEN
                WRITE (IDUNIT,7750)  RECORDS, LAT, LON,
     &          LATS, LATN, LONW, LONE
              ENDIF
              GO TO 90   ! TO REJECT REPORT
            ENDIF
          ENDIF
        ENDIF
        IF (DODIAG.EQ.'y')  THEN
          WRITE (IDUNIT,7751)  RECORDS, LAT, LON,
     &    LATS, LATN, LONW, LONE
7751      FORMAT ( 1X,'CKLL:   RECORD ',I9,', REPORT ',I9,
     +    ', ACCEPTED,  ',F7.1,F8.1,',  LATS',I6,' LATN',I6,
     +    ' LONW',I6,' LONE',I6)

        ENDIF
        RETURN
  90    CONTINUE
        ACK = 'n'
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKRAD (RECORDS,RR,XLAT,XLON,RADR,YLAT,YLON,NIR,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK GEOGRAPHIC CIRCLE (LATITUDE, LONGITUDE, RADIUS)
C
        INTEGER RECORDS, RR
        INTEGER RADR
        REAL*8 XLAT, XLON
        REAL YLAT(100), YLON(100)
        DATA R, DRAD / 6371.2277, 0.0174533/    ! EARTH'S RADIUS IN KM
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO  K = 1, NIR
C         DLON = ABS(XLON - YLON(K))  ! ORIGINAL (ON29), FOR 0W TO 359W
          XL = XLON + 180.0           ! BUFR IS -180 TO +180 (DO LIKE LATITUDE)
          YL = YLON(K) + 180.0        ! BUFR IS -180 TO +180 (DO LIKE LATITUDE)
          DLON = ABS(XL - YL)         ! BUFR IS -180 TO +180 (DO LIKE LATITUDE)
          IF (DLON.GT.180.)  DLON = 360. - DLON
          DL = DLON * DRAD
C
          XL = (XLAT + 90.) * DRAD
          YL = (YLAT(K) + 90.) * DRAD
          DIST = R * ABS(ACOS(COS(XL)*COS(YL)
     +                 + SIN(XL)*SIN(YL)*COS(DL)))
          IF (DIST.LE.RADR)  ACK = 'y'
          IF (DODIAG.EQ.'y')  THEN
            IF (ACK.EQ.'n')  THEN
              WRITE (IDUNIT,7760) RECORDS, RR, XLAT, XLON,
     +          YLAT(K), YLON(K), DIST, RADR
7760          FORMAT (1X,'CKRAD:  RECORD ',I9,', REPORT ',I9,
     +          ', REJECTED, ',' XLAT ',F6.0,' XLON ',F6.0,' YLAT ',
     +          F8.2,' YLON ',F8.2,' DIST ',F6.0,' RADR ',I6)
            ELSE
              WRITE (IDUNIT,7761) RECORDS, RR, XLAT, XLON,
     +          YLAT(K), YLON(K), DIST, RADR
7761          FORMAT (1X,'CKRAD:  RECORD ',I9,', REPORT ',I9,
     +          ', ACCEPTED, ',' XLAT ',F6.0,' XLON ',F6.0,' YLAT ',
     +          F8.2,' YLON ',F8.2,' DIST ',F6.0,' RADR ',I6)
            ENDIF
          ENDIF
          EXIT
        ENDDO
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKWMO(RECORDS,RR,A8RPID,WMOLIST,NWMO,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK WMO STATION NUMBER (bbsss)
C
        INTEGER RECORDS, RR
        CHARACTER*8 A8RPID
        CHARACTER*5 WMOLIST(100)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
            WRITE (IDUNIT,7769) RECORDS, RR, A8RPID(1:5)
7769        FORMAT (1X,'CKWMO:  RECORD ',I9,', REPORT ',I9,
     +       ', STATION ',A5)
        DO N = 1, NWMO
          IF (A8RPID(1:5).EQ.WMOLIST(N)(1:5))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
C        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7770) RECORDS, RR, A8RPID(1:5)
7770        FORMAT (1X,'CKWMO:  RECORD ',I9,', REPORT ',I9,
     +       ', REJECTED, STATION ',A5)
          ELSE
            WRITE (IDUNIT,7771) RECORDS, RR, A8RPID(1:5)
7771        FORMAT (1X,'CKWMO:  RECORD ',I9,', REPORT ',I9,
     +       ', ACCEPTED, STATION ',A5)
C          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKWBB(RECORDS,RR,A8RPID,WBBLIST,NWBB,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK WMO STATION BLOCK NUMBER (bb)
C
        INTEGER RECORDS, RR
        CHARACTER*8 A8RPID
        CHARACTER*2 WBBLIST(100)
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'n'
        DO N = 1, NWBB
          IF (A8RPID(4:5).EQ.WBBLIST(N)(1:2))  THEN
            ACK = 'y'
            EXIT
          ENDIF
        ENDDO
c        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7775) RECORDS, RR, A8RPID(1:8)
7775        FORMAT (1X,'CKWBB:  RECORD ',I9,', REPORT ',I9,
     +       ', REJECTED, STATION ',A8)
          ELSE
            WRITE (IDUNIT,7776) RECORDS, RR, A8RPID(1:8)
7776        FORMAT (1X,'CKWBB:  RECORD ',I9,', REPORT ',I9,
     +       ', ACCEPTED, STATION ',A8)
          ENDIF
c        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE CKELEV (RECORDS,RR,IEL,IELO,IEHI,
     +    IDUNIT,DODIAG,ACK)
C
C       CHECK STATION ELEVATION
C
        INTEGER RECORDS, RR
        CHARACTER*1 DODIAG, ACK
        SAVE
        ACK = 'y'
        IF (IEL.LT.IELO.OR.IEL.GT.IEHI)  ACK = 'n'
        IF (DODIAG.EQ.'y')  THEN
          IF (ACK.EQ.'n')  THEN
            WRITE (IDUNIT,7780) RECORDS, RR, IEL, IELO, IEHI
7780        FORMAT (1X,'CKELEV: RECORD ',I9,', REPORT ',I9,
     +       ', REJECTED, ELEV ',I6,' BOTTOM ',I6,', TOP ',I6)
          ELSE
            WRITE (IDUNIT,7781) RECORDS, RR, IEL, IELO, IEHI
7781        FORMAT (1X,'CKELEV: RECORD ',I9,', REPORT ',I9,
     +       ', ACCEPTED, ELEV ',I6,' BOTTOM ',I6,', TOP ',I6)
          ENDIF
        ENDIF
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE UP2LOW (WORD1,WORD2,NCH)
C
C       MAP UPPER CASE CHARACTERS TO LOWER CASE
C
        CHARACTER*100 WORD1, WORD2
        CHARACTER*1 CHU(26), CHL(26)
        DATA CHU /
     +  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     +  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'/
        DATA CHL /
     +  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     +  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'/
        SAVE
        DO  N = 1, NCH
          K = 13        ! (M)
          KSTEP = 7
          IDO = 0
          DO
            IDO = IDO + 1
            IF (IDO.GT.6)  EXIT
            IF (WORD1(N:N).EQ.CHU(K))  THEN
              WORD2(N:N) = CHL(K)
              GO TO 9
            ENDIF
            IF (WORD1(N:N).LT.CHU(K))  THEN
              K = K - KSTEP
            ELSE
              K = K + KSTEP
            ENDIF
            KSTEP = KSTEP * .5
            IF (KSTEP.LT.1)  KSTEP = 1
          ENDDO
   9      CONTINUE
        ENDDO
        RETURN
        END
C
C       ########################################################################
C
        SUBROUTINE LOW2UP (WORD1,WORD2,NCH)
C
C       MAP LOWER CASE CHARACTERS TO UPPER CASE
C
        CHARACTER*100 WORD1, WORD2
        CHARACTER*1 CHU(26), CHL(26)
        DATA CHU /
     +  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     +  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'/
        DATA CHL /
     +  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     +  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'/
        SAVE
        DO  N = 1, NCH
          K = 13        ! (M)
          KSTEP = 7
          IDO = 0
          DO
            IDO = IDO + 1
            IF (IDO.GT.6)  EXIT
            IF (WORD1(N:N).EQ.CHL(K))  THEN
              WORD2(N:N) = CHU(K)
              GO TO 9
            ENDIF
            IF (WORD1(N:N).LT.CHL(K))  THEN
              K = K - KSTEP
            ELSE
              K = K + KSTEP
            ENDIF
            KSTEP = KSTEP * .5
            IF (KSTEP.LT.1)  KSTEP = 1
          ENDDO
   9      CONTINUE
        ENDDO
        RETURN
        END
        SUBROUTINE VALIDCH (CHSTR,NCH,ACK)
C
C       CHECK WHETHER STRING IS A SIMPLE STRING OF NCH CHARACTERS
C         IN THE SET A-Z AND 0-9
C
        CHARACTER*8 CHSTR
        CHARACTER*1 ALNU(37), ACK, OK
        DATA ALNU /
     +  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     +  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
     +  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ' '/
        SAVE
        ACK = 'y'
        DO  N = 1, NCH
          OK = 'n'
          DO  I = 1, 37
            IF (CHSTR(N:N).EQ.ALNU(I))  THEN
              OK = 'y'
              EXIT
            ENDIF
          ENDDO
          IF (OK.EQ.'y')  CYCLE
          ACK = 'n'
          GO TO 9
        ENDDO
   9    CONTINUE
        RETURN
        END
C ***
C ***   When modifying bufrupprair.f, after editing, a reinstall must
C ***     be done by doing
C ***          cd .../bufrdecode/src
C ***          [edit] bufrupprair.f
C ***          cd ../install
C ***          cat install.sh | sed s/CPLAT=linux/CPLAT=sun/ >! mk_exec.sh
C ***          chmod 700 mk_sun_exec.sh
C ***          mk_sun_exec.sh >>&! /dev/null
C ***     Note that the install expects to be run in the .../install
C ***     directory, to find the source code in a file named
C ***     .../src/bufrupprair.f , and a library of supporting code in
C ***     .../lib , and will write the executable in
C ***     .../exe/bufrupprair.x .  It's default platform is "linux".  You
C ***     you may need to change that to "sun" or something.
C ***
C ***   The install creates a script named convert.csh in directory .../exe,
C ***     which runs every file found in directory .../bufrobs through
C ***     the upprair - .../exe/bufrupprair.x leaving outputs in directory
C ***     .../textobs.
C ***
C ***   Note that the published data files are in:
C ***     /datazone/dsszone/ds351.0/
C ***     A single typical ds351.0 tar file has a name like:
C ***     gdasupaobs.20100410.tar.gz
C ***
