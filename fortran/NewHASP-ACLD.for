**********************************************************************
**********************************************************************
*****                                                            *****
*****      DYNAMIC HEAT LOAD PROGRAM FOR ENERGY SIMULATION       *****
*****                                                            *****
*****      HASP/ACLD/8501       CODED BY Y.MATSUO                *****
*****                                                            *****
*****      NewHASP/ACLD         REVISED BY T.NAGAI               *****
**********************************************************************
**********************************************************************
*
*****      1. JOB START **********************************************
*
      CHARACTER  QVER*8    ! バージョン情報
      PARAMETER (QVER='20200403')                                               ! rev 20200403(T.Nagai)
      PARAMETER (NUB= 1,NUW=11,NUO=12,NUOT=20)   ! NUOT:テキスト出力先装置番号
      PARAMETER (NUWN=2)   ! 窓データファイル装置番号
      PARAMETER (NUBW=3)    ! WCON物性値データ入力装置番号
      PARAMETER (NUOW=10)   ! NUOW:外気温・外気湿度テキスト出力先装置番号
      PARAMETER (NUOB=13)   ! BECSへの受け渡しファイル用装置番号
      PARAMETER (MX=30000)
      PARAMETER (HC=3.5,HR=4.5,HT=HC+HR,FC=HC/HT,FR=HR/HT,HO=20.)
      PARAMETER (DR=0.0174533)
*
      DIMENSION GAS(0:9)   ! FURN(顕熱)による冷房負荷
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X
*
      DIMENSION MT(21),TH(21),G(0:9),P(8),WF(9,4)
      DIMENSION GTR(0:9),GAD(0:9),GRM(0:9),GRL(0:9)
      DIMENSION WD(7,24),ID(7,5),SH(24),CHSA(24),CHCA(24)
      DIMENSION WD8(24)   ! 外気飽和湿度－外気絶対湿度
      DIMENSION ROH(3),ROL(3)
      PARAMETER(MXGL=200,NBLD=3,NTBL=4,MXVS=10,MXGT=100)
                            !  MXGL    :特性値表1つあたり最大ガラス数
                            !  NBLD    :ブラインド色種別数
                            !  NTBL    :窓特性値表の数
                            !  MXVS    :通気量の最大サンプリング数
                            !  MXGT    :最大ガラス種別数
      DIMENSION GLK(MXGL,0:NBLD,NTBL),   ! K[kcal/m2hdeg]
     -          GLR(MXGL,0:NBLD,NTBL), GLC(MXGL,0:NBLD,NTBL)    ! SCR, SCC
      DIMENSION MGT(MXGL,NTBL)       ! ガラス種別
      DIMENSION GLD(MXVS,0:NBLD,0:MXGT/10,6)   ! 第4添字=1:AFWのΔSC
      ! =2:AFWのΔU, =3:PPWのΔSC(Xpull=0), =4:PPWのΔU(Xpull=0),
      ! =5:PPWのΔSC(Xpull=1), =6:PPWのΔU(Xpull=1)
      DIMENSION GLKR(MXGT)            ! 長波放射成分係数kLR（内側ブラインドなし）
      REAL      GLKRB                 ! 内側ブラインドのkLR
      REAL      GLKRBO                ! ブラインドの総合熱伝達率に対する放射熱伝達率の比
      DIMENSION NVS(6)                ! 通気量のサンプリング数（添字は「GLD」第4添字と同）
      DIMENSION GLWK(2,2)             ! Work array(第2添字=1:ΔSC, =2:ΔU,
                                      ! 第1添字=1:ブラインド開, =2:閉)
      DIMENSION FL(5,3)
      DIMENSION AM(3,9)
      INTEGER   MCNTL(32)   ! 「CNTL」カードのデータ内容(XMQ配列に相当)           rev 20200403(T.Nagai)
      COMMON /ETC/MCNTL
      DIMENSION MDW(2),WDND(7,24),IDND(7,5),KSCH(2)
                            !  MDW(1)  :本日の曜日(=1:月,2:火,..,7:日,8:祝,9:特)
                            !  MDW(2)  :明日の曜日
                            !  WDND    :明日の気象データ
                            !  IDND    :明日の日付データ
                            !  KSCH(1) :本日のスケジュール(=1:全日,2:半日,3:1日中0%)
                            !  KSCH(2) :明日のスケジュール
      INTEGER   ISEAS(2)    !  ISEAS(1):本日の季節(=1:夏期,2:冬期,3:中間期)
                            !  ISEAS(2):明日の季節
      PARAMETER(NAZ=20,NHR=24,NSL=2,NWD=7)
                            !  NAZ     :1グループあたりの最大スペース数(変更の場合は
                            !           関連ルーチンのPARAMETER文を全て変更する必要あり)
                            !  NHR     :1日のステップ数(24以外不可)
                            !  NSL     :顕熱と潜熱(2以外不可)
                            !  NWD     :気象データの種類(7以外不可)
      INTEGER   IOPTG(NAZ,NHR)        ! 空調運転状態フラグ、=0:停止中,=1:運転中,=2:起動,=3:停止
      INTEGER   IOPVG(NAZ,NHR)        ! 外気導入状態フラグ、
                                      !     =0:カット中,=1:導入中,=2:導入開始,=3:導入停止
      REAL      SMRT1(NAZ,NHR)        ! 面積を持たない部位からの冷房負荷
      REAL      SMRT2(NAZ,NHR)        ! INFLの吸熱応答係数（瞬時）
      REAL      VOAG(NAZ)             ! 導入時の外気量
      INTEGER   LCG(NAZ)              ! XMQ配列のSPACデータへのポインタ（L）
      REAL      CLDG(NAZ,NHR,NSL)     ! 冷房負荷
      REAL      REFWD(NSL)            ! 基準温湿度
      REAL      P0(NAZ,0:1,NHR,NSL)   ! 瞬時蓄熱応答係数（吸熱される側が正）
                                      !     第2添字=0:二等辺三角
                                      !     第2添字=1:右側直角二等辺三角
      REAL      EXCAP(NAZ,NSL)        ! 各スペースの装置容量（冷却、0以上）
      REAL      SPCAP(NAZ,NSL)        ! 各スペースの装置容量（加熱、0以上）
      REAL      RMMX(NAZ,NSL)         ! 各スペースの設定温湿度上限
      REAL      RMMN(NAZ,NSL)         ! 各スペースの設定温湿度下限
                                      ! （RMMX,RMMNは基準温湿度からの偏差ではない）
      INTEGER   LOPC                  ! OPCOデータへのポインタ(L)
      INTEGER   IZ                    ! 当該スペースは現在のグループの何スペース目か
      INTEGER   IDWK(3)               ! 年・月・日
      INTEGER   NZ                    ! 現在のグループのスペース数
      REAL      VFLOW(NAZ,NAZ,NHR)    ! 第1添字目のスペースから第2添字目のスペースへの流入
                                      !     風量（体積流量、0以上、対角項は0とする）
      INTEGER   ICYCL                 ! 気象データをRewindした回数（Rewind後）
      INTEGER   ICYCLO                ! 気象データをRewindした回数（Rewind前）
      INTEGER   ISTAT                 ! 気象データファイルの状態
                                      !     =1:通常  0:ファイル終了(IOPWE=1のとき)
      INTEGER   ISTAT2                ! SUBROUTINE RTVADJからの返り値
      INTEGER   IOPTWK                ! 空調運転状態フラグ、=0:停止中、=1:運転中、
                                      ! =2:起動、=3:停止
      REAL      WINCHR(0:2,0:1)       ! 窓の物性値（第1添字=0:K, 1:SCC, 2:SCR、
                                      !             第2添字=0:ブラインド開時、1:閉時）
      INTEGER   MFLWK(2)              ! CFLWデータセット用Work array
      REAL      XFLWK(2)              ! CFLWデータセット用Work array
      INTEGER   LSZSPC(0:4)           ! XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
                                      ! (3):WNDW, (4):INFL の変数の数
      INTEGER   IWFLG(4)              ! 気象データヘッダ行のデータ                add 20200403(T.Nagai)
                                      ! (1) =0:ヘッダ行がない、=1:ヘッダ行がある
                                      ! (2):日射・放射の単位 =0:10kJ/m2h,
                                      !   =1:kcal/m2h, =2:kJ/m2h、
                                      ! (3) 雲量モード =0:雲量, =1:夜間放射、
                                      ! (4) 気象データのカラム数(3以上9以下)
      REAL      RWFLG(3)              ! 気象データヘッダ行のデータ                add 20200403(T.Nagai)
                                      ! (1) 緯度[deg]（南緯の場合は負値）、
                                      ! (2) 経度[deg]（西経の場合は負値）、
                                      ! (3) 世界時と地方標準時の時差
                                      !  （日本の場合は9.0）
*
      CHARACTER QKY*4,QD*80,QJB*80,QSP*4,QERR*1
      CHARACTER QWK*200
      CHARACTER QPATH*200   ! 出力データパス(".....\"まで)
      DATA LSZSPC/218,16,26,47,8/                                               ! rev 20200403(T.Nagai)
*
***        INSIDE SURFACE REFLECTANCE
      DATA ROH,ROL/0.7, 0.5, 0.3, 0.3, 0.2, 0.1/
*
***        WINDOW GLASS DATA (Initialization)
      DATA GLK,GLR,GLC/3200*9.999,3200*9.999,3200*9.999/
      DATA GLD/2640*9.999/
      DATA GLKR,GLKRB,GLKRBO/102*9.999/
*
***        WF FOR LIGHTING FIXTURE
      DATA FL/0.4438,0.0534,0.8972,0.0362,0.0000,
     *        0.7321,0.0254,0.8926,0.0309,0.0000,
     *        1.0000,0.0000,0.0000,0.0000,0.0000/
*
***        OCCUPANCY HEAT DISCHARGE
      DATA AM/79.,50.,-3.0,91.,53.,-3.1,102.,54.,-3.4,113.,55.,-3.6,
     *     125.,59.,-3.8,170.,65.,-5.6,194.,72.,-6.0,227.,85.,-6.3,
     *     329.,118.,-5.4/
*
***        SATURATION HUMIDITY
      SATX(T)=1000.*EXP(-5.58001+T*(0.0780136+T*(-2.87894E-04
     *       +T*(1.36152E-06+T*3.49024E-09)))-4.87306E-03*ABS(T))
*
***        SOLAR GAIN FACTOR
      GF(Z)=Z*(2.392+Z*(-3.8636+Z*(3.7568-Z*1.3952)))
*
***        WIND PRESSURE COEFFICIENT
      CF(Z)=-0.01107+Z*(0.03675+Z*0.02332)
*
      DO 102 J=1,24
      SH(J)=0.
      CHSA(J)=0.
  102 CHCA(J)=0.
      DO 101 I=1500,MX   ! 1499番までは固定番地                                   rev 20121002(T.Nagai) Debug
  101 M(I)=0

      READ(5,'(A)') QWK  ! INPUT DATA FILE NAME
      OPEN(NUB,FILE=QWK,STATUS='OLD',FORM='FORMATTED')
      READ(5,'(A)') QWK  ! WEATHER DATA FILE NAME
      OPEN(NUW,FILE=QWK,STATUS='OLD',FORM='FORMATTED')
      READ(NUW,'(A)') QWK                                                       ! add 20200403(T.Nagai)
      IF(QWK(1:1).EQ.'*') THEN                                                  ! add 20200403(T.Nagai)
        IWFLG(1) = 1                                                            ! add 20200403(T.Nagai)
        CALL RHEAD(QWK,IWFLG,RWFLG)                                             ! add 20200403(T.Nagai)
        MCNTL(3) = IWFLG(3)                                                     ! add 20200403(T.Nagai)
        MCNTL(4) = IWFLG(2)                                                     ! add 20200403(T.Nagai)
        MCNTL(31) = IWFLG(4)                                                    ! add 20200403(T.Nagai)
      ELSE                                                                      ! add 20200403(T.Nagai)
        IWFLG(1) = 0                                                            ! add 20200403(T.Nagai)
        BACKSPACE(NUW)                                                          ! add 20200403(T.Nagai)
      END IF                                                                    ! add 20200403(T.Nagai)
      READ(5,'(A)') QPATH  ! OUTPUT DATA FOLDER NAME
      OPEN(NUOW,FILE=QPATH(1:LEN_TRIM(QPATH))//'weath.dat')  !外気温湿度出力ファイル
      WRITE(NUOW,'(A)') 'YEAR,MO,DY,YB,HR,,OUT-T,OUT-X'
***        WINDOW GLASS DATA (Read from file)
      READ(5,'(A)') QWK  ! WINDOW DATA FILE NAME
      OPEN(NUWN,FILE=QWK,STATUS='OLD',FORM='FORMATTED')
*     K,SCC,SCRの読み込み
      DO 90 II=1,NTBL   ! Tableループ
       READ(NUWN,'()')
       READ(NUWN,*) L1
       IF(L1.GT.MXGL) CALL ERROR(48,NERR)
       DO 90 I=1,L1   ! ガラスループ
        READ(NUWN,*) I1,MGT(I,II),(GLK(I,J,II),GLC(I,J,II),GLR(I,J,II),
     -               J=0,NBLD)
        IF(MGT(I,II).GT.MXGT) CALL ERROR(49,NERR)
        DO 98 J=0,NBLD
         IF(ABS(GLK(I,J,II)-9.999).GT.0.001) THEN
          GLK(I,J,II)=GLK(I,J,II)*0.86   ! [W/m2K]から[kcal/m2hdeg]へ
         END IF
   98   CONTINUE
   90 CONTINUE
*     AFW,PPW関連データの読み込み
      DO 92 II=1,6
       READ(NUWN,'()')
       READ(NUWN,*) L1,NVS(II)
       IF(L1.GT.MXGT/10) CALL ERROR(49,NERR)
       IF(NVS(II).GT.MXVS) CALL ERROR(50,NERR)
       READ(NUWN,*) IWK,(GLD(JJ,0,0,II),JJ=1,NVS(II))
       DO 92 I=1,L1
        DO 92 J=0,NBLD
         READ(NUWN,*) IWK,(GLD(JJ,J,I,II),JJ=1,NVS(II))
         IF(MOD(II,2).EQ.0) THEN   ! Uの単位変換
          DO 93 JJ=1,NVS(II)
           IF(ABS(GLD(JJ,J,I,II)-9.999).GT.0.001) THEN
            GLD(JJ,J,I,II)=GLD(JJ,J,I,II)*0.86
           END IF
   93     CONTINUE
         END IF
   92 CONTINUE
*     kLR等の読み込み
      READ(NUWN,'()')
      READ(NUWN,*) L1
      DO 94 I=1,L1
       READ(NUWN,*) L2,GLKR(L2)
       IF(L2.GT.MXGT) CALL ERROR(49,NERR)
   94 CONTINUE
      READ(NUWN,'()')
      READ(NUWN,*) GLKRB, GLKRBO
      IF(GLKRB.GT.0.9999) CALL ERROR(51,NERR)
      READ(5,'(A)') QWK  ! WALL CONSTRUCTION DATA FILE NAME
      OPEN(NUBW,FILE=QWK,STATUS='OLD',FORM='FORMATTED')
      READ(5,'(A)',END=99) QWK  ! OUTPUT FILE NAME FOR ACSS
      IF(LEN_TRIM(QWK).EQ.0) THEN
        IACSS=0
      ELSE
        IACSS=1
        OPEN(NUO,FILE=QPATH(1:LEN_TRIM(QPATH))//QWK,STATUS='UNKNOWN',
     -       FORM='FORMATTED')
      END IF
      READ(5,'(A)',END=99) QWK  ! OUTPUT FILE NAME FOR BECS
      IF(LEN_TRIM(QWK).EQ.0) THEN
        IBECS=0
      ELSE
        IBECS=1
        OPEN(NUOB,FILE=QPATH(1:LEN_TRIM(QPATH))//QWK,STATUS='UNKNOWN',
     -       FORM='FORMATTED')
      END IF
*
   99 NERR=0
      QERR='&'
      READ(NUB,'(A80)') QJB
      WRITE(6,'(1H1,A80)') QJB
*
*****       2. PRELIMINARY PROCESS ***********************************
***          2.1. BUILDING COMMON DATA *******************************
*
      L=1500                                                                    ! rev 20121002(T.Nagai) Debug
*
  100 READ(NUB,'(A80)') QD
      WRITE(6,'(1X,A80)') QD
      QKY=QD(1:4)
      IF(QKY.EQ.'    ') GO TO 200
      IF(QKY.EQ.'BUIL') GO TO 110
      IF(QKY.EQ.'CNTL') GO TO 115
      IF(QKY.EQ.'HRAT') GO TO 125
      IF(QKY.EQ.'EXPS') GO TO 120
      IF(QKY.EQ.'WCON') GO TO 130
      IF(QKY.EQ.'WSCH') GO TO 140
      IF(QKY.EQ.'DSCH') GO TO 150
      IF(QKY.EQ.'SDAY') GO TO 160
      IF(QKY.EQ.'SEAS') GO TO 170
      IF(QKY.EQ.'OPCO') GO TO 180
      IF(QKY.EQ.'OSCH') GO TO 190
      IF(QKY.EQ.'OAHU') GO TO 195
      CALL ERROR(1,NERR)
      GO TO 100
*
***          2.2. 'BUIL' DATA ****************************************
*
  110 CONTINUE
      CALL DCHECK(QD,560,NERR)
*
      IF(QD(30:35).EQ.'      ') QD(30:35)='    10'
      IF(QD(36:41).EQ.'      ') QD(36:41)='  24.0'
      IF(QD(42:47).EQ.'      ') QD(42:47)='    50'
      IF(QD(48:53).EQ.'      ') QD(48:53)='   200'
      IF(QD(54:59).EQ.'      ') QD(54:59)='   9.0'                              ! add 20200403(T.Nagai)
      READ(QD(12:59),'(8F6.0)') W1,W2,(X(I),I=153,156),X(158),W3                ! rev 20200403(T.Nagai)
      IF(IWFLG(1).EQ.1) THEN ! 気象データにヘッダ行がある場合                     add 20200403(T.Nagai)
        X(150)=SIN(DR*RWFLG(1))                                                 ! add 20200403(T.Nagai)
        X(151)=COS(DR*RWFLG(1))                                                 ! add 20200403(T.Nagai)
        X(152)=RWFLG(2)/15.-RWFLG(3)                                            ! add 20200403(T.Nagai)
      ELSE                                                                      ! add 20200403(T.Nagai)
        X(150)=SIN(DR*W1)
        X(151)=COS(DR*W1)
        X(152)=W2/15.-W3                                                        ! rev 20200403(T.Nagai)
      END IF                                                                    ! add 20200403(T.Nagai)
      X(154)=0.01*X(154)
      X(157)=SATX(X(155))*X(156)/100.
      X(158)=0.860*X(158)
      REFWD(1)=X(155)
      REFWD(2)=X(157)
*
      GO TO 100
*
***          2.2.5 'CNTL' DATA ***************************************
*
  115 CONTINUE
      CALL DCHECK(QD,1001,NERR)
*
      DO 116 I=1,15
       L1=11+(I-1)*3
       IF(((I.NE.3).AND.(I.NE.4)).OR.(IWFLG(1).EQ.0)) THEN                      ! add 20200403(T.Nagai)
         ! 気象データヘッダ行の記述を優先する場合を除き                           add 20200403(T.Nagai)
         IF(QD(L1+1:L1+3).NE.'   ') THEN
           READ(QD(L1+1:L1+3),'(I3)') MCNTL(I)
         END IF
       END IF                                                                   ! add 20200403(T.Nagai)
       ! 4.8 INITIALIZATION で予めデフォルト値が代入されている
       ! (カード自体が省略された場合を想定)
  116 CONTINUE
      IF(QD(60:62).NE.'   ') THEN                                               ! add 20200403(T.Nagai)
        READ(QD(60:62),'(I3)') MCNTL(32)                                        ! add 20200403(T.Nagai)
      END IF                                                                    ! add 20200403(T.Nagai)
*
      DO 117 I=1,3
       II=6+(I-1)*3
       IF(MCNTL(5).EQ.2)THEN
        IF(MCNTL(II).GE.51)THEN   ! 51年以上の場合は1900年代とみなす
         MCNTL(II)=1900+MCNTL(II)
        ELSE
         MCNTL(II)=2000+MCNTL(II)
        END IF
       ELSE
        MCNTL(II)=0
       END IF
       MCNTL(18+I)=NDATF(MCNTL(II),MCNTL(II+1),MCNTL(II+2))
       ! 1899/12/31を1とした通算日数(EXCELと合わせるため)
       ! 実在気象データ以外が指定されたときは1999年となる
  117 CONTINUE
*
      GO TO 100
*
***          2.2.7 'HRAT' DATA ***************************************
*
  125 CONTINUE
      CALL DCHECK(QD,1341,NERR)
*
      DO 126 I=1,9
       L1=11+(I-1)*3
       IF(QD(L1+1:L1+3).NE.'   ') THEN
        READ(QD(L1+1:L1+3),'(I3)') MCNTL(21+I)
       END IF
       ! 4.8 INITIALIZATION で予めデフォルト値が代入されている
       ! (カード自体が省略された場合を想定)
  126 CONTINUE
*
      GO TO 100
*
***          2.3. 'EXPS' DATA ****************************************
*
  120 CONTINUE
      CALL DCHECK(QD,597,NERR)
      CALL RETRIV(100,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.0) THEN
       CALL ERROR(2,NERR)
       WRITE(QD(6:9),'(A1,I3)') QERR,NERR
       GO TO 120
      END IF
*
      M(LC)=L
      M(L)=LD
      M(L+1)=NNAM
      READ(QD(12:29),'(2F6.0,2F3.0)') W2,W1,X(L+12),X(L+13)
      X(L+2)=W1
      X(L+3)=SIN(DR*W1)
      X(L+4)=COS(DR*W1)
      X(L+5)=SIN(DR*W2)
      X(L+6)=COS(DR*W2)
      X(L+7)=X(L+3)*X(L+5)
      X(L+8)=X(L+3)*X(L+6)
      X(L+9)=X(L+4)*X(L+5)
      X(L+10)=X(L+4)*X(L+6)
      X(L+11)=(1.-X(L+6))/2.
      READ(QD(30:53),'(4F6.0)') (X(I),I=1,4)
      X(L+14)=X(1)
      X(L+15)=X(3)
      X(L+16)=X(2)+X(3)
      X(L+17)=X(3)+X(4)
      X(L+18)=X(2)+X(3)+X(4)
      READ(QD(54:77),'(4F6.0)') (X(I),I=1,4)
      X(L+19)=X(1)
      X(L+20)=X(3)
      X(L+21)=X(2)+X(3)
      X(L+22)=X(3)+X(4)
      X(L+23)=X(2)+X(3)+X(4)
      X(L+25)=X(L+15)*X(L+20)
      X(L+24)=X(L+18)*X(L+23)-X(L+25)
      IF(X(L+18).EQ.0.) THEN
       X(L+26)=0.
      ELSE
       X(L+26)=0.5*(X(L+18)+X(L+14)-SQRT(X(L+18)**2+X(L+14)**2))/X(L+18)
      END IF
      IF(X(L+23).EQ.0.) THEN
       X(L+27)=0.
      ELSE
       X(L+27)=(X(L+23)+X(L+19)-SQRT(X(L+23)**2+X(L+19)**2))/X(L+23)
      END IF
*
      L=L+76                                                                    ! rev 20200403(T.Nagai)
      GOTO 100
*
***          2.4. 'WCON' DATA ****************************************
*
  130 CONTINUE
      CALL DCHECK(QD,615,NERR)
      CALL RETRIV(102,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.0) THEN
       CALL ERROR(2,NERR)
       WRITE(QD(6:9),'(A1,I3)') QERR,NERR
       GO TO 130
      END IF
*
      M(LC)=L
      M(L)=LD
      M(L+1)=NNAM
*
      NL=0
  131 L1=L+2*NL+3
      K1=6*NL+12
      READ(QD(K1:K1+5),'(I3,F3.0)') M1,W1
      IF(M1.NE.0) THEN
       M(L1)=M1
       X(L1+1)=0.001*W1
       NL=NL+1
       IF(NL.LT.11) GO TO 131
      END IF
*
      M(L+2)=NL
*
      L=L+2*NL+3
      GO TO 100
*
***          2.5. 'WSCH' DATA ****************************************
*
  140 CONTINUE
      CALL DCHECK(QD,661,NERR)
      CALL RETRIV(108,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.0) THEN
       CALL ERROR(2,NERR)
       WRITE(QD(6:9),'(A1,I3)') QERR,NERR
       GO TO 140
      END IF
*
      M(LC)=L
      M(L)=LD
      M(L+1)=NNAM
      READ(QD(12:38),'(9I3)') (M(I),I=L+2,L+10)
*
      L=L+11
      GO TO 100
*
***          2.6. 'DSCH' DATA ****************************************
*
  150 CONTINUE
      CALL RETRIV(104,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.0) THEN
       CALL ERROR(2,NERR)
       WRITE(QD(6:9),'(A1,I3)') QERR,NERR
       GO TO 150
      END IF
*
      M(LC)=L
      M(L)=LD
      M(L+1)=NNAM
*
      DO 151 LL=L+2,L+73
  151 X(LL)=0.
      DO 154 I=1,3
      IF(I.GE.2) THEN
       READ(NUB,'(A80)') QD
       IF(QD(1:4).EQ.'+   ') THEN
        WRITE(6,'(1X,A80)') QD
       ELSE
        BACKSPACE(NUB)
        GO TO 156
       END IF
      END IF
      CALL DCHECK(QD,1225,NERR)
      IF(QD(12:14).EQ.'   ') THEN
       QD(12:20)='  1  0 24'
      END IF
      K1=12
      L1=L+24*I-23
      READ(QD(K1:K1+2),'(I3)') M1
  152 IF(K1.LE.72.AND.M1.NE.0) THEN
       READ(QD(K1+3:K1+8),'(F3.0,I3)') W,M2
       DO 153 J=M1,M2
  153  X(L1+J)=0.01*W
       M1=M2
       K1=K1+6
       GOTO 152
      END IF
  154 CONTINUE
*
  156 L=L+74
      GO TO 100
*
***          2.7. 'SDAY' DATA ****************************************
*
  160 CONTINUE
*     CALL DCHECK(QD,759,NERR)                                                  ! del 20200410(T.Nagai)
*
      N=0
      DO 162 I = 1, 34 ! データ行のループ(34:365日を特別日とする場合の行数)       rev 20200410(T.Nagai)
        IF(I.GE.2) THEN                                                         ! rev 20200410(T.Nagai)
          READ(NUB,'(A80)') QD                                                  ! rev 20200410(T.Nagai)
          IF(QD(1:4).EQ.'+   ') THEN                                            ! rev 20200410(T.Nagai)
            WRITE(6,'(1X,A80)') QD                                              ! rev 20200410(T.Nagai)
          ELSE                                                                  ! rev 20200410(T.Nagai)
            BACKSPACE(NUB)                                                      ! rev 20200410(T.Nagai)
            GO TO 169                                                           ! rev 20200410(T.Nagai)
          END IF                                                                ! rev 20200410(T.Nagai)
        END IF                                                                  ! rev 20200410(T.Nagai)
        CALL DCHECK(QD,753,NERR)                                                ! rev 20200410(T.Nagai)
        READ(QD(12:77),'(22I3)') (M(M1),M1=171,192)                             ! rev 20200410(T.Nagai)
        DO 163 I2 = 1, 11                                                       ! rev 20200410(T.Nagai)
          M1=171+2*(I2-1)                                                       ! rev 20200410(T.Nagai)
          IF(M(M1).NE.0) THEN                                                   ! rev 20200410(T.Nagai)
            M2=NDATE(M(M1),M(M1+1))
            M(192+M2)=1                                                         ! rev 20200410(T.Nagai)
            N=N+1
          END IF                                                                ! rev 20200410(T.Nagai)
  163   CONTINUE                                                                ! rev 20200410(T.Nagai)
  162 CONTINUE                                                                  ! rev 20200410(T.Nagai)
  169 M(170)=N                                                                  ! rev 20200410(T.Nagai)
*
      GO TO 100
*
*************        'SEAS' DATA*************************************
*
  170 CONTINUE
      CALL DCHECK(QD,894,NERR)
*
      DO 171 I=1,12
      L1=I*3+9
      IF(QD(L1:L1+2).NE.'   ') THEN
       READ(QD(L1:L1+2),'(I3)') M(980+I)
      END IF                                                                    ! rev 20200403(T.Nagai)
  171 CONTINUE                                                                  ! rev 20200403(T.Nagai)
      ! 4.8 INITIALIZATION で予めデフォルト値が代入されている
      ! (カード自体が省略された場合を想定)
*
      GO TO 100
*
*************        'OPCO' DATA*************************************
*
  180 CONTINUE
      CALL DCHECK(QD,1131,NERR)
      CALL RETRIV(145,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.0) THEN
       CALL ERROR(2,NERR)
       WRITE(QD(6:9),'(A1,I3)') QERR,NERR
       GO TO 180
      END IF
*
      M(LC)=L
      M(L)=LD
      M(L+1)=NNAM
*
      IF(QD(12:14).EQ.'   ') QD(12:14)='  0'
      READ(QD(12:14),'(I3)') M(L+164)
      IF(QD(15:17).EQ.'   ')THEN
       M1=-1
      ELSE
       READ(QD(15:17),'(I3)') M1
      END IF
      IF(QD(18:20).EQ.'   ')THEN
       M2=-1
      ELSE
       READ(QD(18:20),'(I3)') M2
      END IF
      IF(QD(75:80).EQ.'      ') QD(75:80)='   0.0'
      READ(QD(75:80),'(F6.0)') X(L+165)
*
      IF(QD(27:29).EQ.'   ') QD(27:29)='26.'
      IF(QD(30:32).EQ.'   ') QD(30:32)='26.'
      IF(QD(33:35).EQ.'   ') QD(33:35)='50.'
      IF(QD(36:38).EQ.'   ') QD(36:38)='50.'
      IF(QD(39:41).EQ.'   ') QD(39:41)='  1'
*
      IF(QD(45:47).EQ.'   ') QD(45:47)='22.'
      IF(QD(48:50).EQ.'   ') QD(48:50)='22.'
      IF(QD(51:53).EQ.'   ') QD(51:53)='40.'
      IF(QD(54:56).EQ.'   ') QD(54:56)='40.'
      IF(QD(57:59).EQ.'   ') QD(57:59)='  2'
*
      IF(QD(63:65).EQ.'   ') QD(63:65)='24.'
      IF(QD(66:68).EQ.'   ') QD(66:68)='24.'
      IF(QD(69:71).EQ.'   ') QD(69:71)='50.'
      IF(QD(72:74).EQ.'   ') QD(72:74)='50.'
*
      DO 181 II=1,3
       IWK=26+(II-1)*18
       L1=L+1+(II-1)*4
       READ(QD(IWK+1:IWK+12),'(4F3.0)') (X(L1+I),I=1,4)
       IF(X(L1+1).LT.X(L1+2)) CALL ERROR(40,NERR)
       IF(X(L1+3).LT.X(L1+4)) CALL ERROR(40,NERR)
       X(L1+3)=SATX(X(L1+1))*X(L1+3)/100.
          ! 最大気温、最大相対湿度から最大絶対湿度を求める
       X(L1+4)=SATX(X(L1+2))*X(L1+4)/100.   ! 上と同様
       IF(II.LE.2)THEN
        READ(QD(IWK+13:IWK+15),'(I3)') M(L+165+II)
       END IF
*
       IWK=23+(II-1)*18
       CALL RETRIV(147,QD(IWK+1:IWK+3)//' ',NNAM,LC,LD)
       I=L+14+(II-1)*50
       IF(LD.NE.LC) THEN   ! 該当するOSCHデータが見つからない場合
        IF(QD(IWK+1:IWK+3).NE.'   ')THEN
         READ(QD(IWK+1:IWK+3),'(I3)') M3
         X(I+M3)=1.
         X(I+25+M3)=1.
        END IF
        IF(M1.NE.-1)THEN
         X(I+M1)=-1.
        END IF
        IF(M2.NE.-1)THEN
         X(I+25+M2)=-1.
        END IF
       ELSE   ! 該当するOSCHデータが見つかった場合はそちらのスケジュールに従う
        DO 183 M4=1,2
         DO 184 M5=1,5
          L1=LC+1+(M4-1)*10+(M5-1)*2
          IF(M(L1+1).NE.-1)THEN
           X(I+(M4-1)*25+M(L1+1))=1.
          END IF
          IF(M(L1+2).NE.-1)THEN
           X(I+(M4-1)*25+M(L1+2))=-1.
          END IF
  184    CONTINUE
  183   CONTINUE
       END IF
  181 CONTINUE
*
      L=L+168
      GO TO 100
*
*************        'OSCH' DATA*************************************
*
  190 CONTINUE
      CALL DCHECK(QD,1061,NERR)
      CALL RETRIV(147,QD(6:8)//' ',NNAM,LC,LD)
      IF(LD.NE.0) THEN
       CALL ERROR(2,NERR)
       WRITE(QD(6:9),'(A1,I3)') QERR,NERR
       GO TO 190
      END IF
*
      M(LC)=L
      M(L)=LD
      M(L+1)=NNAM
*
      DO 192 II=1,2
       DO 194 I=1,5
        IWK=11+(II-1)*36+(I-1)*6
        IF(QD(IWK+1:IWK+3).EQ.'   ') QD(IWK+1:IWK+3)=' -1'
        IF(QD(IWK+4:IWK+6).EQ.'   ') QD(IWK+4:IWK+6)=' -1'
        READ(QD(IWK+1:IWK+3),'(I3)') M(L+1+(II-1)*10+(I-1)*2+1)
        READ(QD(IWK+4:IWK+6),'(I3)') M(L+1+(II-1)*10+(I-1)*2+2)
  194  CONTINUE
  192 CONTINUE
*
      L=L+22
      GO TO 100
*
*************        'OAHU' DATA*************************************
*
  195 CONTINUE
      CALL DCHECK(QD,1370,NERR)
      CALL RETRIV(98,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.0) THEN
       CALL ERROR(2,NERR)
       WRITE(QD(6:9),'(A1,I3)') QERR,NERR
       GO TO 195
      END IF
*
      M(LC)=L
      M(L)=LD
      M(L+1)=NNAM
*
      IF(QD(12:14).EQ.'   ') QD(12:14)='  0'
      READ(QD(12:14),'(F3.0)') X(L+2)
      X(L+2)=X(L+2)*0.01
      DO 196 II=1,3   ! 季節ループ
       IWK=14+(II-1)*18
       L1=L+2+(II-1)*9
*      全熱交換器
       IF(QD(IWK+1:IWK+3).EQ.'   ') THEN
        IF(QD(IWK+4:IWK+6).NE.'   ') CALL ERROR(46,NERR)
        M(L1+1)=0
       ELSE
        READ(QD(IWK+1:IWK+3),'(F3.0)') X(L1+2)
        IF(QD(IWK+4:IWK+6).EQ.'   ') THEN
         M(L1+1)=1
        ELSE
         M(L1+1)=2
         READ(QD(IWK+4:IWK+6),'(F3.0)') X(L1+3)
         X(L1+3)=SATX(X(L1+2))*X(L1+3)*0.01
        END IF
       END IF
*      外調機
       DO 197 I=1,2   ! 上下限ループ
        I1=IWK+6+(I-1)*3
        IF(QD(I1+1:I1+3).EQ.'   ') THEN
         IF(QD(I1+7:I1+9).NE.'   ') CALL ERROR(47,NERR)
         M(L1+3+I)=0
        ELSE
         READ(QD(I1+1:I1+3),'(F3.0)') X(L1+5+I)
         IF(QD(I1+7:I1+9).EQ.'   ') THEN
          M(L1+3+I)=1
         ELSE
          M(L1+3+I)=2
          READ(QD(I1+7:I1+9),'(F3.0)') X(L1+7+I)
          X(L1+7+I)=SATX(X(L1+5+I))*X(L1+7+I)*0.01
         END IF
        END IF
  197  CONTINUE
  196 CONTINUE
*
      L=L+183
      GO TO 100
*
***          2.8. 'SPAC' DATA ****************************************
*
  200 CONTINUE
*
      NRM=0
      IZ=1
      LCGB=L   ! 現在のグループの先頭スペースのSPACデータポインタ(L)
      CALL GVECTR('INIT',0,MT,TH,0.0,REAL(NUBW),GTR,GAD)   ! 物性値の読み込み
*
  204 READ(NUB,'(A80)') QD
      WRITE(6,'(1X,A80)') QD
      IF(QD(1:4).EQ.'CMPL') GO TO 400
      IF(QD(1:4).NE.'SPAC') THEN
       CALL ERROR(3,NERR)
       GO TO 204
      END IF
      CALL DCHECK(QD,821,NERR)
*
  205 QSP=QD(6:9)
      CALL RETRIV(106,QSP,NNAM,LC,LD)
      IF(LD.NE.0) THEN
       CALL ERROR(2,NERR)
       WRITE(QD(6:9),'(A1,I3)') QERR,NERR
       GO TO 205
      END IF
      M(LC)=L
      M(L)=LD
      NRM=NRM+1
      M(L+1)=NNAM
*
      CALL RETRIV(108,QD(10:13),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
      ELSE
       M(L+34)=LC+1
      END IF
*
      M(L+35)=0
      M(L+47)=0
      M(L+51)=0
      M(L+55)=0    ! SOPCデータを指定しないスペースはこの値のままとなる
      M(L+60)=0    ! 1時間前から現時刻までの運転状態（初期値）
      M(L+61)=0    ! 前日からの外気導入継続状態（初期値）
      IFURS=0      ! 0以外の家具顕熱容量が指定されたかどうか
      DO 206 II=1,2
       L1=L+86+(II-1)*7
       X(L1+1)=REFWD(II)
       X(L1+6)=REFWD(II)
       DO 207 I=2,5
        X(L1+I)=0.
  207  CONTINUE
       M(L1+7)=9
  206 CONTINUE
*
      CALL ARITH(QD,X(L+2),NERR)
      READ(QD(15:32),'(3F6.0)') X(L+5),X(L+3),X(L+4)
      X(L+6)=0.004*(X(L+5)-X(153)/2.)
      X(L+7)=SQRT(X(L+5)/25.)
      IF(X(L+7).LT.1.) X(L+7)=1.
      IF(QD(36:38).EQ.'   ') QD(36:38)='  3'
      READ(QD(33:38),'(I3,F3.0)') M(L+43),X(L+44)
      M1=M(L+43)
      IF(M1.EQ.0) M1=1
      X(L+46)=ROH(M1)/(X(L+2)*(1.-ROH(M1)*ROL(M1)))
      X(L+45)=ROL(M1)*X(L+46)
*
      LL=L
      L=L+LSZSPC(0)
      DO 201 J=0,9
      GAS(J)=0.
      GRM(J)=0.
  201 GRL(J)=0.
      ARM=0.
*
***          2.9. SPACE ELEMENT DATA *********************************
  202 READ(NUB,'(A80)') QD
      WRITE(6,'(1X,A80)') QD
      QKY=QD(1:4)
      IF(QKY.EQ.'    ') GO TO 310
      IF(QKY.EQ.':   ') GO TO 310
      IF(QKY.EQ.'CFLW') GO TO 310
      IF(QKY.EQ.'OWAL') GO TO 210
      IF(QKY.EQ.'IWAL') GO TO 220
      IF(QKY.EQ.'GWAL') GO TO 230
      IF(QKY.EQ.'BECO') GO TO 240
      IF(QKY.EQ.'WNDW') GO TO 250
      IF(QKY.EQ.'INFL') GO TO 260
      IF(QKY.EQ.'LIGH') GO TO 270
      IF(QKY.EQ.'OCUP') GO TO 280
      IF(QKY.EQ.'HEAT') GO TO 290
      IF(QKY.EQ.'FURN') GO TO 300
      IF(QKY.EQ.'SOPC') GO TO 1310
      CALL ERROR(4,NERR)
      GO TO 202
*
***          2.10. 'OWAL' DATA ***************************************
*
  210 CONTINUE
      CALL DCHECK(QD,830,NERR)
*
      M(L)=1
      CALL ARITH(QD,A,NERR)
      ARM=ARM+A
*
      CALL RETRIV(102,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
       GO TO 215
      END IF
      NL=M(LC+2)
      IF(QD(21:23).EQ.'   '   ) QD(21:23)='  0'      ! 蒸発比[%]
      IF(QD(24:29).EQ.'      ') QD(24:29)='   0.2'   ! 植栽と土壌の間の熱抵抗[m2K/W]
      READ(QD(21:29),'(I3,F6.0)') L1,W2
      W2=W2/0.86   ! [m2K/W] --> [m2hdeg/kcal]
      IF(L1.GT.0) THEN
       W1=0.60*60*L1*0.01
       HOX=HO+W1*1.0   ! 飽和絶対湿度の温度変化に対する微分を1.0[g/kg(DA)/deg]とする
       X(L+15)=W1/HOX
       NL=NL+1
       MT(NL)=-1   ! 純熱抵抗の意味
       TH(NL)=W2   ! 熱抵抗
      ELSE
       HOX=HO
       X(L+15)=0.0
      END IF
      DO 213 I=1,M(LC+2)
      L1=LC+2*I
      MT(I)=M(L1+1)
  213 TH(I)=X(L1+2)
      CALL GVECTR('OWAL',NL,MT,TH,HT,HOX,GTR,GAD)
      DO 211 J=0,9
  211 GRM(J)=GRM(J)+A*GAD(J)
      CALL CPARAM(2,GTR,P)
      X(L+2)=A*P(1)
      X(L+3)=A*P(3)
      X(L+4)=P(5)
      X(L+5)=A*P(6)
      X(L+6)=P(8)
*
  215 CALL RETRIV(100,QD(10:13),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
       GO TO 216
      END IF
      M(L+1)=LC
      V1=X(LC+26)
      V2=X(LC+11)
      V4=X(LC+27)
      V3=0.
      IF(X(LC+12).GT.0.) THEN
       W=SQRT(X(LC+12)**2+(X(LC+13)-X(LL+5))**2)
       W=(X(LC+6)*X(LC+12)-X(LC+5)*(X(LC+13)-X(LL+5)))/W
       V3=(1.-W)/2.
      END IF
*
      IF(V2.GT.V3) THEN
       IF(V1+V2.GT.1.) THEN
        U1=0.
        U2=(1.-V1-V3)*(1.-V4)
       ELSE
        U1=(1.-V1-V2)*(1.-V4)
        U2=(V2-V3)*(1.-V4)
       END IF
      ELSE
       IF(V1+V3.GT.1.) THEN
        U1=0.
        U2=0.
       ELSE
        U1=(1.-V1-V3)*(1.-V4)
        U2=0.
       END IF
      END IF
*
  216 IF(QD(15:17).EQ.'   ') QD(15:17)=' 80'
      IF(QD(18:20).EQ.'   ') QD(18:20)=' 90'
      READ(QD(15:20),'(2F3.0)') W1,W2
      X(L+9)=0.01*W1/HOX
      X(L+10)=X(154)*U2*X(L+9)
      X(L+11)=(U1+X(154)*U2)*X(L+9)
      X(L+12)=0.01*W2*U1/HOX
*
      IF(X(LC+12).NE.0.) THEN
       W=(X(LC+13)-X(LL+5))/X(LC+12)
       X(L+13)=W*X(LC+3)
       X(L+14)=W*X(LC+4)
      END IF
*
      L=L+LSZSPC(1)
      GO TO 202
*
***          2.11. 'IWAL' DATA ***************************************
*
  220 CONTINUE
      CALL DCHECK(QD,1296,NERR)
*
      CALL ARITH(QD,A,NERR)
      ARM=ARM+A
*
      CALL RETRIV(102,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
       GO TO 223
      END IF
      NL=M(LC+2)
      DO 224 I=1,NL
      L1=LC+2*I
      MT(I)=M(L1+1)
  224 TH(I)=X(L1+2)
      CALL GVECTR('IWAL',NL,MT,TH,HT,HT,GTR,GAD)
*
      READ(QD(12:20),'(I3,F6.0)') I,W1
      IF((I.EQ.0).AND.(W1.EQ.0.)) THEN
       DO 221 J=0,9
  221  GRM(J)=GRM(J)+A*(GAD(J)-GTR(J))
      ELSE
       M(L)=2
       M(L+1)=I
       DO 222 J=0,9
        IF(M(L+1).EQ.0) THEN
         GRM(J)=GRM(J)+A*(GAD(J)-(1.-W1)*GTR(J))
        ELSE
         GRM(J)=GRM(J)+A*GAD(J)
        END IF
  222  CONTINUE
       IF(M(L+1).EQ.3) THEN
        CALL RETRIV(106,QD(21:24),NNAM,LC,LD)   ! NNAMへの変換機能のみ利用
        M(L+2)=NNAM
        DO 226 J=0,9
  226   X(L+16+J)=A*GTR(J)
       ELSE
        CALL CPARAM(2,GTR,P)
        X(L+3)=A*P(1)
        X(L+4)=A*P(2)
        X(L+5)=A*P(3)
        X(L+6)=A*P(4)
        X(L+7)=P(5)
        X(L+8)=A*P(6)
        X(L+9)=A*P(7)
        X(L+10)=P(8)
       END IF
       X(L+11)=0.
       X(L+12)=0.
       X(L+13)=0.
       X(L+14)=0.
       X(L+15)=W1
*
       L=L+LSZSPC(2)
      END IF
*
  223 GO TO 202
*
***          2.12. 'GWAL' DATA ***************************************
*
  230 CONTINUE
      CALL DCHECK(QD,843,NERR)
*
      CALL ARITH(QD,A,NERR)
      ARM=ARM+A
*
      CALL RETRIV(102,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
       GO TO 233
      END IF
      NL=M(LC+2)
      W=2.
      DO 231 I=1,NL-1
      L1=LC+2*I
      MT(I)=M(L1+1)
      TH(I)=X(L1+2)
  231 W=W-TH(I)
      MT(NL)=M(LC+2*NL+1)
      TH(NL)=W
      CALL GVECTR('GWAL',NL,MT,TH,HT,0.,GTR,GAD)
      DO 232 J=0,9
  232 GRM(J)=GRM(J)+A*(GAD(J)-GTR(J))
*
  233 GO TO 202
*
***          2.13. 'BECO' DATA ***************************************
*
  240 CONTINUE
      CALL DCHECK(QD,844,NERR)
*
      CALL ARITH(QD,A,NERR)
*
      CALL RETRIV(102,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
       GO TO 243
      END IF
*
      READ(QD(27:38),'(2F6.0)') U1,U2
      U0=2.*(U1+U2)
      A=U0*A
      ARM=ARM+A
*
      NL=M(LC+2)
      DO 241 I=1,NL-1
      L1=LC+2*I
      II=2*NL-I
      W=U1*U2
      U1=U1-2.*X(L1+2)
      U2=U2-2.*X(L1+2)
      MT(I)=M(L1+1)
      TH(I)=(W-U1*U2)/U0
      MT(II)=MT(I)
      TH(II)=TH(I)
  241 CONTINUE
      MT(NL)=M(LC+2*NL+1)
      TH(NL)=U1*U2/U0
      CALL GVECTR('BECO',2*NL-1,MT,TH,HT,HT,GTR,GAD)
      DO 242 J=0,9
  242 GRM(J)=GRM(J)+A*(GAD(J)-GTR(J))
*
  243 GO TO 202
*
***          2.14. 'WNDW' DATA ***************************************
*
  250 CONTINUE
      CALL DCHECK(QD,848,NERR)
*
      M(L)=3
      CALL ARITH(QD,A,NERR)
      ARM=ARM+A
*
      IF((QD(6:9).EQ.'    ').OR.(QD(6:9).EQ.'SNGL')) THEN
       ITB=1   ! テーブル番号
       IAP=0   ! IAP=0:普通,1:AFW,2:PPW
      ELSE IF(QD(8:9).EQ.'06') THEN
       ITB=2
      ELSE IF(QD(8:9).EQ.'12') THEN
       ITB=3
      ELSE IF(QD(6:9).EQ.'DLBT') THEN
       ITB=4
       IAP=0
      ELSE IF(QD(6:9).EQ.'AFWN') THEN
       ITB=4
       IAP=1
      ELSE
       CALL ERROR(52,NERR)
      END IF
      IF((ITB.EQ.2).OR.(ITB.EQ.3)) THEN
       IF(QD(6:7).EQ.'DL') THEN
        IAP=0
       ELSE IF(QD(6:7).EQ.'PP') THEN
        IAP=2
       ELSE
        CALL ERROR(52,NERR)
       END IF
      END IF
      READ(QD(15:17),'(I3)') M1   ! 品種番号
      IF(QD(18:20).EQ.'   ') QD(18:20)='  0'
      READ(QD(18:20),'(I3)') M2   ! ブラインド番号
      IF(ABS(GLK(M1,0,ITB)-9.999).LT.0.001) CALL ERROR(53,NERR)
      IF(ABS(GLC(M1,0,ITB)-9.999).LT.0.001) CALL ERROR(53,NERR)
      IF(ABS(GLR(M1,0,ITB)-9.999).LT.0.001) CALL ERROR(53,NERR)
      IF(ABS(GLK(M1,M2,ITB)-9.999).LT.0.001) CALL ERROR(53,NERR)
      IF(ABS(GLC(M1,M2,ITB)-9.999).LT.0.001) CALL ERROR(53,NERR)
      IF(ABS(GLR(M1,M2,ITB)-9.999).LT.0.001) CALL ERROR(53,NERR)
      X(L+2)=A*GLK(M1,0,ITB)
      X(L+3)=A*GLC(M1,0,ITB)
      X(L+4)=A*GLR(M1,0,ITB)
      X(L+5)=A*GLK(M1,M2,ITB)
      X(L+6)=A*GLC(M1,M2,ITB)
      X(L+7)=A*GLR(M1,M2,ITB)
*
      X(L+38)=IAP
      X(L+45)=GLKR(MGT(M1,ITB))
      IF(ABS(X(L+45)-9.999).LT.0.001) CALL ERROR(53,NERR)
      IF((ITB.EQ.4).OR.(M2.EQ.0)) THEN   ! 室内側にブラインドなし
       X(L+46)=X(L+45)   ! ブラインド開時のkLRと同じ
      ELSE
       IF(ABS(GLKRB-9.999).LT.0.001) CALL ERROR(53,NERR)
       X(L+46)=GLKRB
      END IF
      IF(IAP.GE.1) THEN
       IF(QD(21:26).EQ.'      ') QD(21:26)='     0'
       READ(QD(21:26),'(F6.0)') W1   ! 窓通気量
       W1=W1/3.6   ! [m3/m2h]から[L/m2s]への変換
       IF(QD(27:29).EQ.'   ') QD(27:29)=' 40'
       READ(QD(27:29),'(F3.0)') W2   ! 窓排気率
       W2=W2*0.01
       DO 258 II=1,2   ! ΔSC,ΔUループ
       DO 258 I=1,2    ! ブラインド開、閉ループ
        I1=(IAP-1)*2+II
        IF(I.EQ.1) THEN
         M3=0
        ELSE
         M3=M2
        END IF
        IF(ABS(GLD(1,0,0,I1)-9.999).LT.0.001) CALL ERROR(53,NERR)
        IF(ABS(GLD(1,0,0,I1+2)-9.999).LT.0.001) CALL ERROR(53,NERR)
        IF(ABS(GLD(1,M3,MGT(M1,ITB)/10,I1)-9.999).LT.0.001)
     -   CALL ERROR(53,NERR)
        IF(ABS(GLD(1,M3,MGT(M1,ITB)/10,I1+2)-9.999).LT.0.001)
     -   CALL ERROR(53,NERR)
        GLWK(I,II)=A*DLTGL(IAP,W1,W2,
     -         NVS(I1),GLD(1,0,0,I1),GLD(1,M3,MGT(M1,ITB)/10,I1),
     -         NVS(I1+2),GLD(1,0,0,I1+2),GLD(1,M3,MGT(M1,ITB)/10,I1+2))
        ! A*ΔSC, A*ΔU
  258  CONTINUE
       IF(IAP.EQ.1) THEN   ! AFW
        X(L+41)=GLWK(1,1)*X(L+45)
        X(L+44)=GLWK(2,1)*X(L+46)
       ELSE   ! PPW
        X(L+41)=0.0
        IF(M2.EQ.0) THEN
         X(L+44)=0.0
        ELSE
         I1=3
         IF(ABS(GLKRBO-9.999).LT.0.001) CALL ERROR(53,NERR)
         X(L+44)=(GLKRBO-GLKRB)*X(L+6)/(1-GLKRB)
     -   +GLKRBO*A*DLTGL(IAP,W1,1.0,
     -         NVS(I1),GLD(1,0,0,I1),GLD(1,M2,MGT(M1,ITB)/10,I1),
     -         NVS(I1+2),GLD(1,0,0,I1+2),GLD(1,M2,MGT(M1,ITB)/10,I1+2))
        END IF
       END IF
       DO 259 I=1,2   ! ブラインド開、閉ループ
        X(L+39+(I-1)*3)=GLWK(I,2)
        X(L+40+(I-1)*3)=GLWK(I,1)-X(L+41+(I-1)*3)
  259  CONTINUE
      END IF
      X(L+8)=A*X(158)
      DO 251 J=0,9
  251 GRM(J)=GRM(J)+X(L+5)
*
      CALL RETRIV(100,QD(10:13),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
       GO TO 253
      END IF
      M(L+1)=LC
      V1=X(LC+26)
      V2=X(LC+11)
      V4=X(LC+27)
      IF(X(LC+12).EQ.0.) THEN
       V3=0.
      ELSE
       W=SQRT(X(LC+12)**2+(X(LC+13)-X(LL+5))**2)
       W=(X(LC+6)*X(LC+12)-X(LC+5)*(X(LC+13)-X(LL+5)))/W
       V3=(1.-W)/2.
      END IF
*
      IF(V2.GT.V3) THEN
       IF(V1+V2.GT.1.) THEN
        U1=0.
        U2=(1.-V1-V3)*(1.-V4)
       ELSE
        U1=(1.-V1-V2)*(1.-V4)
        U2=(V2-V3)*(1.-V4)
       END IF
      ELSE
       IF(V1+V3.GT.1.) THEN
        U1=0.
        U2=0.
       ELSE
        U1=(1.-V1-V3)*(1.-V4)
        U2=0.
       END IF
      END IF
*
  253 X(L+9)=0.808*X(154)*U2
      X(L+11)=0.808*U1
      X(L+10)=X(L+9)+X(L+11)
      X(L+12)=0.9*U1/HO
*
      IF(X(LC+12).EQ.0.) THEN
       X(L+13)=0.
       X(L+14)=0.
      ELSE
       W=(X(LC+13)-X(LL+5))/X(LC+12)
       X(L+13)=W*X(LC+3)
       X(L+14)=W*X(LC+4)
      END IF
*
      IF(X(LL+43).EQ.0.) THEN
       DO 252 I=15,19
  252  X(L+I)=0.
      ELSE
       READ(QD(30:41),'(2F6.0)') U,W
       V=A/W
       IF(U.LT.0.75) V=V+U-0.75
       V1=109.*0.5*(1.-X(LL+44)/SQRT(X(LL+44)**2+V**2))
       V2=109.*A*X(LL+45)
       V3=109.*A*X(LL+46)
       X(L+15)=V1
       X(L+16)=V2
       X(L+17)=V3
       X(L+18)=V1+(V2+V3)/2.
       X(L+19)=X(LL+44)*W/X(LL+2)
      END IF
      DO 254 I=0,5
  254 M(L+20+I*3)=0   ! デフォルトで物性値は品種番号の物性値のとおり
      READ(NUB,'(A80)') QD
      IF(QD(1:4).EQ.'+   ')THEN   ! 継続行
       WRITE(6,'(1X,A80)') QD
       CALL DCHECK(QD,1302,NERR)
       DO 255 II=0,1   ! ブラインド開閉ループ
        DO 256 I=0,2   ! K,SCC,SCRループ
         K1=10+II*30+I*9   ! 入力欄のカラム位置
         L1=L+20+II*9+I*3  ! XMQ配列の添字（スケジュールオプション）
         IF(QD(K1:K1+3).EQ.'    ')THEN
          M(L1)=1   ! on時%の値を使用
          IF(QD(K1+5:K1+7).EQ.'   ') QD(K1+5:K1+7)='100'
          READ(QD(K1+5:K1+7),'(F3.0)') W
          X(L1+2)=0.01*W
         ELSE
          M(L1)=2   ! DSCH使用
          CALL RETRIV(104,QD(K1:K1+3),NNAM,LC,LD)
          IF(LD.NE.LC) THEN
           CALL ERROR(5,NERR)
          ELSE
           M(L1+1)=LC+1
          END IF
         END IF
  256   CONTINUE
  255  CONTINUE
      ELSE   ! 継続行ではない
       BACKSPACE(NUB)
      END IF
*
      L=L+LSZSPC(3)
      GO TO 202
*
***          2.15. 'INFL' DATA ***************************************
*
  260 CONTINUE
      CALL DCHECK(QD,1327,NERR)
*
      M(L)=4
      CALL RETRIV(100,QD(10:13),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
      ELSE
       M(L+1)=LC
      END IF
*
      READ(QD(15:17),'(I3)') M(L+2)
      IF(M(L+2).EQ.0) THEN
       IF(QD(21:26).EQ.'      ') QD(21:26)='   5.0'
       READ(QD(21:26),'(F6.0)') W
      CALL ARITH(QD,A,NERR)
       X(L+3)=W*A
      ELSE
       READ(QD(21:26),'(F6.0)') W
       X(L+3)=W*X(LL+2)*X(LL+4)
      END IF
      IF(QD(28:31).EQ.'    ') THEN
       M(L+4)=0   ! オリジナル換気量で一定、あるいは空調on・off時%の値を使用
       IF(QD(33:35).EQ.'   ') QD(33:35)='100'
       IF(QD(36:38).EQ.'   ') QD(36:38)='100'
       READ(QD(33:38),'(2F3.2)') X(L+6),X(L+7)
      ELSE
       M(L+4)=1   ! DSCH使用
       CALL RETRIV(104,QD(28:31),NNAM,LC,LD)
       IF(LD.NE.LC) THEN
        CALL ERROR(5,NERR)
       ELSE
        M(L+5)=LC+1
       END IF
      END IF
*
      L=L+LSZSPC(4)
      GO TO 202
*
***          2.16. 'LIGH' DATA ***************************************
*
  270 CONTINUE
      CALL DCHECK(QD,863,NERR)
*
      CALL RETRIV(104,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
      ELSE
       M(LL+35)=LC+1
      END IF
*
      IF(QD(15:17).EQ.'   ') QD(15:17)='  1'
      IF(QD(18:23).EQ.'      ') QD(18:23)='    20'
      IF(QD(24:26).EQ.'   ') QD(24:26)='  1'
      READ(QD(15:26),'(I3,F6.0,I3)') M1,W,M2
      IF(M2.EQ.1) W=0.001*W*X(LL+2)
      IF(M1.LE.3) W=1.15*W
      W=860.*W
      M1=MOD(M1-1,3)+1
      X(LL+36)=FL(1,M1)*W
      X(LL+37)=FL(2,M1)*W
      X(LL+38)=FL(3,M1)
      X(LL+39)=FL(4,M1)*W
      X(LL+40)=FL(5,M1)
*
      IF(X(LL+43).NE.0) THEN
       IF(QD(33:38).EQ.'      ') QD(33:38)='   700'
       READ(QD(33:38),'(F6.0)') W
       X(LL+43)=W/2.
      END IF
*
      GO TO 202
*
***          2.17. 'OCUP' DATA ***************************************
*
  280 CONTINUE
      CALL DCHECK(QD,874,NERR)
*
      CALL RETRIV(104,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
      ELSE
       M(LL+51)=LC+1
      END IF
*
      IF(QD(15:17).EQ.'   ') QD(15:17)='  3'
      IF(QD(18:23).EQ.'      ') QD(18:23)='   0.2'
      IF(QD(24:26).EQ.'   ') QD(24:26)='  1'
      READ(QD(15:26),'(I3,F6.0,I3)') M1,W,M2
      IF(M2.EQ.1) W=W*X(LL+2)
      X(LL+52)=W*AM(1,M1)
      X(LL+53)=W*AM(2,M1)
      X(LL+54)=W*AM(3,M1)
*
      GO TO 202
*
***          2.18. 'HEAT' DATA ***************************************
*
  290 CONTINUE
      CALL DCHECK(QD,1191,NERR)
*
      CALL RETRIV(104,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
      ELSE
       M(LL+47)=LC+1
      END IF
*
      IF(QD(15:17).EQ.'   ') QD(15:17)='  1'
      IF(QD(30:32).EQ.'   ') QD(30:32)='  1'
      READ(QD(15:32),'(I3,2F6.0,I3)') M1,W,X(LL+50),M2
      IF(M2.EQ.1)THEN
       W=0.001*W*X(LL+2)
       X(LL+50)=0.001*X(LL+50)*X(LL+2)
      END IF
      W=860.*W
      X(LL+50)=860.*X(LL+50)
      IF(M1.EQ.1) THEN
       X(LL+48)=HC*W/HT
       X(LL+49)=HR*W/HT
      ELSE
       X(LL+48)=W
       X(LL+49)=0.
      END IF
*
      GO TO 202
*
***          2.19. 'FURN' DATA ***************************************
*
  300 CONTINUE
      CALL DCHECK(QD,890,NERR)
*
      IF(QD(15:20).EQ.'      ') QD(15:20)='    40'
      IF(QD(21:26).EQ.'      ') QD(21:26)='    80'
      READ(QD(15:26),'(2F6.0)') W1,W2
      W1=W1*X(LL+2)/4.186
      W2=W2*X(LL+2)/4.186
      IF(ABS(W1).GT.0.1) IFURS=1
*
      CALL GVECTR('FURS',0,MT,TH,HT,W1,GTR,GAD)
      DO 303 J=0,9
  303 GAS(J)=GAS(J)+GAD(J)
      CALL GVECTR('FURN',0,MT,TH,HT,W2,GTR,GAD)
      DO 302 J=0,9
  302 GRL(J)=GRL(J)+GAD(J)
*
      GO TO 202
*
***                "SOPC" DATA            ****************************
*
 1310 CONTINUE
      CALL DCHECK(QD,966,NERR)
*
      CALL RETRIV(145,QD(6:9),NNAM,LC,LD)
      IF(LD.NE.LC) THEN
       CALL ERROR(5,NERR)
      ELSE
       M(LL+55)=LC
      END IF
      READ(QD(12:35),'(4F6.0)') ( X(LL+56+I),I=0,3 )
      DO 1312 I=0,3
 1312 X(LL+56+I)=0.860*X(LL+56+I)
      IF(QD(38:41).NE.'    ') THEN
       CALL RETRIV(98,QD(38:41),NNAM,LC,LD)
       IF(LD.NE.LC) THEN
        CALL ERROR(5,NERR)
       ELSE
        M(LL+202)=LC
       END IF
      END IF
      DO 1313 II=0,2 ! 季節ループ                                                 add 20200403(T.Nagai)
        QWK(1:4) = 'CDHS'                                                       ! add 20200403(T.Nagai)
        ! 装置容量の設定                                                          add 20200403(T.Nagai)
        DO 1314 I=0,3 ! CDHSループ                                                add 20200403(T.Nagai)
          J=41+4*II+I+1                                                         ! add 20200403(T.Nagai)
          IF((QD(J:J).EQ.' ').OR.(QD(J:J).EQ.QWK(I+1:I+1))) THEN                ! add 20200403(T.Nagai)
            X(LL+203+4*II+I) = X(LL+56+I)                                       ! add 20200403(T.Nagai)
          ELSE IF(QD(J:J).EQ.'-') THEN                                          ! add 20200403(T.Nagai)
            QWK(I+1:I+1) = '-'                                                  ! add 20200403(T.Nagai)
            X(LL+203+4*II+I) = 0.0                                              ! add 20200403(T.Nagai)
          ELSE                                                                  ! add 20200403(T.Nagai)
            CALL ERROR(55,NERR)                                                 ! add 20200403(T.Nagai)
          END IF                                                                ! add 20200403(T.Nagai)
 1314   CONTINUE                                                                ! add 20200403(T.Nagai)
        ! 人体発熱顕熱比率算出用温度                                              add 20200403(T.Nagai)
        IF(MCNTL(32).EQ.0) THEN                                                 ! add 20200403(T.Nagai)
          X(LL+215+II)=X(155)                                                   ! add 20200403(T.Nagai)
        ELSE IF((QWK(1:1).EQ.'C').AND.(QWK(3:3).EQ.'-')) THEN                   ! add 20200403(T.Nagai)
          X(LL+215+II)=X(M(LL+55)+2+4*II)                                       ! add 20200403(T.Nagai)
        ELSE IF((QWK(1:1).EQ.'-').AND.(QWK(3:3).EQ.'H')) THEN                   ! add 20200403(T.Nagai)
          X(LL+215+II)=X(M(LL+55)+3+4*II)                                       ! add 20200403(T.Nagai)
        ELSE                                                                    ! add 20200403(T.Nagai)
          X(LL+215+II)=0.5*(X(M(LL+55)+2+4*II) + X(M(LL+55)+3+4*II))            ! add 20200403(T.Nagai)
        END IF                                                                  ! add 20200403(T.Nagai)
 1313 CONTINUE                                                                  ! add 20200403(T.Nagai)
*
      GO TO 202
*
***          2.20. SPACE WEIGHTING FACTOR ****************************
*
  310 CONTINUE
      M(L)=5
      X(LL+63)=ARM
*
      DO 311 J=0,9
  311 G(J)=(HC*ARM-FC*GRM(J))/(HC*ARM+FR*GRM(J))
      CALL CPARAM(1,G,P)
      X(LL+8)=P(1)
      X(LL+9)=P(3)
      X(LL+10)=P(5)
*
      DO 312 J=0,9
  312 G(J)=HC*ARM*GRM(J)/(HC*ARM+FR*GRM(J))+GAS(J)
      CALL CPARAM(2,G,P)
      DO 313 I=1,8
  313 X(LL+I+14)=P(I)
*
      CALL CPARAM(1,GRL,P)
      DO 314 I=1,5
  314 X(LL+I+24)=P(I)
*
      L2=LL+LSZSPC(0)
  321 CALL RTVADJ(LSZSPC,L2,JZ,ISTAT2)
      IF((ISTAT2.EQ.1).OR.(ISTAT2.EQ.-2)) THEN
       DO 322 J=0,9
  322  G(J)=HC*ARM/(HC*ARM+FR*GRM(J))*X(L2+16+J)
       CALL CPARAM(2,G,P)
       DO 323 I=1,8
  323  X(L2+I+2)=P(I)
       L2=L2+LSZSPC(M(L2))
       GO TO 321
      END IF
      IF(IFURS.GE.1) THEN
       CALL CPARAM(2,GAS,P)   ! 家具の蓄熱応答係数のみ計算（簡易MRT計算用）
       DO 318 I=1,8
  318  X(LL+I+63)=P(I)
      END IF
*
      M(LL+101)=IZ
      IF(QKY.EQ.':   ')THEN   ! スペースの結合(グループの継続)が指示された場合
       IZ=IZ+1
       IF(IZ.GT.NAZ) CALL ERROR(41,NERR)
      ELSE IF(QKY.EQ.'CFLW')THEN   ! 空気移動量が指定された場合
                                   ! (現在のグループのうち最後のスペースのはず)
  316  CALL DCHECK(QD,1201,NERR)
       IF(QD(10:13).EQ.'    ') THEN   ! DSCH名を引用していない
        MFLWK(1)=2
        IF(QD(15:17).EQ.'   ') QD(15:17)='100'
        IF(QD(18:20).EQ.'   ') QD(18:20)='100'
        READ(QD(15:20),'(2F3.2)') XFLWK(1), XFLWK(2)
       ELSE   ! DSCH名引用
        MFLWK(1)=1
        CALL RETRIV(104,QD(10:13),NNAM,LC,LD)
        IF(LD.NE.LC) THEN
         CALL ERROR(5,NERR)
        ELSE
         MFLWK(2)=LC+1
        END IF
       END IF
       IF(QD(21:26).EQ.'      ') QD(21:26)='   150'
       IF(QD(36:38).EQ.'   ') QD(36:38)='  0'
       IF(QD(39:44).EQ.'      ') QD(39:44)='     0'
       IF(QD(54:56).EQ.'   ') QD(54:56)='  0'
       IF(QD(57:62).EQ.'      ') QD(57:62)='     0'
       IF(QD(72:74).EQ.'   ') QD(72:74)='  0'
       IF(QD(75:80).EQ.'      ') QD(75:80)='     0'
       READ(QD(21:26),'(F6.0)') V1
       DO 317 II=1,3
        IF(QD(18*II+10:18*II+13).EQ.'    ')THEN
         GO TO 317
        ELSE
         CALL RETRIV(LCGB,QD(18*II+10:18*II+13),NNAM,LC1,LD)
         IF(LD.NE.LC1)THEN
          CALL ERROR(42,NERR)
          GO TO 317
         END IF
        END IF
        CALL RETRIV(LCGB,QD(18*II+14:18*II+17),NNAM,LC2,LD)
        IF(LD.NE.LC2)THEN
         CALL ERROR(42,NERR)
         GO TO 317
        END IF
        IF(LC1.EQ.LC2) CALL ERROR(43,NERR)
        READ(QD(18*II+18:18*II+26),'(I3,F6.0)') I, V2
        IF((I.EQ.0).OR.(I.EQ.1))THEN
         L1=LC2+101+(M(LC1+101)-1)*5
         X(L1+1)=V1*V2
         M(L1+2)=MFLWK(1)
         IF(MFLWK(1).EQ.2) THEN
          X(L1+4)=XFLWK(1)
          X(L1+5)=XFLWK(2)
         ELSE
          M(L1+3)=MFLWK(2)
         END IF
        END IF
        IF((I.EQ.0).OR.(I.EQ.2))THEN
         L1=LC1+101+(M(LC2+101)-1)*5
         X(L1+1)=V1*V2
         M(L1+2)=MFLWK(1)
         IF(MFLWK(1).EQ.2) THEN
          X(L1+4)=XFLWK(1)
          X(L1+5)=XFLWK(2)
         ELSE
          M(L1+3)=MFLWK(2)
         END IF
        END IF
  317  CONTINUE
       READ(NUB,'(A80)') QD
       WRITE(6,'(1X,A80)') QD
       QKY=QD(1:4)
       IF(QKY.EQ.'+   ')THEN
        GO TO 316   ! 複数行「CFLW」データを指定することが可能
       ELSE IF(QKY.NE.'    ')THEN
        CALL ERROR(44,NERR)
       END IF
      ELSE   ! グループの終了
       IF(QKY.NE.'    ') CALL ERROR2(80,2)
      END IF

      IF(QKY.EQ.'    ') THEN
        IF(IZ.GE.2) THEN
        ! IWAL(adjacent)の参照error check
         L1=LCGB
         DO 319 I=1,IZ
          L2=L1+LSZSPC(0)
  320     CALL RTVADJ(LSZSPC,L2,JZ,ISTAT2)
          IF(ISTAT2.EQ.-2) THEN
           CALL ERROR(5,NERR)
          ELSE IF(ISTAT2.EQ.-1) THEN
           CALL ERROR2(81,2)
          ELSE IF((ISTAT2.EQ.0).AND.(I.NE.IZ)) THEN
           L1=M(L1)
          ELSE IF(ISTAT2.EQ.1) THEN
           L2=L2+LSZSPC(M(L2))
           GO TO 320
          END IF
  319    CONTINUE
        END IF
        ! 次のグループのためのセット
        IZ=1
        LCGB=L+1
      END IF
*
      L=L+1
      GO TO 204
*
***          2.21. OUTPUT 1 (JOB NAME AND SPACE WF) ******************
*
  400 CONTINUE
      IF(NERR.GT.0) THEN
       WRITE(6,76) NERR
   76  FORMAT(1H0,'JOB INTERUPTED AT END OF PRELIMINARY PROCESS'/1H ,'ER
     *R COUNT=',I3)
       STOP
      END IF
*
      IF(IACSS.EQ.1) THEN
        WRITE(NUO,'(3A)') ' ----- NewHASP/ACLD/', QVER, ' -----'
        WRITE(NUO,'(A)') QJB
        WRITE(NUO,'(6I5,2E15.7,I5)') MCNTL(7),MCNTL(8),MCNTL(10),
     *                MCNTL(11),MCNTL(13),MCNTL(14), X(155), X(157), NRM
        WRITE(NUO,'(20I5)') (M(I),I=171,190)
      END IF
      IF(IBECS.EQ.1) THEN
        WRITE(NUOB,'(3A/A)')
     -         '.    NewHASP/ACLD ', QVER, '                        ',
     -         '.                                                 '
        WRITE(NUOB,'(A)') QJB
        WRITE(NUOB,'(9I5)') (MCNTL(I), I=6,14)
        WRITE(NUOB,'(I5)') NRM
      END IF
*
      LL=M(106)
      NRM=0
  410 IF(LL.EQ.0) GO TO 420
      NRM=NRM+1
*
      WF(1,1)=X(LL+15)
      WF(1,2)=X(LL+16)
      U1=X(LL+17)
      V1=X(LL+18)
      R1=X(LL+19)
      U2=X(LL+20)
      V2=X(LL+21)
      R2=X(LL+22)
      DO 402 J=2,8
      WF(J,1)=U1+U2
      WF(J,2)=V1+V2
      U1=U1*R1
      V1=V1*R1
      U2=U2*R2
      V2=V2*R2
  402 CONTINUE
      WF(9,1)=(U1/(1.-R1)+U2/(1.-R2))/(U1/((1.-R1)*R1)+U2/((1.-R2)*R2))
      WF(9,2)=(V1/(1.-R1)+V2/(1.-R2))/(V1/((1.-R1)*R1)+V2/((1.-R2)*R2))
*
      WF(1,3)=X(LL+25)
      WF(1,4)=X(LL+26)
      U1=X(LL+27)
      V1=X(LL+28)
      DO 403 J=2,8
      WF(J,3)=U1
      WF(J,4)=V1
      U1=U1*X(LL+29)
      V1=V1*X(LL+29)
  403 CONTINUE
      WF(9,3)=X(LL+29)
      WF(9,4)=X(LL+29)
      CALL NAME(QSP,M(LL+1))
*
      IF(IACSS.EQ.1) THEN
        WRITE(NUO,'(A,36E15.7)') QSP, WF
      END IF
      IF(IBECS.EQ.1) THEN
        WRITE(NUOB,'(A,26X,37E15.7)') QSP, X(LL+2), WF
      END IF
*
      IF ( M(LL+55).NE.0 ) THEN
       DO 408 I=1,4
  408  IF(QSP(I:I).EQ.' ') QSP(I:I)='_'
       OPEN(NUOT-1+NRM,FILE=QPATH(1:LEN_TRIM(QPATH))//QSP//'.csv')
       WRITE(NUOT-1+NRM,'(A)') 'YEAR,MO,DY,YB,HR,,ROOM-T,CLOD-S,RHEX-S,A
     -HEX-S,FS,ROOM-H,CLOD-L,RHEX-L,AHEX-L,FL,MRT'''
      END IF
*
      LL=M(LL)
      GO TO 410
*     外調機出力ファイルオープン
  420 LL=M(98)
      NUOT2=NUOT+NRM   ! 外調機出力先装置番号（先頭）
      II=NUOT2
  421 IF(LL.EQ.0) GO TO 500
      CALL NAME(QSP,M(LL+1))
      DO 422 I=1,4
  422 IF(QSP(I:I).EQ.' ') QSP(I:I)='_'
      OPEN(II,FILE=QPATH(1:LEN_TRIM(QPATH))//'OHU-'//QSP//'.csv')
      WRITE(II,'(A)') 'YEAR,MO,DY,YB,HR,,HEX-T,COIL-T,HEX-S,COIL-S,HEX-H
     -,COIL-H,HEX-L,COIL-L,V'
      II=II+1
      LL=M(LL)
      GO TO 421
*
*****       3. MAIN PROCESS ******************************************
***          3.1. DATING AND JOB CONTROL *****************************
*
  500 CONTINUE
*
      ICYCL=1
  502 CALL INWD(NUW,MCNTL(5)/2,MCNTL(31),ICYCL,WDND,IDND,ISTAT)                 ! rev 20200403(T.Nagai)
      IF(MCNTL(5).NE.1)THEN
       KDYF=NDATF(IDND(1,1),IDND(1,2),IDND(1,3))   ! 1899/12/31を1とした通算日数
       IF(((MCNTL(5).EQ.0).AND.(ICYCL.EQ.2)).OR.
     -    ((MCNTL(5).EQ.2).AND.(KDYF.GT.MCNTL(19))))THEN
        CALL ERROR2(100,1)
       ELSE IF(((MCNTL(5).EQ.0).AND.(KDYF.NE.MCNTL(19))).OR.
     -         ((MCNTL(5).EQ.2).AND.(KDYF.LT.MCNTL(19))))THEN
        GO TO 502
       END IF
      END IF
      MODE=1
      KWK=0
*
  501 IF(MODE.EQ.3) GOTO 700
      DO 503 I=1,7
      DO 505 J=1,24
  505 WD(I,J)=WDND(I,J)
      DO 503 J=1,5
  503 ID(I,J)=IDND(I,J)
      KDY=NDATE(ID(1,2),ID(1,3))
      KDYF=NDATF(ID(1,1),ID(1,2),ID(1,3))
      MDW(1)=MKMDW(ID)
      ISEAS(1)=M(980+ID(1,2))
      IDWK(1)=ID(1,1)   ! 今日の年
      IDWK(2)=ID(1,2)   ! 今日の月
      IDWK(3)=ID(1,3)   ! 今日の日
*
      ICYCLO=ICYCL
      CALL INWD(NUW,MCNTL(5)/2,MCNTL(31),ICYCL,WDND,IDND,ISTAT)                 ! rev 20200403(T.Nagai)
      IF(ISTAT.EQ.0)THEN  ! 実在気象データの最終日＝計算最終日
       IF(KDYF.NE.MCNTL(21)) CALL ERROR2(101,1)
       MDW(2)=MDW(1)  ! 計算最終日の翌日の季節・曜日 ＝ 最終日
       ISEAS(2)=ISEAS(1)
      ELSE
       MDW(2)=MKMDW(IDND)
       ISEAS(2)=M(980+IDND(1,2))
      END IF
*
      IF(MODE.EQ.1) THEN
       IF(MCNTL(5).EQ.1)THEN
        IF(ICYCLO.EQ.MCNTL(15)) MODE=2
       ELSE
        IF(KDYF.EQ.MCNTL(20)) MODE=2
       END IF
      END IF
*
      IF(MODE.EQ.2) THEN
       IF(MCNTL(5).EQ.1)THEN
        IF(ICYCL.EQ.MCNTL(15)+1) MODE=3
       ELSE
        IF(KDYF.EQ.MCNTL(21)) MODE=3
       END IF
      END IF
*
   80 FORMAT(' DATE:',2I3)
*
***          3.2 WEATHER DATA ****************************************
*
      DO 504 J=1,24
      IF(MCNTL(31).EQ.3) THEN                                                   ! add 20200403(T.Nagai)
        WD(1,J)=0.1*(WD(1,J)-500.)
      ELSE                                                                      ! add 20200403(T.Nagai)
        WD(1,J)=0.1*WD(1,J)                                                     ! add 20200403(T.Nagai)
      END IF                                                                    ! add 20200403(T.Nagai)
      WD(2,J)=0.1*WD(2,J)
      IF(MCNTL(3).EQ.0)THEN
       WD(5,J)=4.88*(0.01*(WD(1,J)+273.16))**4
     *    *(1.-0.062*WD(5,J))*(0.49-2.1*SQRT(WD(2,J)/(WD(2,J)+622.)))
      END IF
      IF(MCNTL(4).EQ.0)THEN
       WD(3,J)=WD(3,J)/.4186
       WD(4,J)=WD(4,J)/.4186
       IF(MCNTL(3).EQ.1) WD(5,J)=WD(5,J)/.4186
      ELSE IF(MCNTL(4).EQ.2)THEN                                                ! add 20200403(T.Nagai)
       WD(3,J)=WD(3,J)/4.186                                                    ! add 20200403(T.Nagai)
       WD(4,J)=WD(4,J)/4.186                                                    ! add 20200403(T.Nagai)
       IF(MCNTL(3).EQ.1) WD(5,J)=WD(5,J)/4.186                                  ! add 20200403(T.Nagai)
      END IF
      WD(6,J)=(WD(6,J)-8.)*22.5
      WD(7,J)=0.1*WD(7,J)
      WD8(J)=SATX(WD(1,J))-WD(2,J)   ! 飽差（外気飽和絶対湿度－外気絶対湿度)
  504 CONTINUE
*
***          3.3. OUTPUT 2 (WEATHER DATA) ****************************
*
      IF(IACSS.EQ.1) THEN
        WRITE(NUO,'(4I5)') (ID(1,J),J=1,4)
        WRITE(NUO,'(24E15.7)') (WD(1,J),J=1,24)
        WRITE(NUO,'(24E15.7)') (WD(2,J),J=1,24)
      END IF
      IF(IBECS.EQ.1) THEN
        WRITE(NUOB,'(4I5)') (ID(1,I),I=1,3), MDW(1)
        WRITE(NUOB,'(24E15.7)') (WD(1,J),J=1,24)
        WRITE(NUOB,'(24E15.7)') (WD(2,J),J=1,24)
      END IF
*
**           3.4. SOLAR POSITION ******************************
*
      M1=INT((KDY+6)/7)
      IF(M1.EQ.KWK) GOTO 540
      KWK=M1
*
      W=0.0171672*(KDY+3)
      W1=0.362213-23.2476*COS(W+0.153231)-0.336891*COS(2.*W+0.207099)
     *           -0.185265*COS(3.*W+0.620129)
      W2=-0.000279+0.122772*COS(W+1.49831)-0.165458*COS(2.*W-1.26155)
     *           -0.005354*COS(3.*W-1.1571)
      SD=SIN(W1*DR)
      CD=COS(W1*DR)
      ET=(W2+X(152)-12.)
      SS=X(150)*SD
      SC=X(150)*CD
      CS=X(151)*SD
      CC=X(151)*CD
*
      DO 510 J=1,24                                                             ! rev 20200403(T.Nagai)
      W1=15.*(ET+J)*DR
      SH(J)=SS+CC*COS(W1)
      CHSA(J)=CD*SIN(W1)
  510 CHCA(J)=-CS+SC*COS(W1)
*
***          3.5. 'EXPS' UPDATE **************************************
*
      L=M(100)
  520 IF(L.EQ.0) GO TO 540
*
      DO 530 J=1,24                                                             ! rev 20200403(T.Nagai)
*
      IF(SH(J).LE.0.) THEN
       X(L+J+27)=0.                                                             ! rev 20200403(T.Nagai)
       X(L+J+51)=0.                                                             ! rev 20200403(T.Nagai)
       GO TO 530
      END IF
*
      SS=SH(J)*X(L+6)+CHCA(J)*X(L+9)+CHSA(J)*X(L+7)
      IF(SS.LE.0.) THEN
       X(L+J+27)=0.                                                             ! rev 20200403(T.Nagai)
       X(L+J+51)=0.                                                             ! rev 20200403(T.Nagai)
       GO TO 530
      END IF
*
      IF(X(L+14).EQ.0..AND.X(L+19).EQ.0.) THEN
       X(L+J+27)=SS                                                             ! rev 20200403(T.Nagai)
       X(L+J+51)=GF(SS**2)                                                      ! rev 20200403(T.Nagai)
       GO TO 530
      END IF
*
      CS=CHSA(J)*X(L+4)-CHCA(J)*X(L+3)
      CC=-SH(J)*X(L+5)+CHCA(J)*X(L+10)+CHSA(J)*X(L+8)
      U=X(L+19)*ABS(CS)/SS
      V=X(L+14)*ABS(CC)/SS
      IF(U.GE.X(L+23).OR.V.GE.X(L+18)) THEN
       X(L+J+27)=0.                                                             ! rev 20200403(T.Nagai)
       X(L+J+51)=0.                                                             ! rev 20200403(T.Nagai)
       GO TO 530
      END IF
*
      ST=(X(L+23)-U)*(X(L+18)-V)
*
      IF(X(L+25).EQ.0.) THEN
       SG=0.
       GO TO 529
      END IF
*
      IF(CS.LT.0.) THEN
       UG=X(L+22)-U
       IF(UG.GT.X(L+20)) UG=X(L+20)
       IF(UG.LT.0.) UG=0.
      ELSE
       UG=X(L+21)-U
       IF(UG.GT.X(L+20)) UG=X(L+20)
       IF(UG.LT.0.) UG=0.
      END IF
*
      IF(CC.LT.0.) THEN
       VG=X(L+17)-V
       IF(VG.GT.X(L+15)) VG=X(L+15)
       IF(VG.LT.0.) VG=0.
      ELSE
       VG=X(L+16)-V
       IF(VG.GT.X(L+15)) VG=X(L+15)
       IF(VG.LT.0.) VG=0.
      END IF
*
      SG=UG*VG
      X(L+J+51)=GF(SS**2)*SG/X(L+25)                                            ! rev 20200403(T.Nagai)
  529 IF(X(L+24).EQ.0.) THEN  ! 壁面全体面積-窓面積=0                           ! rev 20121213(T.Nagai) Debug
       X(L+J+27)=0.                                                             ! rev 20200403(T.Nagai)
      ELSE                                                                      ! add 20121213(T.Nagai) Debug
       X(L+J+27)=SS*(ST-SG)/X(L+24)                                             ! rev 20200403(T.Nagai)
      END IF                                                                    ! add 20121213(T.Nagai) Debug
  530 CONTINUE
*
      L=M(L)
      GO TO 520
*
***          3.5.5 OAHU PRE-PROCESS **********************************
*
  540 CONTINUE
      LL=M(98)
  541 IF(LL.EQ.0) GO TO 600
      L1=LL+2+(ISEAS(1)-1)*9
      DO 542 II=1,2   ! 顕熱・潜熱ループ
      X(LL+30+(II-1)*25)=X(LL+54+(II-1)*25)   ! 計算初日は0となる
  542 X(LL+80+(II-1)*25)=X(LL+104+(II-1)*25)   ! 計算初日は0となる
      X(LL+130)=X(LL+178)   ! 計算初日は0となる
      DO 543 J=1,24
       DO 544 II=1,2   ! 顕熱・潜熱ループ
*       全熱交出口温湿度
        IF(M(L1+1).GE.II) THEN
         W1=X(LL+2)*X(L1+1+II)+(1.0-X(LL+2))*WD(II,J)
        ELSE
         W1=WD(II,J)
        END IF
        X(LL+30+(II-1)*25+J)=W1
*       外調機出口温湿度
        IF( (M(L1+4).GE.II).AND.(W1.GT.X(L1+6+(II-1)*2)) ) THEN
         W2=X(L1+6+(II-1)*2)
        ELSE IF( (M(L1+5).GE.II).AND.(W1.LT.X(L1+7+(II-1)*2)) ) THEN
         W2=X(L1+7+(II-1)*2)
        ELSE
         W2=W1
        END IF
        X(LL+80+(II-1)*25+J)=W2
  544  CONTINUE
*      風量
       DO 545 I=1,2
  545  X(LL+130+(J-1)*2+I)=0.0   ! 積算風量ゼロクリア
  543 CONTINUE
      LL=M(LL)
      GO TO 541
*
***          3.6. SPACE LOOP START ***********************************
*
  600 CONTINUE
      LC=M(106)
      LCGB=LC
      KSPAC = 0
*
  610 IF((KSPAC.NE.0).AND.(M(LC+101).NE.M(LCO+101)+1))THEN
      ! 新しいグループに移ったとき(最初のグループを除く)
       LC1=LCGB
       LCGB=LC
       IF(M(LCO+55).NE.0)THEN   ! SOPCデータによってOPCOデータを引用した場合
        ! 前のグループのスペース数をカウントするとともに除去熱量計算ルーチンを呼ぶ
        NZ=1
        DO 611 II=1,NAZ
         IF(LC1.EQ.LCO)THEN
          GO TO 614   ! グループ最後のスペースであるのでスペース数のカウント終了
         ELSE
          LC1=M(LC1)
          NZ=NZ+1
         END IF
  611   CONTINUE
  614   II=MIN(2,ISEAS(1))  ! 中間期の場合は'2'（ダミー）
C       除去熱量の計算
        CALL EXTRC2(NHR,MCNTL(1),ISEAS(1),NAZ,IOPTG,NZ,IOPVG,SMRT1,
     -   SMRT2,VOAG,LCG,CLDG,NWD,WD,REFWD,P0,M(LOPC+165+II),VFLOW,EXCAP,
     -   SPCAP,RMMX,RMMN,10,NUOT+KSPAC-NZ,IDWK,MDW(1),MODE,MCNTL(2),
     -   LSZSPC,IBECS,NUOB)
       END IF
      END IF
*
      IF(LC.EQ.0) THEN   ! 全スペース終了
*      外調機出力
       CALL OAHUOUT(NHR,M(98),ISEAS(1),MCNTL(2),NUOT2,NWD,WD,IDWK,
     -              MDW(1),MODE)
*      外気温湿度出力
       CALL WEOUT(NHR,NWD,WD,IDWK,MDW(1),MODE,MCNTL(2),NUOW)
       GO TO 501
      END IF
      KSPAC = KSPAC + 1
*
      M1=M(LC+34)
      KSCH(1)=M(M1+MDW(1))
      L1=(KSCH(1)-1)*24
      LL=M(LC+35)+L1
      LH=M(LC+47)+L1
      LO=M(LC+51)+L1
      IF(M(LC+35).EQ.0) LL=0
      IF(M(LC+47).EQ.0) LH=0
      IF(M(LC+51).EQ.0) LO=0
*
      LOPC=M(LC+55)
      IF(LOPC.NE.0) THEN
       KSCH(2)=M(M1+MDW(2))
      END IF
*
***          3.7. HOURLY LOOP START **********************************
*
      DO 670 J=1,24
      ACC1=0.
      ACC2=0.
      ACC3=0.
      ACC4=0.
      ACC5=0.
      ACC6=0.
*
      X(LC+74)=0.
      X(LC+75)=0.
      L=LC+LSZSPC(0)
      IF ( LOPC.NE.0 ) THEN
       CALL EXTRC0(J,LOPC,LC,ISEAS,KSCH,IOPTWK)   ! 空調運転状態(IOPTWK,M(LC+60))
      END IF
*
  612 GO TO (620,630,640,650,660),M(L)
*
***          3.8. HEAT GAIN THROUGH OUTSIDE WALL **********************
*
  620 CONTINUE
      LE=M(L+1)
      IF(CHCA(J)*X(LE+4)+CHSA(J)*X(LE+3).LT.0.) THEN
        W=WD(3,J)
      ELSE
        IF(X(L+13)*CHSA(J)+X(L+14)*CHCA(J).LT.SH(J)) THEN
          W=WD(3,J)
        ELSE
          W=0.
        END IF
      END IF
*
      EXC=WD(1,J)-X(155)+W*(X(LE+J+27)*X(L+9)+SH(J)*X(L+10))                    ! rev 20200403(T.Nagai)
     *    +WD(4,J)*X(L+11)-WD(5,J)*X(L+12)
     *    -WD8(J)*X(L+15)
      W=X(L+7)+X(L+8)+EXC*X(L+2)
      ACC1=ACC1+W*FC
      ACC2=ACC2+W*FR
      X(L+7)=X(L+7)*X(L+4)+EXC*X(L+3)
      X(L+8)=X(L+8)*X(L+6)+EXC*X(L+5)
*
      L=L+LSZSPC(1)
      GO TO 612
*
***          3.9. HEAT GAIN THROUGH INSIDE WALL **********************
*
  630 CONTINUE
      IF(M(L+1).NE.3) THEN
       IF(M(L+1).EQ.0) THEN
        EXC=(WD(1,J)-X(155))*X(L+15)
       ELSE IF(M(L+1).EQ.1) THEN
        EXC=WD(1,J)+X(L+15)-X(155)
       ELSE
        EXC=X(L+15)-X(155)
       END IF
       W=X(L+11)+X(L+12)+EXC*X(L+3)
       ACC1=ACC1+W*FC
       ACC2=ACC2+W*FR
       X(L+11)=X(L+11)*X(L+7)+EXC*X(L+5)
       X(L+12)=X(L+12)*X(L+10)+EXC*X(L+8)
      END IF
*
      L=L+LSZSPC(2)
      GO TO 612
*
***          3.10. HEAT GAIN THROUGH WINDOW **************************
*
  640 CONTINUE
      LE=M(L+1)
      IF(CHCA(J)*X(LE+4)+CHSA(J)*X(LE+3).LT.0.) THEN
        W=WD(3,J)
      ELSE
        IF(X(L+13)*CHSA(J)+X(L+14)*CHCA(J).LT.SH(J)) THEN
          W=WD(3,J)
        ELSE
          W=0.
        END IF
      END IF
*
      EXC1=WD(1,J)-X(155)-WD(5,J)*X(L+12)
      V1=W*X(LE+J+51)                                                           ! rev 20200403(T.Nagai)
      V2=W*SH(J)*X(L+9)
      V3=WD(4,J)*X(L+11)
      V4=WD(4,J)*X(L+9)
      EXC2=V1+V2+V3+V4
*
      DO 643 II=0,1   ! ブラインド開閉ループ
       DO 644 I=0,2   ! K,SCC,SCRループ
        L1=L+20+II*9+I*3  ! XMQ配列の添字（スケジュールオプション）
        W=X(L+2+II*3+I)   ! オリジナルの物性値
        IF(M(L1).EQ.0) THEN   ! オリジナルのまま
         WINCHR(I,II)=W
        ELSE IF(M(L1).EQ.1) THEN   ! on時%の値を使用
         IF((IOPTWK.EQ.1).OR.(IOPTWK.EQ.3)) THEN   ! 稼動中あるいは停止時
          WINCHR(I,II)=X(L1+2)*W
         ELSE
          WINCHR(I,II)=W
         END IF
        ELSE   ! DSCHを使用
         WINCHR(I,II)=X(M(L1+1)+(KSCH(1)-1)*24+J)*W
        END IF
  644  CONTINUE
  643 CONTINUE
      IF((M(L+38).GE.1).AND.
     -   ((IOPTWK.EQ.1).OR.(IOPTWK.EQ.3))) THEN   ! 稼動中あるいは停止時
       DO 645 II=0,1   ! ブラインド開閉ループ
        DO 646 I=0,2   ! K,SCC,SCRループ
         WINCHR(I,II)=WINCHR(I,II)+X(L+39+II*3+I)
  646   CONTINUE
  645  CONTINUE
      END IF
*
      IF(X(LO+J).EQ.0.) GO TO 641
      W=EXC2*WINCHR(2,0)
      IF(W.GT.X(L+8)) GO TO 641
*
      W1=EXC1*WINCHR(0,0)
      ACC1=ACC1+W1*(1.0-X(L+45))+EXC2*WINCHR(1,0)
      ACC2=ACC2+W1*X(L+45)+W
      ACC4=ACC4+WINCHR(0,0)-X(L+5)
*
      IF(X(LC+43).EQ.0.) GO TO 642
      W=(X(L+15)*V3+X(L+16)*(V1+V3)+X(L+17)*(V2+V4))*WINCHR(2,0)
      IF(W.GT.X(LC+43)) ACC6=ACC6+X(L+19)
      GO TO 642
*
  641 W1=EXC1*WINCHR(0,1)
      ACC1=ACC1+W1*(1.0-X(L+46))+EXC2*WINCHR(1,1)
      ACC2=ACC2+W1*X(L+46)+EXC2*WINCHR(2,1)
*
      IF(X(LC+43).EQ.0.) GO TO 642
      W=X(L+18)*(V1+V2+V3+V4)*WINCHR(2,1)
      IF(W.GT.X(LC+43)) ACC6=ACC6+X(L+19)
*
  642 CONTINUE
*
      L=L+LSZSPC(3)
      GO TO 612
*
***          3.11. INFILTRATION **************************************
*
  650 CONTINUE
      LE=M(L+1)
      W=ABS(X(LE+2)-WD(6,J))
      U=CF(COS(W*DR))*X(LC+7)*WD(7,J)**2+(WD(1,J)-X(155))*X(LC+6)
*
      IF(M(L+4).EQ.0) THEN   ! オリジナル換気量で一定、あるいは空調on・off時%の値を使用
       IF((IOPTWK.EQ.1).OR.(IOPTWK.EQ.3)) THEN   ! 稼動中あるいは停止時
        W=X(L+6)
       ELSE
        W=X(L+7)
       END IF
      ELSE   ! DSCH使用
       W=X(M(L+5)+(KSCH(1)-1)*24+J)
      END IF
      IF(M(L+2).NE.0) GO TO 651
      IF(U.GT.0.) THEN
       V=W*X(L+3)*U**0.67
      ELSE
       V=0.
      END IF
      GO TO 652
*
  651 IF((U.GT.0.).OR.(M(L+2).EQ.2)) THEN
       V=W*X(L+3)
      ELSE
       V=0.
      END IF
*
  652 ACC1=ACC1+0.288*V*(WD(1,J)-X(155))
      ACC3=ACC3+0.720*V*(WD(2,J)-X(157))
      ACC4=ACC4+0.288*V
      ACC5=ACC5+0.720*V
      X(LC+74)=X(LC+74)+0.288*V*(WD(1,J)-X(155))
      X(LC+75)=X(LC+75)+0.288*V
*
      L=L+LSZSPC(4)
      GO TO 612
*
***          3.12. INTERNAL HEAT *************************************
*
  660 CONTINUE
      IF(LL.EQ.0) GO TO 661
      IF(X(LC+43).EQ.0.) ACC6=0.
      IF(ACC6.GT.1.) ACC6=1.
      W=X(LL+J)*MCNTL(21+ISEAS(1))/100.*(1.-ACC6)   ! 季節別発熱割合を掛ける
      W1=X(LC+41)+X(LC+42)+W*X(LC+36)
      X(LC+41)=X(LC+41)*X(LC+38)+W*X(LC+37)
      X(LC+42)=X(LC+42)*X(LC+40)+W*X(LC+39)
      ACC1=ACC1+W1*FC
      ACC2=ACC2+W1*FR
*
  661 IF(LH.EQ.0) GO TO 662
      W1=X(LH+J)*MCNTL(27+ISEAS(1))/100.   ! 季節別発熱割合を掛ける
      ACC1=ACC1+W1*X(LC+48)
      ACC2=ACC2+W1*X(LC+49)
      ACC3=ACC3+W1*X(LC+50)
      IF(ABS(X(LC+49)).LT.0.001) THEN   ! 強制対流式の場合
       X(LC+74)=X(LC+74)+W1*X(LC+48)    ! 面積を持たない部位からの負荷に算入
      END IF
*
  662 IF(LO.EQ.0) GO TO 663
      W1=X(LO+J)*MCNTL(24+ISEAS(1))/100.   ! 季節別発熱割合を掛ける
*     W2=X(LC+53)+X(LC+54)*(X(155)-24.)                                         ! rev 20121002(T.Nagai) Debug
      W2=X(LC+53)+X(LC+54)*(X(LC+214+ISEAS(1))-24.)                             ! rev 20200403(T.Nagai)
      ACC1=ACC1+W1*W2*FC
      ACC2=ACC2+W1*W2*FR
      ACC3=ACC3+(X(LC+52)-W2)*W1
*
***          3.13 CONVERT HEAT GAIN TO COOLING LOAD ******************
*
  663 X(J)=ACC1+X(LC+11)+ACC2*X(LC+8)
      X(LC+11)=X(LC+11)*X(LC+10)+ACC2*X(LC+9)
      X(J+24)=ACC4
      X(J+48)=ACC3
      X(J+72)=ACC5
*
***                CALCULATION EXTRACTING LOAD   *********************
*
      IF ( LOPC.NE.0 ) THEN
        CALL EXTRC1(J,NHR,LOPC,LC,NAZ,ISEAS(1),KSCH(1),IOPTWK,IOPTG,
     -         IOPVG,SMRT1,SMRT2,LCG,VOAG,CLDG,P0,RMMN,RMMX,SPCAP,
     -         EXCAP,VFLOW)
      END IF
*
***          3.14. END OF HOURLY LOOP ********************************
*
  670 CONTINUE
*
*
***          3.15. OUTPUT 3 (SPACE COOLING LOAD) *********************
*
      IF(IACSS.EQ.1) THEN
        WRITE(NUO,'(24E15.7)') (X(I),I=1,48)
        WRITE(NUO,'(24E15.7)') (X(I),I=49,96)
      END IF
*
***          3.16. END OF SPACE LOOP *********************************
*
      LCO=LC
      LC=M(LC)
      GO TO 610
*
***         3.17. JOB END ********************************************
*
  700 CONTINUE
      WRITE(6,91)
   91 FORMAT(1H0,'JOB COMPLETED')
      STOP
      END
*
*****       4. SUBROUTINES *******************************************
***          4.1. CODE NAME RETRIEVAL ********************************
*
      SUBROUTINE RETRIV(LO,QNAM,NNAM,LC,LD)
*
      PARAMETER (MX=30000)
      CHARACTER QNAM*4,QLIT*47
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X
*
      DATA QLIT/' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789=+-*/(),.:'/
*
      NNAM=0
      DO 11 I=1,4
      N=INDEX(QLIT,QNAM(I:I))
      IF(N.EQ.0) N=1
   11 NNAM=100*NNAM+N
      LC=LO
      LD=LO
   10 IF(M(LC+1).EQ.NNAM) RETURN
      LD=M(LC)
      IF(LD.EQ.0) RETURN
      LC=LD
      GO TO 10
*
      ENTRY NAME(QNAM,NNAM)
      NN=NNAM
      DO 12 I=1,4
      N=INT(NN/100**(4-I))
      QNAM=QNAM(2:4)//QLIT(N:N)
      NN=NN-N*100**(4-I)
   12 CONTINUE
      RETURN
*
      END
*
*
***          4.2. DECODING ARITHMETIC EXPRESSION *********************
*
      SUBROUTINE ARITH(Q,A,NERR)
*
      DIMENSION STC(40),MST(40)
      CHARACTER Q*80,QN*40,QC,QA*18
      EQUIVALENCE (STC,MST)
      DATA QA/' ()+-*/.0123456789'/
*
      I=41
      ISTC=1
      MST(1)=0
*
   10 QN=' '
      MD=1
*
   11 IF(I.EQ.72) THEN
       IF(MD.NE.3) GO TO 14
       GO TO 13
      END IF
*
      I=I+1
      QC=Q(I:I)
      JS=INDEX(QA,QC)
*
      IF(JS.EQ.0) GO TO 14
      IF(JS.EQ.1) THEN
       IF(MD.EQ.2) GO TO 12
       GO TO 11
      END IF
      IF(JS.EQ.2) THEN
       IF(MD.NE.1) GO TO 14
       ISTC=ISTC+1
       MST(ISTC)=2
       GO TO 11
      END IF
      IF(JS.GE.8) THEN
       IF(MD.EQ.3) GO TO 14
       QN=QN(2:40)//QC
       MD=2
       GO TO 11
      END IF
      IF(MD.EQ.1) GO TO 14
      IF(MD.EQ.3) GO TO 13
*
   12 READ(QN,'(F40.0)') A
*
      IF(JS.EQ.1) THEN
       MD=3
       GO TO 11
      END IF
*
   13 JR=MST(ISTC)
      ISTC=ISTC-1
*
      IF(JS.EQ.3.AND.JR.EQ.2) THEN
       MD=3
       GO TO 11
      END IF
*
      IF(JS.EQ.1.AND.JR.EQ.0) THEN
C      WRITE(6,90) A
C  90  FORMAT(5X,'DECODED ARITHMETICS=',1PE12.4)
       RETURN
      END IF
*
      IF(JR.LT.JS) THEN
       IF(JS.EQ.3) GO TO 14
       STC(ISTC+2)=A
       MST(ISTC+3)=JS
       ISTC=ISTC+3
       GO TO 10
      ELSE
       IF(JR.EQ.4) A=STC(ISTC)+A
       IF(JR.EQ.5) A=STC(ISTC)-A
       IF(JR.EQ.6) A=STC(ISTC)*A
       IF(JR.EQ.7) A=STC(ISTC)/A
       ISTC=ISTC-1
       GO TO 13
      END IF
*
   14 CALL ERROR(20,NERR)
      A=1.
      RETURN
*
      END
*
*
***          4.3. TRANSFER VECTERS ***********************************
*
      SUBROUTINE GVECTR(QWL,NL,MT,TH,HT,HO,TRNS,ADMT)
*
      DIMENSION MT(NL),TH(NL),R(0:22),C(0:22),S(0:9)
      DIMENSION TRNS(0:9),ADMT(0:9),TCM(2,100)
      CHARACTER QWL*4
*     家具容量の応答に関する石野らの実験・調査データ（顕熱用）
      REAL*8    SUM
      DIMENSION G0QC(22), G1QC(22), EPS(22), XN(22)
      DATA G0QC/ 0.2893, 0.2518, 0.2311, 0.2119, 0.1790, 0.1794, 0.2187,
     -           0.2038, 0.1234, 0.0696, 0.0772, 0.0927, 0.0966, 0.0411,
     -           0.0465, 0.1398, 0.0741, 0.5120, 0.3653, 0.1982, 1.0000,
     -           0.2572/
      DATA G1QC/ 0.5579, 0.5260, 0.3129, 0.3720, 0.3038, 0.3176, 0.3820,
     -           0.5263, 0.4707, 0.3461, 0.2612, 0.3520, 0.2566, 0.2090,
     -           0.1535, 0.7415, 0.5722, 0.2513, 0.5668, 0.6222, 0.0000,
     -           1.6416/
      DATA EPS / 0.785 , 0.703 , 0.407 , 0.472 , 0.370 , 0.387 , 0.489 ,
     -           0.661 , 0.537 , 0.372 , 0.283 , 0.388 , 0.284 , 0.218 ,
     -           0.161 , 0.862 , 0.618 , 0.515 , 0.893 , 0.776 , 0.000 ,
     -           2.210/
      DATA XN  /  3.0  , 13.2  , 17.1  ,  7.7  ,  0.0  ,  9.8  ,  0.0  ,
     -            1.2  , 10.2  ,  5.3  ,  3.7  ,  1.4  , 11.4  ,  7.5  ,
     -            3.9  ,  0.0  ,  0.0  ,  3.6  ,  0.0  ,  0.5  ,  0.4  ,
     -            0.1/
*
      DATA S/ 0., 0.00025,0.001,  0.004,  0.016,  0.064,   0.256,
     *     1.024, 4.096,  16.384/
*
      DATA TCM/200*0.0/
      SAVE TCM
      IF(QWL.EQ.'INIT') THEN   ! 特性値の読み込み
       I=NINT(HO)   ! 装置番号
       DO 5 L=1,100
        READ(I,*,END=99) J,IU,ALMD,CRHO
        IF((J.GE.1).AND.(J.LE.100)) THEN
         IF(IU.EQ.1) THEN   ! SI入力
          IF(J.GT.90) THEN  ! 純抵抗
           IF(ALMD.EQ.0.0) THEN   ! 未定義(非引用)の材料を想定
            TCM(1,J)=1.0E+6
           ELSE
            TCM(1,J)=1.0/(ALMD/0.86)   ! [m2K/W]から[(m2hdeg/kcal)^(-1)]へ
           END IF
           TCM(2,J)=0.0
          ELSE
           TCM(1,J)=ALMD*0.86   ! [W/mK]から[kcal/mhdeg]へ
           TCM(2,J)=CRHO/4.186  ! [kJ/m3K]から[kcal/m3deg]へ
          END IF
         ELSE
          IF(J.GT.90) THEN  ! 純抵抗
           IF(ALMD.EQ.0.0) THEN   ! 未定義(非引用)の材料を想定
            TCM(1,J)=1.0E+6
           ELSE
            TCM(1,J)=1.0/ALMD    ! [(m2hdeg/kcal)^(-1)])
           END IF
           TCM(2,J)=0.0
          ELSE
           TCM(1,J)=ALMD        ! [kcal/mhdeg]
           TCM(2,J)=CRHO        ! [kcal/m3deg]
          END IF
         END IF
        END IF
    5  CONTINUE
   99  RETURN
      END IF
*
      IF(QWL.EQ.'FURN') GO TO 13
      IF(QWL.EQ.'FURS') GO TO 16   ! 顕熱家具熱容量
      R(0)=1/HT
      C(0)=0.
      DO 10 L=1,NL
      IF(MT(L).GT.0) THEN
       I=MT(L)
       IF(I.GT.90) TH(L)=1.
       R(L)=TH(L)/TCM(1,I)
       C(L)=TH(L)*TCM(2,I)
      ELSE
       R(L)=TH(L)   ! 純熱抵抗
       C(L)=0.
      END IF
   10 CONTINUE
*
      IF(QWL.EQ.'GWAL') GO TO 15
      NL=NL+1
      R(NL)=1/HO
      C(NL)=0.
*
   15 CONTINUE
*
      DO 12 J=0,9
      G0=1.
      U1=1.
      U2=0.
      U3=0.
      U4=1.
*
      DO 11 L=0,NL
      W=SQRT(S(J)*R(L)*C(L))
      IF(W.EQ.0.) THEN
       V1=1.
       V2=R(L)
       V3=0.
      ELSE
       V0=EXP(-W)
       G0=G0*V0
       V1=0.5*(1.+V0**2)
       V2=0.5*R(L)*(1.-V0**2)/W
       V3=0.5*W*(1.-V0**2)/R(L)
      END IF
*
      W1=U1
      W3=U3
      U1=W1*V1+U2*V3
      U2=W1*V2+U2*V1
      U3=W3*V1+U4*V3
      U4=W3*V2+U4*V1
   11 CONTINUE
*
      TRNS(J)=G0/U2
      ADMT(J)=U4/U2
   12 CONTINUE
*
      RETURN
*
   13 V=2
      W=V*HO
      DO 14 J=0,9
      TRNS(J)=0.
   14 ADMT(J)=W*S(J)/(S(J)+V)
      RETURN
*     石野らの文献参照（AIJ黄表紙Vol.372, pp.59-66, 1987）
   16 CONTINUE
      DO 17 J=0,9
        TRNS(J)=0.
        SUM=0.0D0
        DO 18 K=1,22
          IF(ABS(EPS(K)).LT.0.0001)THEN
            SUM=SUM+XN(K)*G0QC(K)
          ELSE
            SUM=SUM+XN(K)*(G0QC(K)+G1QC(K)/(S(J)+EPS(K)))
          END IF
   18   CONTINUE
        ADMT(J)=SUM*0.01*HO*S(J)
   17 CONTINUE
      RETURN
*
      END
*
*
***          4.4. CONVOLUTION PARAMETERS *****************************
*
*     数値Laplace逆変換し，convolution parametersを求める．
*     HASP/ACLD/8501の収束計算でしばしばエラーになるのを改良
*     2001-06-25, Akihiro Nagata (Revised by T.Nagai)

      subroutine cparam(nt, g, p)

*argument:
      integer    nt     !r! 項数
      real       g(0:9) !r! 応答
      real       p(8)   !w! convolution parameters
*local:
      real*8     z(4,5), zz(5), h(9), e(2), ee(2), a(2)
      real*8     u, v, w, r, rr, rr0, rr1
*local(data):
      real*8     s(0:9) !-! Laplace parameters
      data   s / 0.00000d0, 0.00025d0, 0.00100d0, 0.00400d0, 0.01600d0,
     &           0.06400d0, 0.25600d0, 1.02400d0, 4.09600d0,16.38400d0/

c      write(6,'(1x,a)') '% g-vectors'
c      write(6,'(1x,1p2e20.12)') (s(j), g(j), j = 0, 9)

      do j = 1, 9
        h(j) = (g(j) - g(0))/s(j)
      end do
      do i = 1, 8
        p(i) = 0.
      end do

      n = 1
   12 continue
      rr0  = 1d20
      nn   = 2*n
      nn1  = nn + 1
      e(n) = s(9)
      nc   = 1

   13 continue
      do i = 1, nn
        do k = 1, nn1
          z(i,k) = 0.
        end do
      end do

      do j = 1, 9
        do i = 1, n
          zz(i)   = 1/(s(j) + e(i))
          zz(n+i) = zz(i)**2
        end do
        zz(nn1) = h(j)
        do i = 1, nn
          do k = 1, nn1
            z(i,k) = z(i,k) + zz(i)*zz(k)
          end do
        end do
      end do

      do i = 1, nn
        if (abs(z(i,i)) .lt. 1d-20) then
          w = 1
          nc = 100
          goto 100
        end if
        do j = i+1, nn1
          z(i,j) = z(i,j)/z(i,i)
        end do

        do k = 1, nn
          if(i .ne. k) then
            do j = i+1, nn1
              z(k,j) = z(k,j) - z(i,j)*z(k,i)
            end do
          end if
        end do
      end do

      do i = 1, n
        w = z(n+i,nn1)/(e(i)*z(i,nn1) + z(n+i,nn1))
        if (abs(w) .lt. 10.) then
          ee(i) = e(i)*exp(-w)
        else
          nc = 100
        end if
      end do

*     残差の計算

      rr = 0d0
      do j = 1, 9
        r = h(j)
        do i = 1, n
          r = r - z(i,2*n+1)/(s(j) + ee(i))
        end do
        rr = rr + r*r
      end do
      rr = sqrt(rr/9.)

      if (rr .lt. rr0) then
        rr0 = rr
        do i = 1, n
          a(i) = z(i,2*n+1)
          e(i) = ee(i)
        end do
        do i = n+1, 2
          a(i) = 0
          e(i) = 0
        end do
c        write(6, '(2i5,1p,5e15.5)') nc, n, e(1), e(2), a(1), a(2), rr0
      else
        nc = 100
      end if

  100 continue
      if (abs(w) .gt. 1d-4) then
        if (nc .ge. 100) then  ! not converged
          if (n .eq. 1) then ! 1項でも収束しない場合はエラー
            NERR=0
            CALL ERROR2(30,1)
            return
          else if (rr0 .ge. rr1) then ! 1項近似より残差が大きい
            n = 1
            a(1) = u
            e(1) = v
          end if
        else
          nc = nc + 1
          go to 13
        end if
      else if ((nt .eq. 2).and.(n .eq. 1).and.(e(1) .lt. 1.)) then
        u = z(1,3)
        v = e(1)
        n = 2
        rr1 = rr  ! 1項で近似したときのRMSE
       go to 12
      end if

*     Convolution parameters

      p(1) = g(0)
      p(2) = g(0)
      do i = 1, n
        u    = a(i)/e(i)
        v    = exp(-e(i))
        p(1) = p(1) + u*(1.-v)
        p(2) = p(2) + 2.*u*(1.-v)/(1.+v)
        p(3*i+0) = -u*(1.-v)**2
        p(3*i+1) = -u*(1.-v)**2/(1.+v)
        p(3*i+2) = v
      end do
      do i = n+1, 2
        p(3*i+0) = 0
        p(3*i+1) = 0
        p(3*i+2) = 0
      end do

c     write(6,'(5x,a,1p5e12.4)')
c    &  'approximate step response:', g(0), (a(i), e(i), i = 1, n)

      return
      end
*
*
***          4.5 ERROR MESSAGE ***************************************
*
      SUBROUTINE ERROR(I,NERR)
*
      WRITE(6,90) I
   90 FORMAT(4X,'****** ERROR',I3,' ******')
      NERR=NERR+1
      RETURN
      END
*
***        4.5.5 ERROR MESSAGE (2) ***********************************
*
      SUBROUTINE ERROR2(I,IOPT)
*
      IF(IOPT.EQ.0) THEN
       WRITE(6,'(4X,A,I3,A)') '****** WARNING',I,' ******'
      ELSE
       WRITE(6,'(4X,A,I3,A)') '****** ERROR',I,' ******'
       IF(IOPT.NE.1) THEN
        WRITE(6,'(4X,A)') 'PLEASE INFORM YOUR DISTRIBUTOR WITH THE ERROR
     - CODE'
       END IF
       STOP
      END IF

      RETURN
      END
*
***          4.6. ARRAY LISTING **************************************
*
      SUBROUTINE DEBUGA(X)
      CHARACTER QA*125
      DATA QA,I/'     ',0/
*
      I=I+1
      II=12*I-6
      WRITE(QA(II:II+11),'(1PE12.4)') X
      IF(I.EQ.10) GOTO 10
      RETURN
*
      ENTRY DEBUGB(M)
      I=I+1
      II=12*I-6
      WRITE(QA(II:II+11),'(I12)') M
      IF (I.EQ.10) GO TO 10
      RETURN
*
      ENTRY DEBUGC
   10 WRITE(6,'(A125)') QA
      QA='     '
      I=0
      RETURN
*
      END
*
***          4.7 INPUT DATA CHECK ************************************
*
      SUBROUTINE DCHECK(Q,LC,NERR)
*
      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X
*
      CHARACTER Q*80,QBL*20,QDT*20
      DATA QBL/'                    '/
*
      L=LC
      K=5
*
  100 M1=M(L)
      IF(M1.EQ.16) RETURN
      IF(M1.LT.0) THEN
       L=L+1
       K=K+IABS(M1)
       GO TO 100
      END IF
*
      M2=INT(M1/8+1)*3
      M3=MOD(M1,8)/4
      M4=MOD(M1,4)/2
      M5=MOD(M1,2)
      QDT=QBL(M2:19)//Q(K:K+M2-1)
      IF(QDT.NE.QBL) GOTO 101
*
      IF(M4.EQ.1) CALL ERROR(10,NERR)
      GO TO 103
*
  101 IF(QDT(20:20).EQ.' ') CALL ERROR(11,NERR)
*
      IF(M5.EQ.0) GO TO 103
      IF(M3.EQ.0) THEN
       READ(QDT,'(I20)') MZ
       IF(MZ.LT.M(L+1).OR.MZ.GT.M(L+2)) CALL ERROR(13,NERR)
      ELSE
       READ(QDT,'(F20.0)') WZ
       IF(WZ.LT.X(L+1).OR.WZ.GT.X(L+2)) CALL ERROR(13,NERR)
      END IF
*
  103 L=L+M5*2+1
      K=K+M2
      GO TO 100
*
      END
*
***          4.8 INITIALIZATION **************************************
*
      BLOCK DATA XMQARY
      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X
      INTEGER MCNTL(32)                                                         ! rev 20200403(T.Nagai)
      COMMON /ETC/MCNTL
*
      DATA (M(I),I=1,118)/108*0,1010101,1,1,1,1,1,2,3,3,3/
      DATA  X(119)/-1.0/
*
      DATA (X(I),I=120,158)/39*0.0/
*
      DATA (M(I),I=159,559)/401*0/
*
***        'BUIL' CHECK LIST ********************
      DATA M(560),M(561),X(562),X(563),M(564),X(565),X(566)/
     *     -7,   13,-90.0,90.0,   13,-180.0,180.0/                              ! rev 20200409(T.Nagai)
      DATA M(567),M(568),M(569),M(570),M(571),X(572),X(573)/
     *     14,   9,0,100,   13,-50.0,100.0/
      DATA (M(I),I=574,578),X(579),X(580),M(581)/                               ! rev 20200403(T.Nagai)
     *     9,0,100,   12,   13,-12.0,12.0,   16/                                ! rev 20200403(T.Nagai)
*
***        'CNTL' CHECK LIST ********************
      DATA (M(I),I=1001,1051)/ -7,   1,0,1,   1,0,1,   1,0,1,   1,0,1,          ! rev 20200409(T.Nagai)
     *   1,0,2,   1,0,99,   1,1,12,   1,1,31,   1,0,99,   1,1,12,
     *   1,1,31,   1,0,99,   1,1,12,   1,1,31,   1,1,999,   -3,                 ! rev 20200409(T.Nagai)
     *   1,0,1,   16/                                                           ! rev 20200409(T.Nagai)
*
***        'HRAT' CHECK LIST ********************
      DATA (M(I),I=1341,1369)/ -7,   1,0,100,   1,0,100,   1,0,100,
     *   1,0,100,   1,0,100,   1,0,100,   1,0,100,   1,0,100,   1,0,100,
     *   16/
*
***        'EXPS' CHECK LIST ********************
      DATA M(597),M(598),X(599),X(600),M(601),X(602),X(603)/
     *     -7,   15,0.0,180.0,   15,-180.0,360.0/
      DATA (M(I),I=604,614)/   4,   4,   12,   12,   12,   12,
     *      12,   12,   12,   12,   16/
*
***        'WCON' CHECK LIST ********************
      DATA (M(I),I=615,660)/   -7,   3,1,100,   0,   1,1,100,   0,
     *   1,1,100,   0,   1,1,100,   0,   1,1,100,   0,   1,1,100,   0,
     *   1,1,100,   0,   1,1,100,   0,   1,1,100,   0,   1,1,100,   0,
     *   1,1,100,   0,   16/
*
***        'WSCH' CHECK LIST ********************
      DATA (M(I),I=661,689)/   -7,   1,1,3,   1,1,3,   1,1,3,   1,1,3,
     *      1,1,3,   1,1,3,   1,1,3,   1,1,3,   1,1,3,   16/
*
***        'SDAY' CHECK LIST ********************
      DATA (M(I),I=753,820)/   -7,   1,1,12,   1,1,31,   1,1,12,                ! rev 20200410(T.Nagai)
     *      1,1,31,   1,1,12,   1,1,31,   1,1,12,   1,1,31,   1,1,12,
     *      1,1,31,   1,1,12,   1,1,31,   1,1,12,   1,1,31,   1,1,12,
     *      1,1,31,   1,1,12,   1,1,31,   1,1,12,   1,1,31,   1,1,12,           ! rev 20200410(T.Nagai)
     *      1,1,31,   16/                                                       ! rev 20200410(T.Nagai)
*
***        'OSCH' CHECK LIST ********************
      DATA (M(I),I=1061,1123)/   -7,   1,0,24,   1,0,24,   1,0,24,
     *      1,0,24,   1,0,24,   1,0,24,   1,0,24,   1,0,24,   1,0,24,
     *      1,0,24,   -6,   1,0,24,   1,0,24,   1,0,24,   1,0,24,
     *      1,0,24,   1,0,24,   1,0,24,   1,0,24,   1,0,24,   1,0,24,
     *      16/
*
***        'OPCO' CHECK LIST ********************
      DATA (M(I),I=1131,1144),X(1145),X(1146),M(1147),X(1148),X(1149),
     *     (M(I),I=1150,1156),X(1157),X(1158),M(1159),X(1160),X(1161),
     *     (M(I),I=1162,1168),X(1169),X(1170),M(1171),X(1172),X(1173),
     *     M(1174),M(1175)/
     *   -7,   1,0,24,   1,0,24,   1,0,24,   -6,             4,   4,
     *   5,0.0,100.0,   5,0.0,100.0,   1,0,24,  -3,        4,   4,
     *   5,0.0,100.0,   5,0.0,100.0,   1,0,24,  -3,        4,   4,
     *   5,0.0,100.0,   5,0.0,100.0,   12,   16/
*
***        'OAHU' CHECK LIST ********************
      DATA M(1370),M(1371),X(1372),X(1373),M(1374),M(1375),X(1376),
     *     X(1377),M(1378),M(1379),M(1380),X(1381),X(1382),M(1383),
     *     X(1384),X(1385),M(1386),M(1387),X(1388),X(1389),M(1390),
     *     M(1391),M(1392),X(1393),X(1394),M(1395),X(1396),X(1397),
     *     M(1398),M(1399),X(1400),X(1401),M(1402),M(1403),M(1404),
     *     X(1405),X(1406),M(1407),X(1408),X(1409),M(1410)/
     *     -7,   5,0.0,100.0,   4,   5,0.0,100.0,
     *     4,   4,   5,0.0,100.0,   5,0.0,100.0,   4,   5,0.0,100.0,
     *     4,   4,   5,0.0,100.0,   5,0.0,100.0,   4,   5,0.0,100.0,
     *     4,   4,   5,0.0,100.0,   5,0.0,100.0,   16/
*
***        'SPAC' CHECK LIST ********************
      DATA (M(I),I=821,829)/   -10,   14,   14,   14,   1,0,3,
     *      4,   16/
*
***        'OWAL' CHECK LIST ********************
      DATA (M(I),I=830,841)/
     *      -10,   1,0,100,   1,0,100,   1,0,100,   12,   16/
*
***        'GWAL' CHECK LIST ********************
      DATA M(843)/   16/
*
***        'BECO' CHECK LIST ********************
      DATA (M(I),I=844,847)/   -22,   14,   14,   16/
*
***        'WNDW' CHECK LIST ********************   (1行目)
      DATA (M(I),I=848,861)/   -10,   3,1,200,   1,0,3,
     *      12,   1,0,100,   12,   12,   16/
*
***        'LIGH' CHECK LIST ********************
      DATA (M(I),I=863,873)/   -10,   1,1,6,   12,   1,1,2,
     *      -6,   12,   16/
*
***        'OCUP' CHECK LIST ********************
      DATA (M(I),I=874,882)/   -10,   1,1,9,   12,   1,   1,
     *      2,   16/
*
***        'HEAT' CHECK LIST ********************
      DATA (M(I),I=1191,1200)/   -10,   1,1,2,   12,   12,   1,1,2,
     *   16/
*
***        'FURN' CHECK LIST ********************
      DATA (M(I),I=890,893)/   -10,   12,   12,   16/
*
***        'SEAS' CHECK LIST ********************
      DATA (M(I),I=894,931)/   -7,   1,1,3,   1,1,3,   1,1,3,   1,1,3,
     *      1,1,3,   1,1,3,   1,1,3,   1,1,3,   1,1,3,   1,1,3,   1,1,3,
     *      1,1,3,   16/
*
***        'SOPC' CHECK LIST ********************
      DATA (M(I),I=966,971)/   -7,   12,   12,   12,   12,   16/
*
***        'SEAS' DATA INITIALIZATION ***********
      DATA (M(I),I=981,992)/2,2,2,3,3,1,1,1,1,3,3,2/
*
***        'CNTL' DATA INITIALIZATION ***********
      DATA (MCNTL(I),I=1,15)/6*0,12,15,0,1,1,0,12,31,1/
      DATA (MCNTL(I),I=19,21)/36509,36161,36525/
      DATA (MCNTL(I),I=31,32)/3,0/                                              ! add 20200403(T.Nagai)
*
***        'HRAT' DATA INITIALIZATION ***********
      DATA (MCNTL(I),I=22,30)/9*100/
*
***        'CFLW' CHECK LIST ********************
      DATA (M(I),I=1201,1224)/   -10,   1,0,999,   1,0,999,  12,   -9,
     *   1,0,2,   12,   -9,   1,0,2,   12,   -9,   1,0,2,   12,   16/
*
***        'DSCH' CHECK LIST ********************
      DATA (M(I),I=1225,1295)/   -7,   1,0,24,   1,0,999,   1,0,24,
     *      1,0,999,   1,0,24,   1,0,999,   1,0,24,   1,0,999,
     *      1,0,24,   1,0,999,   1,0,24,    1,0,999,  1,0,24,
     *      1,0,999,   1,0,24,   1,0,999,   1,0,24,   1,0,999,
     *      1,0,24,   1,0,999,   1,0,24,    1,0,999,  1,0,24,   16/
*
***        'IWAL' CHECK LIST ********************
      DATA (M(I),I=1296,1301)/   -7,   1,0,3,   12,   16/
*
***        'WNDW' CHECK LIST ********************   (2行目)
      DATA (M(I),I=1302,1326)/   -10,   1,0,999,   -6,   1,0,999,   -6,
     *      1,0,999,   -9,   1,0,999,   -6,   1,0,999,   -6,   1,0,999,
     *      16/
*
***        'INFL' CHECK LIST ********************
      DATA (M(I),I=1327,1340)/   -10,   1,0,2,   -3,   12,   -6,
     *      1,0,999,   1,0,999,   16/
*
      END
*
**************** READ STANDARD WEATHER DATA **************************
*     LATEST REVISION     - 2004.02.07
*                         - 2020.04.03                                          ! add 20000403(T.Nagai)
*     ARGUMENTS NUW       - I    気象データファイルの装置番号
*               IOPWE     - I    =0:周期データ、=1:実在データ
*               NCLM      - I    データカラム数（3以上9以下）                   ! add 20000403(T.Nagai)
*               ICYCL     - I/O  現在のファイル読み込みサイクル数
*               WD(7,24)  - O    気象データ（加工前）
*               ID(7,5)   - O    日付データ（4桁で返す。ただし
*                                IOPWE=0のときの年は"0"）
*               ISTAT     - O    =1:通常  0:ファイル終了(IOPWE=1のとき)
*     REQ. ROUTINES       - NDATE
*********************************************************************
      SUBROUTINE INWD(NUW,IOPWE,NCLM,ICYCL,WD,ID,ISTAT)                         ! rev 20000403(T.Nagai)
      DIMENSION WD(7,24),ID(7,5)
      CHARACTER  Q1*1, QFMT*16                                                  ! add 20000403(T.Nagai)
      DATA QFMT/'(24F .0,3I2,2I1)'/                                             ! add 20000403(T.Nagai)
*
      WRITE(QFMT(5:5),'(I1)') NCLM                                              ! add 20000403(T.Nagai)
  100 DO 200 I=1,7
  200 READ(NUW,QFMT,END=999)                                                    ! rev 20000403(T.Nagai)
     -           (WD(I,J),J=1,24),(ID(I,J),J=1,5)
      IF(IOPWE.EQ.0)THEN
       DO 210 I=1,7
  210  ID(I,1)=0
      ELSE
       DO 220 I=1,7
        IF(ID(I,1).GE.51)THEN
         ID(I,1)=1900+ID(I,1)
        ELSE
         ID(I,1)=2000+ID(I,1)
        END IF
  220  CONTINUE
      END IF
      ISTAT=1
      RETURN
  999 IF(IOPWE.EQ.0)THEN
       REWIND(NUW)
       READ(NUW,'(A)') Q1                                                       ! add 20000403(T.Nagai)
       IF(Q1.NE.'*') THEN                                                       ! add 20000403(T.Nagai)
        BACKSPACE(NUW)    ! 先頭行がヘッダ行でない場合は1行巻き戻す               add 20000403(T.Nagai)
       END IF                                                                   ! add 20000403(T.Nagai)
       ICYCL=ICYCL+1
       IF(ICYCL.EQ.1000)THEN
        CALL ERROR2(102,1)
       END IF
       GO TO 100
      ELSE
       ISTAT=0
       RETURN
      END IF
*
      END
*
**************** READ HEADER OF STANDARD WEATHER DATA ****************
*     LATEST REVISION     - 2020.04.03
*     ARGUMENTS QA*200    - I    ヘッダ行の文字列
*               IWFLG(4)  - I    (1) =0:ヘッダ行がない、=1:ヘッダ行がある
*                                    (この数値は引用されない。=1となって
*                                     このルーチンが呼ばれるはず）
*                           O    (2) 日射・放射の単位、=0:[10kJ/m2h],
*                                    =1:[kcal/m2h], =2:[kJ/m2h]
*                           O    (3) 雲量モード、=0: 雲量, =1: 夜間放射
*                           O    (4) 気象データのカラム数(3以上9以下)
*               RWFLG(3)  - O    (1) 緯度[deg]（南緯の場合は負値）
*                           0    (2) 経度[deg]（西経の場合は負値）
*                           0    (3) 世界時と地方標準時の時差
*                                    （日本の場合は9.0）
*     REQ. ROUTINES       - NONE.
*     REMARKS             - 
**********************************************************************
      SUBROUTINE RHEAD(QA,IWFLG,RWFLG)                                          ! add 20200403(T.Nagai)
      CHARACTER  QA*200
      DIMENSION  IWFLG(4), RWFLG(3)

      IF(QA(16:19).EQ.'10kJ') THEN
        IWFLG(2) = 0
      ELSE IF(QA(16:19).EQ.'kcal') THEN
        IWFLG(2) = 1
      ELSE IF(QA(16:19).EQ.'kJ  ') THEN
        IWFLG(2) = 2
      ELSE
        CALL ERROR2(54,1)
      END IF

      IF(QA(21:23).EQ.'CA ') THEN
        IWFLG(3) = 0
      ELSE IF(QA(21:23).EQ.'LNR') THEN
        IWFLG(3) = 1
      ELSE
        CALL ERROR2(54,1)
      END IF

      READ(QA(25:25),'(I1)') IWFLG(4)
      IF((IWFLG(4).LT.3).OR.(IWFLG(4).GT.9)) THEN
        CALL ERROR2(54,1)
      END IF

      READ(QA(61:65),'(I2,I3)') IWK1, IWK2
      IF(QA(66:66).EQ.'N') THEN
        RWFLG(1) = IWK1 + IWK2/600.0
      ELSE IF(QA(66:66).EQ.'S') THEN
        RWFLG(1) = -IWK1 - IWK2/600.0
      ELSE
        CALL ERROR2(54,1)
      END IF

      READ(QA(68:73),'(2I3)') IWK1, IWK2
      IF(QA(74:74).EQ.'E') THEN
        RWFLG(2) = IWK1 + IWK2/600.0
      ELSE IF(QA(74:74).EQ.'W') THEN
        RWFLG(2) = -IWK1 - IWK2/600.0
      ELSE
        CALL ERROR2(54,1)
      END IF

      READ(QA(78:83),'(F6.0)') RWFLG(3)

      RETURN
      END
*
**************** DATING (DAY OF THE WEEK) ****************************
*     LATEST REVISION     - 1996.06.15
*     ARGUMENTS ID(7,5)   - INPUT   DATE.
*     FUNCTION  MKMDW               DAY OF THE WEEK.
*     REQ. ROUTINES       - NDATE
**********************************************************************
      INTEGER FUNCTION MKMDW(ID)
      PARAMETER (MX=30000)
      DIMENSION ID(7,5)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X
*
      KDY=NDATE(ID(1,2),ID(1,3))
*
      MKMDW=ID(1,4)-1
      IF(MKMDW.EQ.0) MKMDW=7
      IF(MKMDW.LT.0) MKMDW=8
      IF(M(KDY+192).EQ.1) MKMDW=9                                               ! rev 20200410(T.Nagai)
*
      RETURN
      END
*
**************** ANNUAL BASE DATING **********************************
*     ARGUMENTS MON       - INPUT.  MONTH.
*               IDY       - INPUT.  DAY OF THE MONTH.
*     FUNCTION  NDATE               DAY OF THE YEAR.
*     REQ. ROUTINES       - NONE.
**********************************************************************
      INTEGER FUNCTION NDATE(MON,IDY)
*
      NDATE=INT(30.57*MON-31.06-SIGN(1,MON-3)+IDY)
*
      RETURN
      END
*
**************** 2004.02.07 BASE DATING ******************************
*     1899年12月31日を1とした通算日数
*     ARGUMENTS IYR       - INPUT.  YEAR.(1951-2050あるいは0)
*               MON       - INPUT.  MONTH.
*               IDY       - INPUT.  DAY OF THE MONTH.
*     FUNCTION  NDATF               CUMULATIVE DAY.
*     REQ. ROUTINES       - NONE.
*     REMARKS   IYR=0のときは1999の月日に対する通算日数を返す
**********************************************************************
      INTEGER FUNCTION NDATF(IYR,MON,IDY)

      INTEGER NDS(1951:2050)    ! その年の前年12/31の通算日数
      DATA NDS  / 18628, 18993, 19359, 19724, 20089,    ! 1951-55
     -            20454, 20820, 21185, 21550, 21915,    ! 1956-60
     -            22281, 22646, 23011, 23376, 23742,    ! 1961-65
     -            24107, 24472, 24837, 25203, 25568,    ! 1966-70
     -            25933, 26298, 26664, 27029, 27394,    ! 1971-75
     -            27759, 28125, 28490, 28855, 29220,    ! 1976-80
     -            29586, 29951, 30316, 30681, 31047,    ! 1981-85
     -            31412, 31777, 32142, 32508, 32873,    ! 1986-90
     -            33238, 33603, 33969, 34334, 34699,    ! 1991-95
     -            35064, 35430, 35795, 36160, 36525,    ! 1996-2000
     -            36891, 37256, 37621, 37986, 38352,    ! 2001-05
     -            38717, 39082, 39447, 39813, 40178,    ! 2006-10
     -            40543, 40908, 41274, 41639, 42004,    ! 2011-15
     -            42369, 42735, 43100, 43465, 43830,    ! 2016-20
     -            44196, 44561, 44926, 45291, 45657,    ! 2021-25
     -            46022, 46387, 46752, 47118, 47483,    ! 2026-30
     -            47848, 48213, 48579, 48944, 49309,    ! 2031-35
     -            49674, 50040, 50405, 50770, 51135,    ! 2036-40
     -            51501, 51866, 52231, 52596, 52962,    ! 2041-45
     -            53327, 53692, 54057, 54423, 54788/    ! 2046-50

      IF(IYR.EQ.0)THEN
       NDATF = NDS(1999) + NDATE(MON,IDY)
      ELSE
       IF((IYR.LT.1951).OR.(IYR.GT.2050)) CALL ERROR2(108,2)
       NDATF = NDS(IYR) + NDATE(MON,IDY)
       IF ( (MOD(IYR,4).EQ.0).AND.(MON.GE.3) ) THEN   ! 2000年以外に100で割り切れる年はない
          NDATF = NDATF + 1
       END IF
      END IF

      RETURN
      END
*
**************** WINDOW DATA INTERPOLATION ***************************
*     LATEST REVISION     - 2006.03.30
*     ARGUMENTS IOPGL      - I   1:AFW指定、2:PPW指定
*               VV         - I   通気風量[L/m2s]
*               XPULL      - I   窓排気率[-](IOPGL=2のときのみ引用)
*               NVS0       - I   通気風量サンプリング数(>=1)
*                                (IOPGL=2のときはXPULL=0)
*               VVB0(NVS0) - I   通気風量サンプリング値[L/m2s](同上)
*               DSU0(NVS0) - I   ΔSC, ΔUのサンプリング値 (同上)
*               NVS1       - I   通気風量サンプリング数(>=1)
*                                (IOPGL=2のときのみ引用、XPULL=1)
*               VVB1(NVS1) - I   通気風量サンプリング値[L/m2s](同上)
*               DSU1(NVS1) - I   ΔSC, ΔUのサンプリング値 (同上)
*     REQ. ROUTINES       - NONE
*********************************************************************
      REAL FUNCTION DLTGL(IOPGL,VV,XPULL,NVS0,VVB0,DSU0,NVS1,VVB1,DSU1)
      DIMENSION VVB0(NVS0),DSU0(NVS0),VVB1(NVS1),DSU1(NVS1)
      DIMENSION DSUWK(0:1)
*
      IF(VV.LE.VVB0(1)) THEN
       DSUWK(0)=DSU0(1)
      ELSE IF(VV.GT.VVB0(NVS0)) THEN
       DSUWK(0)=DSU0(NVS0)
      ELSE
       DO 100 I=2,NVS0
        IF(VV.LE.VVB0(I)) THEN
         DSUWK(0)=((VV-VVB0(I-1))*DSU0(I)
     -             +(VVB0(I)-VV)*DSU0(I-1))/(VVB0(I)-VVB0(I-1))
         GO TO 200
        END IF
  100  CONTINUE
      END IF
*
  200 IF(IOPGL.EQ.1) THEN
       DLTGL=DSUWK(0)
       RETURN
      END IF
*
      IF(VV.LE.VVB1(1)) THEN
       DSUWK(1)=DSU1(1)
      ELSE IF(VV.GT.VVB1(NVS1)) THEN
       DSUWK(1)=DSU1(NVS1)
      ELSE
       DO 300 I=2,NVS1
        IF(VV.LE.VVB1(I)) THEN
         DSUWK(1)=((VV-VVB1(I-1))*DSU1(I)
     -             +(VVB1(I)-VV)*DSU1(I-1))/(VVB1(I)-VVB1(I-1))
         GO TO 400
        END IF
  300  CONTINUE
      END IF
*
  400 DLTGL=XPULL*DSUWK(1)+(1-XPULL)*DSUWK(0)
      RETURN
      END
