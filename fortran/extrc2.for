C-----------------------------------------------------------------------
C     1日分の除去熱量を計算する
C     LATEST REVISION   2012.08.03
C-----------------------------------------------------------------------
      SUBROUTINE EXTRC2(NHR,IPEAK,ISEAS,NAZ,IOPTG,NZ,IOPVG,SMRT1,SMRT2,
     -   VOAG,LCG,CLDG,NWD,WD,REFWD,P0,NSTP,VFLOW,EXCAP,SPCAP,RMMX,RMMN,
     -   NITER,NUOT1,ID,MDW,MODE,IOUT,LSZSPC,IBECS,NUOB)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

      PARAMETER ( NSL=2 )              !     顕熱と潜熱
C     引数
      INTEGER     NHR                  ! I   1日のステップ数
      INTEGER     IPEAK                ! I   1:ピーク計算モード、0:シミュレーションモード
      INTEGER     ISEAS                ! I   1:夏期、2:冬期、3:中間期
      INTEGER     NAZ                  ! I   ゾーン数を表わす整合寸法
      INTEGER     IOPTG(NAZ,NHR)       ! I   空調運転状態フラグ、=0:停止中、
                                       !     =1:運転中、=2:起動、=3:停止
      INTEGER     NZ                   ! I   現在のグループのゾーン数
      INTEGER     IOPVG(NAZ,NHR)       ! I   外気導入状態フラグ、=0:カット中
                                       !     =1:導入中、=2:導入開始、=3:導入停止
      REAL        SMRT1(NAZ,NHR)       ! I   面積を持たない部位からの冷房負荷
      REAL        SMRT2(NAZ,NHR)       ! I   INFLの吸熱応答係数
      REAL        VOAG(NZ)             ! I   導入時の外気量
      INTEGER     LCG(NZ)              ! I   XMQ配列のSPACデータへのポインタ（L）
      REAL        CLDG(NAZ,NHR,NSL)    ! I   冷房負荷
      INTEGER     NWD                  ! I   WDの整合寸法（=7）
      REAL        WD(NWD,NHR)          ! I   外界気象（基準温湿度からの偏差ではない）
      REAL        REFWD(NSL)           ! I   基準温湿度
      REAL        P0(NAZ,0:1,NHR,NSL)  ! I   瞬時蓄熱応答係数（吸熱される側が正）
                                       !     第2添字=0:二等辺三角
                                       !     第2添字=1:右側直角二等辺三角
      INTEGER     NSTP                 ! I   予熱時間（ステップ）
      REAL        VFLOW(NAZ,NAZ,NHR)   ! I   第1添字目のゾーンから第2添字目のゾーンへの流入
                                       !     風量（体積流量、0以上、対角項は0とする）
      REAL        EXCAP(NAZ,NSL)       ! I   各スペースの装置容量（冷却、0以上）
      REAL        SPCAP(NAZ,NSL)       ! I   各スペースの装置容量（加熱、0以上）
      REAL        RMMX(NAZ,NSL)        ! I   各スペースの設定温湿度上限
      REAL        RMMN(NAZ,NSL)        ! I   各スペースの設定温湿度下限
                                       !     （RMMX,RMMNは基準温湿度からの偏差ではない）
      INTEGER     NITER                ! I   収束計算における許容繰り返し計算数
      INTEGER     NUOT1                ! I   テキスト出力ファイルの装置番号（最初の装置番号）
      INTEGER     ID(3)                ! I   年・月・日（出力情報）
      INTEGER     MDW                  ! I   曜日（出力情報）
      INTEGER     MODE                 ! I   =1:助走、=2:本計算、=3:最終日
      INTEGER     IOUT                 ! I   =0:簡易出力(1h1行)、=1:詳細出力(1h2行)
      INTEGER     LSZSPC(0:4)          ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
                                       !     (3):WNDW, (4):INFL の変数の数
      INTEGER     IBECS                ! I   BECSへの受け渡しデータを、=1:出力する, 0:出力しない
      INTEGER     NUOB                 ! I   BECSへの受け渡しファイル用装置番号

C     ローカル変数
      REAL        CRHO(NSL)            !     空気の容積比熱
                                       !     （潜熱の場合は密度に蒸発潜熱を掛けたもの）
      INTEGER     NTRM(NSL)            !     蓄熱応答係数の項数
      PARAMETER ( MTRM=2 )             !     蓄熱応答係数の項数
      INTEGER     LSTP(MTRM,NSL)       !     蓄熱応答係数（瞬時分を除く）へのポインタ
      INTEGER     LSTQ(MTRM,NSL)       !     蓄熱負荷（瞬時分を除く）へのポインタ
      INTEGER     ISL                  !     =1:顕熱、=2:潜熱
      INTEGER     KSTP                 !     予熱開始後の経過ステップ
      INTEGER     JHR                  !     時刻
      INTEGER     IWARM                !     そのステップが予熱中(=1)かそれ以外(=0)か
      INTEGER     JHR0                 !     予熱を開始した時刻
      INTEGER     ICHNG                !     そのステップでいずれかのゾーンで階段状の変化
                                       !     （空調発停、外気量の変化）があったかどうか
      INTEGER     ISTOP                !     そのステップで全てのゾーンで空調停止となるか
      PARAMETER ( MSTP=24 )            !     許容最大予熱ステップ数
      INTEGER     IDLT(MSTP)           !     予熱開始後、各ステップにおいて直後室温湿度を
                                       !     =0：未知数としない、=1：未知数とする
      INTEGER     IZ                   !     現在のゾーンが何ゾーン目か（1<=IZ<=NZ）
      INTEGER     IREP                 !     現在の時刻ステップの直前か直後か
                                       !     =0：直前あるいは二等辺三角、=1：直後
      PARAMETER ( MZ=20 )              !     1グループあたりの最大ゾーン数
      PARAMETER ( MHR=24 )             !     整合寸法（=NHR）
      REAL        RMSET(MZ)            !     各ゾーンの一定除去熱量計算時設定温湿度
      REAL        VOAWK(MZ,0:1,MHR)    !     各ゾーンの外気量（体積流量、0以上）
      REAL        OATX(MZ,MHR)         !     導入外気温湿度（外調機考慮、基準温湿度からの
                                       !     偏差ではない）
      INTEGER     IOPTWK(MZ)           !     各ゾーン・各段の空調運転状態（=1:運転、=0:停止）
      INTEGER     IWARM1               !     現在の段（直前･直後を区別する）が予熱時間帯か
      REAL        FIXEDL(MZ)           !     各ゾーンの、未知変数に依存しない固定流入熱量
      INTEGER     LSTPWK
      INTEGER     LSTQWK
      REAL        PV(0:MSTP-1,MZ)      !     予熱開始後各ステップにおける蓄熱応答係数
                                       !     （二等辺三角、吸熱側が正）
      REAL        PR(0:MSTP,MZ)        !     予熱開始後各ステップにおける蓄熱応答係数
                                       !     （右側直角三角、吸熱側が正）
                                       !     PV(0),PR(0)は現在時刻のもの
      INTEGER     I
      INTEGER     J
      INTEGER     NSIZE                !     方程式の数（未知数の数）
      INTEGER     IPS(0:MSTP)          !     予熱開始後、各時刻ステップの室温湿度が、未知ベク
                                       !     トルの何次元目から始まるか（=IPS+1次元目から）
      INTEGER     LMODE(MZ,0:1,MHR,NSL)!     各ゾーンの負荷状態モード（-2：暖房過負荷,
                                       !     -1：暖房軽負荷,0：無負荷,1：冷房軽負荷,
                                       !     2：冷房過負荷,9：停止）
      REAL        AN(MZ,0:1,MHR,NSL)   !     各ゾーンの装置除去熱量
      REAL        RM(MZ,0:1,MHR,NSL)   !     各ゾーンの室温湿度（基準温湿度からの偏差）
      REAL        EOA                  !     各ゾーンの外気負荷（実室温湿度基準）
      REAL        RN(MZ,0:1,MHR,NSL)   !     各ゾーンの室除去熱量
      REAL        AMRT(MZ,0:1,MHR)     !     MRT
      PARAMETER ( NA=100 )             !     許容最大方程式数（未知数の数）
      REAL*8      AA(NA,NA)            !     Work Array （連立方程式左辺係数行列）
      REAL*8      BB(NA)               !     Work Array （連立方程式右辺係数ベクトル）
      INTEGER     IP(NA)               !     Work Array
      INTEGER     INFO
      REAL*8      WK(MTRM)             !     Work Array
      REAL        FF
      REAL        EOUT(4,NSL)
      REAL        EMRT
      INTEGER     LMODEB(NSL)
      INTEGER     LC
      INTEGER     NSTP1
      INTEGER     L
      INTEGER     IWL
      INTEGER     JZ
      INTEGER     ISTAT

      DATA CRHO   /0.288, 0.720/       !     単位依存
      DATA NTRM   /2, 1/
      DATA LSTP   /17,20, 27,0/        !     潜熱の2項目はダミー（XMQ配列変更時注意）
      DATA LSTQ   /23,24, 30,0/        !     潜熱の2項目はダミー（XMQ配列変更時注意）

      IF(NZ.GT.NAZ) CALL ERROR2(139,2)
      IF(NZ.GT.MZ) CALL ERROR2(138,2)
      IF(NSTP.GT.MSTP) CALL ERROR2(137,2)
      IF((IPEAK.EQ.1).AND.(NSTP.LT.0)) CALL ERROR2(136,2)
      IF(NHR.NE.MHR) CALL ERROR2(135,2)

      DO ISL = 1, NSL   ! 顕熱・潜熱 loop

         DO IZ = 1, NZ
            IF ( ISEAS.EQ.2 ) THEN
               RMSET(IZ) = RMMN(IZ,ISL)
            ELSE
               RMSET(IZ) = RMMX(IZ,ISL)
            END IF
         END DO
         KSTP = NSTP
         NSTP1 = NSTP

         DO JHR = 1, NHR   ! 時刻ループ

C           導入外気温湿度（外調機考慮） (OUT: OATX)
            DO IZ = 1, NZ
               L = M(LCG(IZ)+202)   ! OAHUデータへのポインタ(L)
               IF ( L.EQ.0 ) THEN   ! OAHUデータが指定されていない場合
                  OATX(IZ,JHR) = WD(ISL,JHR)
               ELSE
                  OATX(IZ,JHR) = X(L+80+(ISL-1)*25+JHR)
               END IF
            END DO

C           予熱ステップか（その場合の予熱後ステップ数）（OUT: IWARM, KSTP）
            IF ( (IPEAK.EQ.1).AND.(ISEAS.LE.2).AND.   ! ピーク計算モードで夏期・冬期で
     -           (IOPTG(1,JHR).NE.0) ) THEN   ! かつ、空調稼動ステップである

               IF ( (IOPTG(1,JHR).EQ.2).AND.(NSTP.GE.1) ) THEN
               ! 一定除去熱量の計算ステップである（予熱開始時刻）
               ! このグループの全てのゾーンで予熱時間が同じでないとダメ
                  IF ( JHR.EQ.NHR ) THEN   ! 24時(0時)の起動に対するピーク計算は未対応
                     CALL ERROR2(120,1)
                  ELSE
                     KSTP = 0
                     IWARM = 1
                     JHR0 = JHR   ! 予熱開始時刻の記録
                     NSTP1 = NSTP
                     DO J = 1, NSTP-1
                        IF ( JHR+J.EQ.NHR ) THEN   ! 予熱終了前に24時に達する
                           CALL ERROR2(121,0)
                           NSTP1 = J
                           GO TO 100
                        ELSE
                           DO IZ = 1, NZ
                              IF ( IOPTG(IZ,JHR+J).EQ.3 ) THEN
                              ! 予熱終了前に空調停止する
                                 CALL ERROR2(122,0)
                                 NSTP1 = J
                                 GO TO 100
                              END IF
                           END DO
                        END IF
                     END DO
  100                CONTINUE
                  END IF
               ELSE IF ( (KSTP.GE.0).AND.(KSTP.LT.NSTP1) ) THEN
               ! 一定除去熱量の計算ステップである（予熱開始時刻以外）
                  KSTP = KSTP + 1
                  IWARM = 1
               ELSE
               ! シミュレーション計算を行うステップである
                  IWARM = 0
               END IF

            ELSE
            ! シミュレーション計算を行うステップである
               IWARM = 0
            END IF

C           いずれかのゾーンで階段状変化が生じるか(OUT: ICHNG, ISTOP, IDLT)
            ICHNG = 0
            ISTOP = 1
            DO IZ = 1, NZ
               IF ( (IOPTG(IZ,JHR).GE.2).OR.   ! 空調起動あるいは停止時刻、
     -              (IOPVG(IZ,JHR).GE.2) ) THEN   ! 外気導入開始・終了時刻
                  ICHNG = 1
               END IF

               IF ( IOPTG(IZ,JHR).NE.3 ) THEN
                  ISTOP = 0   ! 全てのゾーンで空調停止となるわけではない
               END IF
            END DO

            IF ( (IWARM.EQ.1).AND.(KSTP.GE.1) ) THEN
               IF ( (ICHNG.EQ.1).AND.(ISTOP.EQ.0) ) THEN
                  IDLT(KSTP) = 1
               ELSE
                  IDLT(KSTP) = 0
               END IF
            END IF

            DO IREP = 0, 1   ! 直前・直後 loop

               DO IZ = 1, NZ
C                 外気導入量（VOAWK(NZ,IREP,JHR)）
                  IF ( (IOPVG(IZ,JHR).EQ.2).AND.(IREP.EQ.0) ) THEN
                     VOAWK(IZ,IREP,JHR) = 0.0
                  ELSE IF ( (IOPVG(IZ,JHR).EQ.3).AND.(IREP.EQ.1) ) THEN
                     VOAWK(IZ,IREP,JHR) = 0.0
                  ELSE IF (IOPVG(IZ,JHR).GE.1) THEN
                     VOAWK(IZ,IREP,JHR) = VOAG(IZ)
                  ELSE
                     VOAWK(IZ,IREP,JHR) = 0.0
                  END IF

C                 空調運転状態（IOPTWK(NZ))
                  IF ( (IOPTG(IZ,JHR).EQ.2).AND.(IREP.EQ.0) ) THEN
                     IOPTWK(IZ) = 0
                  ELSE IF ( (IOPTG(IZ,JHR).EQ.3).AND.(IREP.EQ.1) ) THEN
                     IOPTWK(IZ) = 0
                  ELSE IF ( IOPTG(IZ,JHR).GE.1 ) THEN
                     IOPTWK(IZ) = 1
                  ELSE
                     IOPTWK(IZ) = 0
                  END IF
               END DO

C              一定除去熱量計算中か（IWARM1）
               IF ( IWARM.EQ.1 ) THEN
                  IF  ( (KSTP.EQ.0).AND.(IREP.EQ.0) ) THEN
                     IWARM1 = 0
                  ELSE IF ( (KSTP.EQ.NSTP1).AND.
     -               (IDLT(KSTP).EQ.0).AND.(IREP.EQ.1) ) THEN
                     IWARM1 = 0
                  ELSE
                     IWARM1 = 1
                  END IF
               ELSE
                  IWARM1 = 0
               END IF

C              FIXEDL(NZ)のセット
               DO IZ = 1, NZ
                  FIXEDL(IZ) = CLDG(IZ,JHR,ISL)
     -            + CRHO(ISL)*VOAWK(IZ,IREP,JHR)
     -               *( OATX(IZ,JHR) - REFWD(ISL) )
                  DO I = 1, NTRM(ISL)
                     FIXEDL(IZ) = FIXEDL(IZ) + X(LCG(IZ)+LSTQ(I,ISL))
                  END DO
                  IF ( ISL.EQ.1 ) THEN
                     L = LCG(IZ) + LSZSPC(0)
                     DO IWL = 1, 9999
                        CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                        IF ( ISTAT.NE.1 ) THEN
                           GO TO 200
                        ELSE
                           FIXEDL(IZ) = FIXEDL(IZ) + X(L+11) + X(L+12)
                           L = L + LSZSPC(M(L))
                        END IF
                     END DO
                     CALL ERROR2(133,2)
                  END IF
  200             CONTINUE

                  ! 直前の室温湿度変動による直後の流入熱を加算する
                  IF ( ( (IWARM1.EQ.0).AND.(IREP.EQ.1) ).OR.
     -                 ( (IWARM1.EQ.1).AND.(KSTP.EQ.0) ) ) THEN
                     IF(IREP.EQ.0) CALL ERROR2(134,2)
                     FIXEDL(IZ) = FIXEDL(IZ)
     -               - ( P0(IZ,0,JHR,ISL) - P0(IZ,1,JHR,ISL) )
     -                  *RM(IZ,0,JHR,ISL)

                     IF ( ISL.EQ.1 ) THEN
                        L = LCG(IZ) + LSZSPC(0)
                        DO IWL = 1, 9999
                           CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                           IF ( ISTAT.NE.1 ) THEN
                              GO TO 300
                           ELSE
                              FIXEDL(IZ) = FIXEDL(IZ) + ( X(L+3) -
     -                           X(L+4) )*RM(JZ,0,JHR,ISL)
                              L = L + LSZSPC(M(L))
                           END IF
                        END DO
                     END IF
  300                CONTINUE

                  END IF
               END DO

C              一定除去熱量を計算する段である
               IF ( IWARM1.EQ.1 ) THEN

C                 前処理（OUT: PV(0:NSTP1-1,NZ), PR(0:NSTP1,NZ), 添字0を除く）
                  IF ( KSTP.EQ.0 ) THEN
                     DO IZ = 1, NZ
                        DO J = 1, NSTP1
                           PR(J,IZ) = 0.0
                           IF ( J.LE.NSTP1-1 ) THEN
                              PV(J,IZ) = 0.0
                           END IF
                           DO I = 1, NTRM(ISL)
                              LSTPWK = LCG(IZ) + LSTP(I,ISL)
                              PR(J,IZ) = PR(J,IZ)
     -                           + X(LSTPWK+1)*X(LSTPWK+2)**(J-1)
                              IF ( J.LE.NSTP1-1 ) THEN
                                 PV(J,IZ) = PV(J,IZ) + X(LSTPWK)
     -                                 *X(LSTPWK+2)**(J-1)
                              END IF
                           END DO
                        END DO
                     END DO

                     ! 係数行列の0クリア
                     DO I = 1, NA
                        DO J = 1, NA
                           AA(I,J) = 0.0D0
                        END DO
                        BB(I) = 0.0D0
                     END DO
                     IPS(KSTP) = NZ
                     NSIZE = 0
                  END IF   ! 前処理

C                 方程式に係数を加算する
                  IF ( (IREP.EQ.0).OR.(ICHNG.EQ.1) ) THEN
                     DO IZ = 1, NZ
                        PR(0,IZ) = P0(IZ,1,JHR,ISL)
                        PV(0,IZ) = P0(IZ,0,JHR,ISL)
                        CALL COEFFP(IZ,KSTP,NZ,IREP,NSTP1,IDLT,
     -                  VFLOW(1,IZ,JHR),PV(0,IZ),PR(0,IZ),CRHO(ISL),
     -                  VOAWK(IZ,IREP,JHR),FIXEDL(IZ),RMSET,REFWD(ISL),
     -                  ISL,LCG(IZ)+LSZSPC(0),LSZSPC,NA,NSIZE,AA,BB,IPS)
                     END DO
                  END IF

C                 予熱開始直前までの室温湿度変動による蓄熱負荷を更新する
                  IF ( IREP.EQ.1 ) THEN
                     DO IZ = 1, NZ
                        DO J = 1, NTRM(ISL)
                           LSTPWK = LCG(IZ) + LSTP(J,ISL)
                           LSTQWK = LCG(IZ) + LSTQ(J,ISL)
                           IF ( KSTP.EQ.0 ) THEN
                              X(LSTQWK) = X(LSTPWK+2)*X(LSTQWK)
     -                        - ( X(LSTPWK) - X(LSTPWK+1) )
     -                           *RM(IZ,0,JHR,ISL)
                           ELSE
                              X(LSTQWK) = X(LSTPWK+2)*X(LSTQWK)
                           END IF
                        END DO

                        IF ( ISL.EQ.1 ) THEN
                           L = LCG(IZ) + LSZSPC(0)
                           DO IWL = 1, 9999
                              CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                              IF ( ISTAT.NE.1 ) THEN
                                 GO TO 400
                              ELSE
                                 IF ( KSTP.EQ.0 ) THEN
                                    X(L+11) = X(L+7)*X(L+11)
     -                                + (X(L+5)-X(L+6))*RM(JZ,0,JHR,ISL)
                                    X(L+12) = X(L+10)*X(L+12)
     -                                + (X(L+8)-X(L+9))*RM(JZ,0,JHR,ISL)
                                 ELSE
                                    X(L+11) = X(L+7)*X(L+11)
                                    X(L+12) = X(L+10)*X(L+12)
                                 END IF
                                 L = L + LSZSPC(M(L))
                              END IF
                           END DO
                        END IF
  400                   CONTINUE

                     END DO
                  END IF

C                 予熱終了段であり方程式を解く
                  IF ( (KSTP.EQ.NSTP1).AND.
     -               ( (IDLT(KSTP).EQ.0).OR.(IREP.EQ.1) ) ) THEN
                     IF(NSIZE.GT.NA) CALL ERROR2(124,1)

C                    方程式を解く
                     CALL DGESV(NSIZE,1,AA,NA,IP,BB,NA,INFO)
                     IF(INFO.NE.0) CALL ERROR2(132,2)

C                    予熱終了時の後処理（負荷等の抽出と蓄熱負荷の更新）
                     CALL POSTP(MZ,NZ,LCG,NSTP1,IDLT,JHR0,NHR,NSIZE,BB,
     -               IPS,CRHO(ISL),VOAWK,OATX,REFWD(ISL),ISL,
     -               RMSET,NTRM(ISL),LSTP(1,ISL),LSTQ(1,ISL),
     -               NAZ,VFLOW,SMRT1,SMRT2,LSZSPC,
     -               LMODE(1,0,1,ISL),AN(1,0,1,ISL),RM(1,0,1,ISL),
     -               RN(1,0,1,ISL),AMRT,WK)

                  END IF

C              シミュレーションを行う段である
               ELSE

C                 ! （OUT: LMODE,AN,RM,RN）
                  IF ( ( ICHNG.EQ.0 ).AND.( IREP.EQ.1 ) ) THEN
                  ! 階段状変化がなく直後の計算が不要である

                     DO IZ = 1, NZ
                        LMODE(IZ,IREP,JHR,ISL) = LMODE(IZ,0,JHR,ISL)
                        AN(IZ,IREP,JHR,ISL) = AN(IZ,0,JHR,ISL)
                        RM(IZ,IREP,JHR,ISL) = RM(IZ,0,JHR,ISL)
                        RN(IZ,IREP,JHR,ISL) = RN(IZ,0,JHR,ISL)
                     END DO

                  ELSE

                     ! 熱平衡式を解く
                     CALL SLVSM(NZ,IOPTWK,EXCAP(1,ISL),SPCAP(1,ISL),NAZ,
     -               VFLOW(1,1,JHR),P0(1,IREP,JHR,ISL),CRHO(ISL),
     -               VOAWK(1,IREP,JHR),RMMX(1,ISL),RMMN(1,ISL),
     -               REFWD(ISL),IPEAK,ISEAS,NITER,FIXEDL,
     -               ISL,IREP,LCG,LSZSPC,
     -               LMODE(1,IREP,JHR,ISL),AN(1,IREP,JHR,ISL),
     -               RM(1,IREP,JHR,ISL),NA,AA,BB,IP)

                     ! 外気負荷・室負荷を求める
                     DO IZ = 1, NZ
                        EOA = CRHO(ISL)
     -                       *VOAWK(IZ,IREP,JHR)*( OATX(IZ,JHR)
     -                             - (RM(IZ,IREP,JHR,ISL)+REFWD(ISL)) )
                        RN(IZ,IREP,JHR,ISL) = AN(IZ,IREP,JHR,ISL) - EOA
                     END DO
                  END IF

C                 MRTの計算
                  IF ( ISL.EQ.1 ) THEN
                     DO IZ = 1, NZ
                        CALL CLCMRT(NZ,VFLOW(1,IZ,JHR),MZ,
     -                    RM(1,0,JHR,ISL),
     -                    IZ,IREP,CRHO(ISL),SMRT1(IZ,JHR),SMRT2(IZ,JHR),
     -                    LCG(IZ),RN(IZ,IREP,JHR,ISL),AMRT(IZ,IREP,JHR))
                     END DO
                  END IF

C                 蓄熱負荷の更新
                  IF ( IREP.EQ.1 ) THEN
                     DO IZ = 1, NZ
                        DO J = 1, NTRM(ISL)
                           LSTPWK = LCG(IZ) + LSTP(J,ISL)
                           LSTQWK = LCG(IZ) + LSTQ(J,ISL)
                           X(LSTQWK) = X(LSTPWK+2)*X(LSTQWK)
     -                     - ( X(LSTPWK) - X(LSTPWK+1) )
     -                        *RM(IZ,0,JHR,ISL)
     -                     - X(LSTPWK+1)*RM(IZ,1,JHR,ISL)
                        END DO

                        IF ( ISL.EQ.1 ) THEN
                           L = LCG(IZ) + LSZSPC(0)
                           DO IWL = 1, 9999
                              CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                              IF ( ISTAT.NE.1 ) THEN
                                 GO TO 500
                              ELSE
                                 X(L+11)=X(L+7)*X(L+11)
     -                              +(X(L+5)-X(L+6))*RM(JZ,0,JHR,ISL)
     -                              +X(L+6)*RM(JZ,1,JHR,ISL)
                                 X(L+12)=X(L+10)*X(L+12)
     -                              +(X(L+8)-X(L+9))*RM(JZ,0,JHR,ISL)
     -                              +X(L+9)*RM(JZ,1,JHR,ISL)
                                 L = L + LSZSPC(M(L))
                              END IF
                           END DO
                        END IF
  500                   CONTINUE

                     END DO
                  END IF

               END IF

            END DO   ! 直前・直後 loop
         END DO   ! 時刻 loop
      END DO   ! 顕熱・潜熱 loop


C     計算結果の出力
      DO IZ = 1, NZ   ! ゾーンループ
         LC = LCG(IZ)
         FF = 1.163/X(LC+2)   ! Kcal/h から W/m2 への換算係数
         DO JHR = 1, NHR   ! 時刻ループ
            DO IREP = 0, 1   ! 直前・直後ループ
               IF ( IOUT.EQ.1 ) THEN
                  IF ( MODE.GE.2 ) THEN
                     WRITE(NUOT1-1+IZ,'(I4,'','',2(I2,'',''),I1,'','',
     -               I2,'','',I1,'','',2(F6.2,'','',3(F6.1,'',''),I2,
     -               '',''),F6.2)') ( ID(J), J = 1, 3 ),
     -               MDW, JHR, IREP, ( RM(IZ,IREP,JHR,ISL)+REFWD(ISL),
     -               CLDG(IZ,JHR,ISL)*FF, RN(IZ,IREP,JHR,ISL)*FF,
     -               AN(IZ,IREP,JHR,ISL)*FF,
     -               LMODE(IZ,IREP,JHR,ISL), ISL = 1, NSL ),
     -               AMRT(IZ,IREP,JHR)+REFWD(1)
                  END IF
               ELSE   ! 簡易出力（1時間分の平均の出力）
                  DO ISL = 1, NSL
                     EOUT(1,ISL) = RM(IZ,IREP,JHR,ISL)+REFWD(ISL)
                     EOUT(2,ISL) = CLDG(IZ,JHR,ISL)*FF
                     EOUT(3,ISL) = RN(IZ,IREP,JHR,ISL)*FF
                     EOUT(4,ISL) = AN(IZ,IREP,JHR,ISL)*FF
                     LMODEB(ISL) = LMODE(IZ,IREP,JHR,ISL)
                  END DO
                  EMRT = AMRT(IZ,IREP,JHR)+REFWD(1)
                  IF ( (IREP.EQ.0).AND.(MODE.GE.2) ) THEN   ! 平均を取って出力
                     DO ISL = 1, NSL
                        DO I = 1, 4
                           EOUT(I,ISL) = 0.5*( X(LC+86+(ISL-1)*7+I)
     -                                         + EOUT(I,ISL) )
                        END DO
                        IF ( (M(LC+93+(ISL-1)*7).EQ.9).AND.
     -                       (LMODEB(ISL).EQ.9) ) THEN
                           LMODEB(ISL) = 9
                        ELSE IF ( (M(LC+93+(ISL-1)*7).NE.9).AND.
     -                            (LMODEB(ISL).NE.9) ) THEN
                           LMODEB(ISL) = 10
                        ELSE
                           CALL ERROR2(131,2)
                        END IF
                     END DO
                     WRITE(NUOT1-1+IZ,'(I4,'','',2(I2,'',''),I1,'','',
     -               I2,'','',I1,'','',2(F6.2,'','',3(F6.1,'',''),I2,
     -               '',''),F6.2)') ( ID(J), J = 1, 3 ),
     -               MDW, JHR, IREP, ( ( EOUT(I,ISL), I = 1, 4 ),
     -               LMODEB(ISL), ISL = 1, NSL ), 0.5*(X(LC+92)+EMRT)
                  ELSE IF ( IREP.EQ.1 ) THEN   ! 次ステップのために記憶
                     DO ISL = 1, NSL
                        DO I = 1, 4
                           X(LC+86+(ISL-1)*7+I) = EOUT(I,ISL)
                        END DO
                        M(LC+93+(ISL-1)*7) = LMODEB(ISL)
                     END DO
                     X(LC+92) = EMRT
                  END IF
               END IF
            END DO
         END DO
      END DO

C     BECSへの出力
      IF(IBECS.EQ.1) THEN
         CALL OUTBECS(NUOB,NHR,NZ,LCG,NAZ,IOPTG,MZ,MHR,RN)
      END IF

C     導入外気量の外調機別加算
      DO IZ = 1, NZ   ! ゾーンループ
         L = M(LCG(IZ)+202)   ! OAHUデータへのポインタ(L)
         IF ( L.NE.0 ) THEN   ! OAHUデータが指定されている場合
            DO JHR = 1, NHR   ! 時刻ループ
               DO IREP = 0, 1   ! 直前・直後ループ
                  X(L+131+(JHR-1)*2+IREP) = X(L+131+(JHR-1)*2+IREP)
     -               + VOAWK(IZ,IREP,JHR)
               END DO
            END DO
         END IF
      END DO

      RETURN
      END
C-----------------------------------------------------------------------
C     BECS用の1日分の出力（日付および外気温湿度を除く）
C     1日の最大ステップ数は24
C     LATEST REVISION   2011.09.19
C                       2011.10.15 コメントの修正のみ
C-----------------------------------------------------------------------
      SUBROUTINE OUTBECS(NUOB,NHR,NZ,LCG,NAZ,IOPTG,MZ,MHR,RN)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

      PARAMETER ( NSL=2 )              !     顕熱と潜熱

C     引数
      INTEGER     NUOB                 ! I   BECSへの受け渡しファイル用装置番号
      INTEGER     NHR                  ! I   1日のステップ数
      INTEGER     NZ                   ! I   現在のグループのゾーン数
      INTEGER     LCG(NZ)              ! I   XMQ配列のSPACデータへのポインタ（L）
      INTEGER     NAZ                  ! I   グループあたりの最大ゾーン数を表わす整合寸法
      INTEGER     IOPTG(NAZ,NHR)       ! I   空調運転状態フラグ、=0:停止中、
                                       !     =1:運転中、=2:起動、=3:停止
      INTEGER     MZ                   ! I   グループあたりの最大ゾーン数を表わす整合寸法
      INTEGER     MHR                  ! I   1日の最大ステップ数を表わす整合寸法
      REAL        RN(MZ,0:1,MHR,NSL)   ! I   各ゾーンの室除去熱量
C     ローカル変数
      INTEGER     JSTART(2)            !     起動時刻
      INTEGER     JSTOP(2)             !     停止時刻
      CHARACTER   QSP*4                !     スペース名
      PARAMETER(  MHR2=24 )            !     1日の最大ステップ数
      REAL        QLOAD(MHR2)          !     室顕熱（潜熱）負荷[kcal/h]

      IF(NHR.GT.MHR2) CALL ERROR2(180,2)

C     ゾーンループ
      DO IZ = 1, NZ
         LC = LCG(IZ)
         FF = 1.0/X(LC+2)   ! kcal/h から kcal/m2h への換算係数

         ! 起動・停止時刻、スペース名の抽出と出力
         ISTART = 0   ! いくつ目の起動か
         ISTOP  = 0   ! いくつ目の停止か
         DO I = 1, 2
            JSTART(I) = 0
            JSTOP(I) = 0
         END DO
         DO JHR = 1, NHR   ! 時刻ループ
            IF(IOPTG(IZ,JHR).EQ.2) THEN   ! 起動
               ISTART = ISTART + 1
               IF(ISTART.LE.2) THEN
                  JSTART(ISTART) = JHR
               END IF
            ELSE IF(IOPTG(IZ,JHR).EQ.3) THEN   ! 停止
               ISTOP = ISTOP + 1
               IF(ISTOP.LE.2) THEN
                  JSTOP(ISTOP) = JHR
               END IF
            END IF
         END DO
         CALL NAME(QSP,M(LC+1))
         WRITE(NUOB,'(2(I3,A,I3,A),1X,A)')
     -          (JSTART(I), '00', JSTOP(I), '00', I = 1, 2), QSP

         ! 室顕熱・潜熱のデータの生成（正時）と出力
         DO ISL = 1, NSL   ! 顕熱・潜熱ループ
            DO JHR = 1, NHR
               QLOAD(JHR) = 0.0
               IF(IOPTG(IZ,JHR).EQ.2) THEN        ! 起動
                  QLOAD(JHR) = RN(IZ,1,JHR,ISL)      ! 正時直後の値
               ELSE IF(IOPTG(IZ,JHR).EQ.3) THEN   ! 停止
                  QLOAD(JHR) = RN(IZ,0,JHR,ISL)      ! 停止直前の値
               ELSE
                  QLOAD(JHR) = 0.5*(RN(IZ,0,JHR,ISL) + RN(IZ,1,JHR,ISL)) ! 直前と直後の平均
               END IF
            END DO
            WRITE(NUOB,'(24E15.7)') (QLOAD(JHR)*FF, JHR = 1, NHR)
         END DO

      END DO

      RETURN
      END
