import numpy as np
from RTVADJ import RTVADJ

def COEFFP(IZ,KSTP,NZ,IREP,NSTP,IDLT,VFLOW,PV,PR,CRHO,VOA,FIXEDL,RMSET,REFWD,ISL,LSTRT,LSZSPC,NA,II,AA,BB,IPS,X,M):
    """
    C     一定除去熱量計算のための方程式を作成する
    C     LATEST REVISION   2012.03.05
    C     ・ このルーチンは、予熱時間が終了するまで、他のグループなど
    C        から同時に呼ばれてはならない
    C     ・ 引数のうち出力（II, AA, BB,IPS）については予熱時間終了時に
    C        正しい値となる。それまで、親ルーチンにおいてこれらの
    C        変数を引用・定義してはならない。
    C     ・ 時間ループの中の、直前・直後ループの中の、
    C        ゾーンループの中で呼ばれることを想定している。

    C     引数
    INTEGER     IZ              ! I   現在のゾーンが何ゾーン目か（1<=IZ<=NZ）
    INTEGER     KSTP            ! I   予熱開始後何ステップ目か(0<=KSTP<=NSTP)
    INTEGER     NZ              ! I   現在のグループのゾーン数
    INTEGER     IREP            ! I   現在の時刻ステップの直前か直後か
                                !     =0：直前あるいは二等辺三角、=1：直後
    INTEGER     NSTP            ! I   予熱時間（NSTP>=1）
    INTEGER     IDLT(NSTP)      ! I   予熱開始後、各ステップにおいて直後室温湿度を
                                !     =0：未知数としない、=1：未知数とする
                                !     （KSTPを含んでそれ以前の値のみ引用される）
    REAL        VFLOW(NZ)       ! I   各ゾーンからの流入風量（体積流量、正値）
    REAL        PV(0:NSTP-1)    ! I   予熱開始後各ステップにおける蓄熱応答係数
                                !     （二等辺三角、吸熱側が正）
    REAL        PR(0:NSTP)      ! I   予熱開始後各ステップにおける蓄熱応答係数
                                !     （右側直角三角、吸熱側が正）
                                !     PV(0), PR(0)はそれぞれ現在時刻のものを入力する
    REAL        CRHO            ! I   空気の容積比熱（潜熱の場合は密度に蒸発潜熱を掛けたもの）
    REAL        VOA             ! I   現在のゾーンに対する外気量（体積流量、0以上）
    REAL        FIXEDL          ! I   現在のゾーンの、未知変数に依存しない固定流入熱量
    REAL        RMSET(NZ)       ! I   各ゾーンの設定温湿度（予熱終了後に必ず達成される）
                                ! I   （基準温湿度からの偏差ではない）
    REAL        REFWD           ! I   基準温湿度
    INTEGER     ISL             ! I   =1:顕熱、=2:潜熱
    INTEGER     LSTRT           ! I   SPACデータのうち、OWAL, IWAL等の先頭ポインタ(検索開始点)
    INTEGER     LSZSPC(0:4)     ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
                                !     (3):WNDW, (4):INFL の変数の数
    INTEGER     NA              ! I   行列AAの整合寸法
    INTEGER     II              ! I/O 未知数の数（行列のサイズ）
    REAL*8      AA(NA,NA)       ! I/O 連立方程式左辺係数行列
    REAL*8      BB(NA)          ! I/O 連立方程式右辺係数ベクトル
    INTEGER     IPS(0:NSTP)     ! I/O 予熱開始後、各時刻ステップの室温湿度が、未知ベクトルの
                                !     何次元目から始まるか（=IPS+1次元目から）

    C     ローカル変数
    INTEGER     I
    INTEGER     J
    REAL*8      VSUM            !     現在のゾーンに流入する風量の合計
    PARAMETER ( MZ=20 )         !     1グループあたり最大ゾーン数
    REAL        COEF(MZ)        !     現在のゾーンについての方程式のうち、添字目のゾーンの
                                !     現在の室温湿度に対する係数
    INTEGER     MSTP
    REAL*8      WK0, WK1
    INTEGER     L
    INTEGER     IWL
    INTEGER     JZ
    INTEGER     ISTAT
    """

    MZ = 20  # 1グループあたり最大ゾーン数
    COEF = np.zeros(MZ+1)

    if (NZ > MZ) or (IZ > NZ): 
        raise Exception("例外が発生しました")
    if (KSTP < 0) or (KSTP > NSTP):
        raise Exception("例外が発生しました")
    if (IREP != 0) and (IREP != 1):
        raise Exception("例外が発生しました")
    if (NSTP < 1):
        raise Exception("例外が発生しました")
    if (KSTP == 0) and (IREP == 0):
        raise Exception("例外が発生しました")

    # 行列要素のインデックスの更新
    if (IREP == 0) and (IZ == 1):
        if ( KSTP == 1 ):
            IPS[KSTP] = IPS[KSTP-1] + NZ   # 予熱開始直後のみ未知
        else:
            IPS[KSTP] = IPS[KSTP-1] + ( IDLT[KSTP-1] + 1 )*NZ
    II = II + 1
    if (II > NA): 
        raise Exception("例外が発生しました")

    # 現在のゾーンに流入する風量の合計
    VSUM = 0.0
    for J in range(1, NZ+1):
        VSUM = VSUM + VFLOW[J]
    
    # 現在の室温湿度（未知数）に対する係数（前処理）
    for J in range(1, NZ+1):
        if ( J == IZ ):
            if ( IREP == 1 ): # 現在が階段状変化直後である（予熱開始時刻を含む）
                COEF[J] = PR[0] + CRHO*( VOA + VSUM )
            else:
                COEF[J] = PV[0] + CRHO*( VOA + VSUM )
        else:
            COEF[J] = -CRHO*VFLOW[J]

    if ( ISL == 1 ):
        L = int(LSTRT)

        flag_RTVADJ = True
        while flag_RTVADJ:
            (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
            if ISTAT != 1:
                flag_RTVADJ = False
            else:
                if ( IREP == 1 ):
                    COEF[JZ] = COEF[JZ] - X[L+4]
                else:
                    COEF[JZ] = COEF[JZ] - X[L+3]
                L = L + int(LSZSPC[ int(M[L]) ])

    # 右辺ベクトルの初期セット
    BB[II] = FIXEDL

    # 行列の該当要素への代入

    # 一定除去熱量（未知数）の係数
    AA[II,IZ] = 1.0

    # 現在の時刻ステップの室温湿度に対する係数
    for MSTP in range(0, KSTP):
    
        if ( MSTP == KSTP ):
                
            if ( (KSTP >= 1) and (IREP == 1) ):

                # 階段状変化直後の場合（予熱開始直後を除く）
                AA[II,IPS[MSTP]+IZ] = PV[0] - PR[0]

                # 階段状変化直前の室温湿度に対する係数
                if ( ISL == 1 ):
                    L = LSTRT
                    flag_RTVADJ = True
                    while flag_RTVADJ:
                        (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                        if ISTAT != 1:
                            flag_RTVADJ = False
                        else:
                            AA[II,IPS[MSTP]+JZ] = AA[II,IPS[MSTP]+JZ] - ( X[L+3] - X[L+4] )
                            L = L + int( LSZSPC[ int(M[L]) ])

            if (MSTP == NSTP) and ((IDLT[NSTP] == 0) or (IREP == 1)):    # 予熱終了時刻で かつ 設定温湿度に達する側のステップ

                # 右辺ベクトルの修正
                WK1 = 0.0
                for J in range(1, NZ+1):
                    WK1 = WK1 + COEF[J] * ( RMSET[J] - REFWD )
                BB[II] = BB[II] - WK1
            else:
                for J in range(1, NZ+1):
                    if ( KSTP == 0 ):
                        AA[II, int(IPS(MSTP)+J) ] = COEF[J]
                    else:
                        AA[II, int(IPS(MSTP)+IREP*NZ+J) ] = COEF[J]
 
        # 予熱開始1時刻後から、1ステップ前までの室温湿度に対する係数（KSTP>=2の場合のみ）
        elif ( MSTP >= 1 ):

            AA[II,IPS[MSTP]+IZ] = PV[KSTP-MSTP] - PR[KSTP-MSTP]*IDLT[MSTP]

            if ( IDLT[MSTP] == 1 ):    # 予熱開始MSTP後に階段状変化が生じた
                AA[II, int(IPS[MSTP]+NZ+IZ) ] = PR[KSTP-MSTP]

            if ( ISL == 1 ):
                L = LSTRT

                flag_RTVADJ = True
                while flag_RTVADJ:
                    (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                    if ISTAT != 1:
                        flag_RTVADJ = False
                    else:

                        WK0 = X[L+5] * X[L+ 7]**(KSTP-MSTP-1) + X[L+8] * X[L+10]**(KSTP-MSTP-1)
                        WK1 = X[L+6] * X[L+ 7]**(KSTP-MSTP-1) + X[L+9] * X[L+10]**(KSTP-MSTP-1)
                        AA[II,IPS[MSTP]+JZ] = AA[II,IPS[MSTP]+JZ] - ( WK0 - WK1*IDLT[MSTP] )
                        if ( IDLT[MSTP] == 1 ):
                            AA[II, int(IPS[MSTP]+NZ+JZ)] = AA[II,int(IPS[MSTP]+NZ+JZ)] - WK1

                        L = L + int(LSZSPC[int(M[L])])

        else:   # 予熱開始直後の室温湿度に対する係数（KSTP>=1の場合のみ）

            AA[II,int(IPS[MSTP]+IZ)] = PR[KSTP]

            if ( ISL == 1 ):
                L = LSTRT

                flag_RTVADJ = True
                while flag_RTVADJ:
                    (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                    if ISTAT != 1:
                        flag_RTVADJ = False
                    else:
                        WK1 = X[L+6] * X[L+7]**(KSTP-1) + X[L+9]*X[L+10]**(KSTP-1)
                        AA[II, int(IPS(MSTP)+JZ)] = AA[II,int(IPS(MSTP)+JZ)] - WK1
                        L = L + int(LSZSPC[int(M[L])])


    if (KSTP == NSTP) and (IZ == NZ) and ((IDLT[NSTP] == 0) or (IREP == 1)):
        if (II != IPS[KSTP] + IDLT[KSTP]*NZ): 
            raise Exception("例外が発生しました")

    return II,AA,BB,IPS,X,M