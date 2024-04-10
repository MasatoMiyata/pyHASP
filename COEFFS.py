import numpy as np
from RTVADJ import RTVADJ
from mprint import mprint

def COEFFS(LMODE,NZ,IZ,RMMX,RMMN,GRADL,CRHO,VFLOW,FIXEDL,EXCAP1,SPCAP1,ISL,IREP,LSTRT,LSZSPC,NA,AA,X,M):
    """シミュレーション計算のための方程式を作成する

    # 引数
    INTEGER     LMODE          ! I   負荷状態モード（-2：暖房過負荷,-1：暖房軽負荷,
                                !     0：無負荷,1：冷房軽負荷,2：冷房過負荷,99：停止）
    INTEGER     NZ             ! I   現在のグループのゾーン数
    INTEGER     IZ             ! I   現在のゾーンが何ゾーン目か（1<=IZ<=NZ）
    REAL        RMMX           ! I   設定温湿度上限（基準温湿度からの偏差）
    REAL        RMMN           ! I   設定温湿度下限（基準温湿度からの偏差）
                                !     （RMMX,RMMNは基準温湿度からの偏差ではない）
    REAL        GRADL          ! I   現在のゾーンの単位温湿度変化に対する除去熱量の変化
                                !     （符合を逆にしたもの。いつも正のはず）
    REAL        CRHO           ! I   空気の容積比熱（潜熱の場合は密度に蒸発潜熱を掛けたもの）
    REAL        VFLOW(NZ)      ! I   各ゾーンからの流入風量（体積流量、正値）
    REAL        FIXEDL         ! I   現在のゾーンの、温湿度や除去熱量に依存しない固定流入熱量
    REAL        EXCAP1         ! I   装置容量（冷却）。0以上。
    REAL        SPCAP1         ! I   装置容量（加熱）。0以上。
    INTEGER     ISL            ! I   =1:顕熱、=2:潜熱
    INTEGER     IREP           ! I   現在の時刻ステップの直前か直後か
                                !     =0：直前あるいは二等辺三角、=1：直後
    INTEGER     LSTRT          ! I   SPACデータのうち、OWAL, IWAL等の先頭ポインタ(検索開始点)
    INTEGER     LSZSPC(0:4)    ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
                                !     (3):WNDW, (4):INFL の変数の数
    INTEGER     NA             ! I   行列AAの整合寸法
    REAL*8      AA(NA,NZ)      ! O   連立方程式左辺係数行列
    REAL*8      BB             ! O   連立方程式右辺係数ベクトルのIZ列目（IZゾーン目）

    # ローカル変数
    INTEGER     J   
    INTEGER     L
    INTEGER     IWL
    INTEGER     JZ
    INTEGER     ISTAT
    """

    if (NZ > NA) or (IZ > NZ): 
        raise Exception("想定外エラー")
    if (GRADL < 0): 
        raise Exception("想定外エラー")
    if (EXCAP1 < 0): 
        raise Exception("想定外エラー")
    if (SPCAP1 < 0): 
        raise Exception("想定外エラー")
    if (RMMN > RMMX): 
        raise Exception("想定外エラー")

    # 軽負荷モードのときは室温湿度＝設定値という式を立てる
    if ( LMODE == 1 ) or ( LMODE == -1 ):

        for J in range(1, NZ+1):
            if ( J == int(IZ) ):
                AA[int(IZ),J] = 1.0
            else:
                AA[int(IZ),J] = 0.0

        if ( LMODE == 1 ):
            BB = RMMX
        else:
            BB = RMMN

    else:   # その他の場合は熱平衡式を用いる

        for J in range(1, NZ+1):

            if ( J == IZ ):
                AA[int(IZ),J] = GRADL
            else:
                if (VFLOW[J] < 0): 
                    raise Exception("想定外エラー")
                
                AA[int(IZ),int(J)] = -CRHO*VFLOW[J]

        if ( ISL == 1 ): # 顕熱計算
            
            L = int(LSTRT)
        
            flag_RTVADJ = True
            while flag_RTVADJ:

                # L: IWALのポインタ
                # JZ: 隣接する内壁のポインタ
                # ISTAT: =1 IWAL(adjacent)が見つかった
                (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                
                if ISTAT != 1:

                    flag_RTVADJ = False
                
                else:

                    # ISTAT = 1: IWAL(adjacent)が見つかった場合
                    # X(L+3+0): 貫流応答係数（顕熱）P0 （瞬時応答係数、二等辺三角波励振[kcal/h℃]）
                    # X(L+4+1): 貫流応答係数（顕熱）P0R （瞬時応答係数、右側直角三角波励振[kcal/h℃]）

                    AA[int(IZ),int(JZ)] = AA[int(IZ),int(JZ)] - X[int(L+3+IREP)]

                    # mprint("COEFFS AA", AA)
                    # mprint("COEFFS NZ", NZ)
                    # mprint("COEFFS IZ", IZ)

                    L += int(LSZSPC[int(M[L])])

        mprint("COEFFS LMODE", LMODE)
        mprint("COEFFS FIXEDL", FIXEDL)
        mprint("COEFFS EXCAP1", EXCAP1)
        mprint("COEFFS SPCAP1", SPCAP1)
        mprint("COEFFS GRADL", GRADL)
        mprint("COEFFS CRHO", CRHO)
        mprint("COEFFS VFLOW", VFLOW)

        if LMODE == 2:
            BB = FIXEDL - EXCAP1
        elif LMODE == -2 :
            BB = FIXEDL + SPCAP1
        else:                    # 停止・あるいは無負荷
            BB = FIXEDL

    return AA,BB,X,M