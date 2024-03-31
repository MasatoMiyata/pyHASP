def CLCMRT(NZ,VFLOW,MZ,RM,IZ,IREP,CRHO,SMRT1,SMRT2,LC,RN,X,M):
    """簡易MRT計算

    # 引数
    INTEGER     NZ                  ! I   現在のグループのゾーン数
    REAL        VFLOW(NZ)           ! I   各ゾーンからの流入風量（体積流量、正値）
    INTEGER     MZ                  ! I   整合寸法（1グループあたりの最大ゾーン数）
    REAL        RM(MZ,0:1)          ! I   各ゾーンの室温（基準温度からの偏差）
                                    !     第2添字の(0):直前、(1):直後
    INTEGER     IZ                  ! I   現在のゾーンが何ゾーン目か（1<=IZ<=NZ）
    INTEGER     IREP                ! I   現在の時刻ステップの直前か直後か
                                    !     =0：直前、=1：直後
    REAL        CRHO                ! I   空気の容積比熱
    REAL        SMRT1               ! I   面積を持たない部位からの冷房負荷
    REAL        SMRT2               ! I   INFLの吸熱応答係数
    INTEGER     LC                  ! I   XMQ配列のSPACデータへのポインタ（L）
    REAL        RN                  ! I   室除去熱量
    REAL        AMRT                ! O   MRT
    C     M(LC+63)                  ! I   表面積合計
    C     M(LC+64)                  ! I   FURN 蓄熱応答係数（顕熱） PV0
    C     M(LC+65)                  ! I     PR0
    C     M(LC+66)                  ! I     PV1
    C     M(LC+67)                  ! I     PR1
    C     M(LC+68)                  ! I     R1
    C     M(LC+69)                  ! I     PV2
    C     M(LC+70)                  ! I     PR2
    C     M(LC+71)                  ! I     R2
    C     M(LC+72)                  ! I/O   Q1
    C     M(LC+73)                  ! I/O   Q2     
    """

    # 整数化
    LC = int(LC)

    # 対流熱伝達率 [kcal/m2hC](単位依存)
    HC = 3.5
    
    # 隣接ゾーンとの対流による熱流収支
    WK = 0.0
    for J in range(1, NZ+1):
        WK = WK + VFLOW[J]*( RM[J,IREP] - RM[IZ,IREP] )
    WK = WK*CRHO

    # 室負荷のうち、面積を持たない部位からのもの
    RNNA = SMRT1 + X[LC+72] + X[LC+73] - SMRT2*RM[IZ,IREP] + WK
    if ( IREP == 0 ):
        RNNA = RNNA - X[LC+64]*RM[IZ,IREP]
    else:
        RNNA = RNNA - ( X[LC+64] - X[LC+65] )*RM[IZ,0] - X[LC+65]*RM[IZ,IREP]

    # 簡易MRT
    AMRT = ( RN - RNNA )/(HC*X[LC+63]) + RM[IZ,IREP]

    # 家具熱容量による蓄熱負荷の更新
    if ( IREP == 1 ):
        X[LC+72] = X[LC+68]*X[LC+72] - ( X[LC+66] - X[LC+67] )*RM[IZ,0] - X[LC+67]*RM[IZ,1]
        X[LC+73] = X[LC+71]*X[LC+73] - ( X[LC+69] - X[LC+70] )*RM[IZ,0] - X[LC+70]*RM[IZ,1]

    return AMRT,X,M


# if __name__ == '__main__':

