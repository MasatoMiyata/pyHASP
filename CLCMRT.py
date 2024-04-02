def CLCMRT(NZ,VFLOW,MZ,RM,IZ,IREP,CRHO,SMRT1,SMRT2,LC,RN,X,M):
    """簡易MRT計算

    # 引数
    INTEGER     NZ                  ! I   現在のグループのゾーン数
    REAL        VFLOW(NZ)           ! I   各ゾーンからの流入風量（体積流量、正値）
    INTEGER     MZ                  ! I   整合寸法（1グループあたりの最大ゾーン数）
    REAL        RM(MZ,0:1)          ! I   各ゾーンの室温（基準温度からの偏差） 第2添字の(0):直前、(1):直後
    INTEGER     IZ                  ! I   現在のゾーンが何ゾーン目か（1<=IZ<=NZ）
    INTEGER     IREP                ! I   現在の時刻ステップの直前か直後か =0：直前、=1：直後
    REAL        CRHO                ! I   空気の容積比熱
    REAL        SMRT1               ! I   面積を持たない部位からの冷房負荷
    REAL        SMRT2               ! I   INFLの吸熱応答係数
    INTEGER     LC                  ! I   XMQ配列のSPACデータへのポインタ（L）
    REAL        RN                  ! I   室除去熱量
    REAL        AMRT                ! O   MRT
    C     M(LC+63)                  ! I   表面積合計（OWAL+IWAL+GWAL+BECO+WNDW）
    C     M(LC+64)                  ! I    FURN蓄熱応答係数（顕熱）：P0 瞬時応答係数、二等辺三角波励振[kcal/h℃]
    C     M(LC+65)                  ! I    FURN蓄熱応答係数（顕熱）：P0R 瞬時応答係数、右側直角三角波励振[kcal/h℃]
    C     M(LC+66)                  ! I    FURN蓄熱応答係数（顕熱）：P1 1項目の1ステップ後の応答係数、二等辺三角波励振[kcal/h℃]
    C     M(LC+67)                  ! I    FURN蓄熱応答係数顕熱）：P1R 1項目の1ステップ後の応答係数、右側直角三角波励振[kcal/h℃]
    C     M(LC+68)                  ! I    FURN蓄熱応答係数（顕熱）：R1 1項目の公比[-]
    C     M(LC+69)                  ! I    FURN蓄熱応答係数（顕熱）：P2 2項目の1ステップ後の応答係数、二等辺三角波励振[kcal/h℃]
    C     M(LC+70)                  ! I    FURN蓄熱応答係数（顕熱）：P2R 2項目の1ステップ後の応答係数、右側直角三角波励振[kcal/h℃]
    C     M(LC+71)                  ! I    FURN蓄熱応答係数（顕熱）：R2 2項目の公比[-]
    C     M(LC+72)                  ! I/O  FURN蓄熱応答係数（顕熱）：Q1 前ステップまでの励振による現ステップでの1項目の蓄熱負荷 [kcal/h]
    C     M(LC+73)                  ! I/O  FURN蓄熱応答係数（顕熱）：Q2 前ステップまでの励振による現ステップでの2項目の蓄熱負荷 [kcal/h]
    """

    # 整数化
    LC = int(LC)

    # 対流熱伝達率 [kcal/m2hC](単位依存)
    HC = 3.5
    
    # 隣接ゾーンとの対流による熱流収支
    WK = 0.0

    for J in range(1, NZ+1):
        WK += VFLOW[J]*( RM[J,IREP] - RM[IZ,IREP] )

    # CRHO 空気の容積比熱
    WK = WK * CRHO

    # 室負荷のうち、面積を持たない部位からのもの
    RNNA = SMRT1 + X[LC+72] + X[LC+73] - SMRT2 * RM[IZ,IREP] + WK

    if ( IREP == 0 ):
        RNNA = RNNA - X[LC+64] * RM[IZ,IREP]
    else:
        RNNA = RNNA - ( X[LC+64] - X[LC+65] ) * RM[IZ,0] - X[LC+65] * RM[IZ,IREP]

    # 簡易MRT
    AMRT = ( RN - RNNA )/(HC * X[LC+63]) + RM[IZ,IREP]

    # print(f"--- CLCMRT RM[IZ,IREP]: {RM[IZ,IREP]} ---")
    # print(f"--- CLCMRT X[LC+63]: {X[LC+63]} ---")
    # print(f"--- CLCMRT RNNA: {RNNA} ---")
    # print(f"--- CLCMRT RN: {RN} ---")
    # print(f"--- CLCMRT AMRT: {AMRT} ---")

    # 家具熱容量による蓄熱負荷の更新
    if ( IREP == 1 ):
        
        X[LC+72] = X[LC+68]*X[LC+72] - ( X[LC+66] - X[LC+67] )*RM[IZ,0] - X[LC+67]*RM[IZ,1]
        X[LC+73] = X[LC+71]*X[LC+73] - ( X[LC+69] - X[LC+70] )*RM[IZ,0] - X[LC+70]*RM[IZ,1]

    return AMRT,X,M


# if __name__ == '__main__':

