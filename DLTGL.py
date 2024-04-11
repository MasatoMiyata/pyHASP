import numpy as np

def DLTGL(IOPGL,VV,XPULL,NVS0,VVB0,DSU0,NVS1,VVB1,DSU1):
    """WINDOW DATA INTERPOLATION
    ***************  ***************************
    *     LATEST REVISION     - 2006.03.30
    *     ARGUMENTS IOPGL      - I   1:AFW指定、2:PPW指定
    *               VV         - I   通気風量[L/m2s]
    *               XPULL      - I   窓排気率[-](IOPGL=2のときのみ引用)
    *               NVS0       - I   通気風量サンプリング数(>=1) (IOPGL=2のときはXPULL=0)
    *               VVB0(NVS0) - I   通気風量サンプリング値[L/m2s](同上)
    *               DSU0(NVS0) - I   ΔSC, ΔUのサンプリング値 (同上)
    *               NVS1       - I   通気風量サンプリング数(>=1) (IOPGL=2のときのみ引用、XPULL=1)
    *               VVB1(NVS1) - I   通気風量サンプリング値[L/m2s](同上)
    *               DSU1(NVS1) - I   ΔSC, ΔUのサンプリング値 (同上)
    *     REQ. ROUTINES       - NONE
    *********************************************************************
    """

    DSUWK = np.zeros(2+1)

    # VV: 通気風量[L/m2s]
    if (VV <= VVB0[1]):  # 補正値表の最小値より小さい場合
        DSUWK[0]= DSU0[1]

    elif (VV > VVB0[int(NVS0)]):  # 補正値表の最大値より大きい場合
        DSUWK[0] = DSU0[int(NVS0)]
    
    else:
        for I in range(2,NVS0+1):
            if (VV >= VVB0[I]):
                DSUWK[0] = ((VV-VVB0[I-1])*DSU0(I) +(VVB0[I]-VV)*DSU0[I-1]) / (VVB0[I]-VVB0[I-1])
                break
    
    # IOPGL =1:AFW、=2:PPW
    if (IOPGL == 1):
        DLTGL = DSUWK[0]
        return DLTGL

    if VV <= VVB1[1]:
        DSUWK[1] = DSU1[1]
    elif VV > VVB1[NVS1]:
        DSUWK[1] = DSU1[NVS1]
    else:
        for I in range(2,NVS1+1):
            DSUWK[1] = ((VV-VVB1[I-1])*DSU1[I] +(VVB1[I]-VV)*DSU1[I-1]) / (VVB1[I]-VVB1[I-1])
            break

    DLTGL = XPULL * DSUWK[1] + (1-XPULL)*DSUWK[0]

    return DLTGL

