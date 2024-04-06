import numpy as np
import math

from mprint import mprint

def GVECTR(QWL,NL,MT,TH,HT,HO,TCM=np.zeros([2,100])):
    """_summary_

    ★引数にリストがあるため参照渡しになる点に注意！

    Args:
        QWL (_type_): 識別子（文字列4）
        NL (_type_): 層の数
        MT (_type_): 建材番号のリスト
        TH (_type_): 長さのリスト
        HT (_type_): HC+HR
        HO (_type_): 建材データベースのファイル、H0=20 （QWLによって変わる）
        TCM (_type_, optional): 建材データベースを読みとった値. Defaults to np.zeros([2,100]).

    Returns:
        _type_: _description_
    """

    TRNS = np.zeros(10)
    ADMT = np.zeros(10)
    R = np.zeros(23)
    C = np.zeros(23)


    # 家具容量の応答に関する石野らの実験・調査データ（顕熱用）
    G0QC = [0.2893, 0.2518, 0.2311, 0.2119, 0.1790, 0.1794, 0.2187,
            0.2038, 0.1234, 0.0696, 0.0772, 0.0927, 0.0966, 0.0411,
            0.0465, 0.1398, 0.0741, 0.5120, 0.3653, 0.1982, 1.0000,
            0.2572]
    G1QC = [0.5579, 0.5260, 0.3129, 0.3720, 0.3038, 0.3176, 0.3820,
            0.5263, 0.4707, 0.3461, 0.2612, 0.3520, 0.2566, 0.2090,
            0.1535, 0.7415, 0.5722, 0.2513, 0.5668, 0.6222, 0.0000,
            1.6416]
    EPS =   [0.785 , 0.703 , 0.407 , 0.472 , 0.370 , 0.387 , 0.489 ,
            0.661 , 0.537 , 0.372 , 0.283 , 0.388 , 0.284 , 0.218 ,
            0.161 , 0.862 , 0.618 , 0.515 , 0.893 , 0.776 , 0.000 ,
            2.210]
    XN  =   [3.0  , 13.2  , 17.1  ,  7.7  ,  0.0  ,  9.8  ,  0.0  ,
            1.2  , 10.2  ,  5.3  ,  3.7  ,  1.4  , 11.4  ,  7.5  ,
            3.9  ,  0.0  ,  0.0  ,  3.6  ,  0.0  ,  0.5  ,  0.4  ,
            0.1]

    S =  [0., 0.00025, 0.001,  0.004,  0.016,  0.064,   0.256, 1.024, 4.096,  16.384]


    if QWL == "INIT":  # 特性値の読み込み

        for L in range(0,100):
        
            J  = int(HO.sheet_by_name("Sheet1").cell(L,0).value)
            IU = int(HO.sheet_by_name("Sheet1").cell(L,1).value)
            ALMD = float(HO.sheet_by_name("Sheet1").cell(L,2).value)
            CRHO = float(HO.sheet_by_name("Sheet1").cell(L,3).value)

            if J >= 1 and J <= 100:

                if (IU == 1):  # SI入力
                    if (J >= 90):  # 純抵抗
                        if (ALMD == 0):  # 未定義(非引用)の材料を想定
                            TCM[0,J-1] = 1.0*10**6
                        else:
                            TCM[0,J-1] = 1.0/(ALMD/0.86)   # [m2K/W]から[(m2hdeg/kcal)^(-1)]へ
                        TCM[1,J-1] = 0
                    else:
                        TCM[0,J-1]=ALMD*0.86   # [W/mK]から[kcal/mhdeg]へ
                        TCM[1,J-1]=CRHO/4.186  # [kJ/m3K]から[kcal/m3deg]へ

                else:

                    if (J >= 90):  # 純抵抗
                        if (ALMD == 0):  # 未定義(非引用)の材料を想定
                            TCM[0,J-1] = 1.0*10**6
                        else:
                            TCM[0,J-1] = 1.0/(ALMD)
                        TCM[1,J-1] = 0
                    else:
                        TCM[0,J-1]=ALMD
                        TCM[1,J-1]=CRHO

        return TCM
    
    elif QWL == "FURN":
        V = 2
        W = V*HO
        for J in range(0,10):
            TRNS[J]=0.
            ADMT[J]=W*S[J]/(S[J]+V)

        return TRNS,ADMT

    elif QWL == "FURS":
        
        for J in range(0,10):
                TRNS[J]=0.0
                SUM = 0.0
                for K in range(0,22):
                    if abs(EPS[K]) < 0.0001:
                        SUM=SUM+XN[K]*G0QC[K]
                    else:
                        SUM=SUM+XN[K]*(G0QC[K]+G1QC[K]/(S[J]+EPS[K]))
                ADMT[J]=SUM*0.01*HO*S[J]

        return TRNS,ADMT

    else:

        # print('-----  GVECTR  -----')

        # 初項
        R[0] = 1/HT
        C[0] =0.0
        
        # 初項 0 があるため、NLと1個ずれることに注意。
        for L in range(0,int(NL)):
            if MT[L] > 0:

                # 建材番号
                I = int(MT[L])
                if I > 90:
                    TH[L] = 1.0
                R[L+1]=TH[L]/TCM[0,I-1]
                C[L+1]=TH[L]*TCM[1,I-1]
            else:
                R[L+1]=TH[L]   # 純熱抵抗
                C[L+1]=0.0

        # GWALでなければ一層追加
        if QWL != "GWAL":
            NL=NL+1
            R[int(NL)]=1/HO
            C[int(NL)]=0.

        mprint("NL",NL)
        mprint("R",R)
        mprint("C",C)

        for J in range(0,10):

            G0 = 1.0
            U1 = 1.0
            U2 = 0.0
            U3 = 0.0
            U4 = 1.0
        
            # 層の数
            for L in range(0,int(NL+1)):
            
                W = math.sqrt(S[J]*R[L]*C[L])

                if (W == 0):
                    V1 = 1.
                    V2 = R[L]
                    V3 = 0.

                    mprint('-------------------',"")
                    mprint("L",L)
                    mprint("J",J)
                    mprint("W",W)
                    mprint("R[L]",R[L])
                    mprint("V0","-")
                    mprint("V1",V1)
                    mprint("V2",V2)
                    mprint("V3",V3)

                else:
                    V0 = math.exp(-W)
                    G0 = G0*V0
                    V1 = 0.5*(1.0 + V0**2)
                    V2 = 0.5*R[L]*(1.0 - V0**2)/W
                    V3 = 0.5*W*(1.0 - V0**2)/R[L]

                    mprint('-------------------',"")
                    mprint("L",L)
                    mprint("J",J)
                    mprint("W",W)
                    mprint("0.5*R[L]",0.5*R[L])
                    mprint("V0**2",V0**2)
                    mprint("(1.0 - V0**2)",(1.0 - V0**2))
                    mprint("1/W",1/W)
                    mprint("R[L]",R[L])
                    mprint("V0",V0)
                    mprint("V1",V1)
                    mprint("V2",V2)
                    mprint("V3",V3)

                W1 = U1
                W3 = U3
                U1 = W1*V1 + U2*V3
                U2 = W1*V2 + U2*V1
                U3 = W3*V1 + U4*V3
                U4 = W3*V2 + U4*V1

                mprint('-------------------',"")
                mprint("W1",W1)
                mprint("W3",W3)
                mprint("U1",U1)
                mprint("U2",U2)
                mprint("U3",U3)
                mprint("U4",U4)
            
            TRNS[J] = G0/U2
            ADMT[J] = U4/U2

            mprint('--------------------------------------',"")
            mprint("G0",G0)
            mprint("U4",U4)
            mprint("U2",U2)

        mprint("TRNS",TRNS)
        mprint("ADMT",ADMT)

    return TRNS,ADMT