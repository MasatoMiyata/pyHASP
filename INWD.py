import numpy as np

def INWD(NUW,IOPWE,NCLM,ICYCL,NUW_day):
    """
    引数:
    NUW       気象データファイルの装置番号
    IOPWE     =0:周期データ、=1:実在データ
    NCLM      データカラム数（3以上9以下）
    ICYCL     現在のファイル読み込みサイクル数
    WD(7,24)  気象データ（加工前）
    ID(7,5)   日付データ（4桁で返す。ただしIOPWE=0のときの年は"0"）
    ISTAT     =1:通常  0:ファイル終了(IOPWE=1のとき)
    """

    WD = np.zeros([8,25])
    ID = np.zeros([8,6])

    line = 7 * NUW_day
    for I in [1,2,3,4,5,6,7]:

        for J in range(1,25):
            WD[I,J] = NUW[line+I][4*(J-1):4*J]

        ID[I,1] =  NUW[line+I][96:98]
        ID[I,2] =  NUW[line+I][98:100]
        ID[I,3] =  NUW[line+I][100:102]
        ID[I,4] =  NUW[line+I][102:103]
        ID[I,5] =  NUW[line+I][103:104]

    if IOPWE == 0:
        for I in [1,2,3,4,5,6,7]:
            ID[I,1]=0
    else:
        for I in [1,2,3,4,5,6,7]:

            # 年の4桁化
            if ID[I,1] >= 51:
                ID[I,1] = 1900+ID[I,1]
            else:
                ID[I,1] = 2000+ID[I,1]

    ISTAT = 1

    # 周期データには対応していない

    # 次の行数
    NUW_day += 1
    if len(NUW)-1 <= 7*(NUW_day):  # 最終行まで来たら繰り返す
        NUW_day = 0
        ICYCL += 1

    return ICYCL,WD,ID,ISTAT,NUW_day