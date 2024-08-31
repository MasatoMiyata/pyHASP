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
    
    if NUW[0][0] == "*":
        line = 7 * NUW_day
    else:
        line = 7 * NUW_day - 1

    for I in [1,2,3,4,5,6,7]:

        for J in range(1,25):
            WD[I,J] = NUW[line+I][NCLM*(J-1):NCLM*J]

        ID[I,1] =  NUW[line+I][24*NCLM+0 : 24*NCLM+2]
        ID[I,2] =  NUW[line+I][24*NCLM+2 : 24*NCLM+4]
        ID[I,3] =  NUW[line+I][24*NCLM+4 : 24*NCLM+6]
        ID[I,4] =  NUW[line+I][24*NCLM+6 : 24*NCLM+7]
        ID[I,5] =  NUW[line+I][24*NCLM+7 : 24*NCLM+8]

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
    
    # 次の日付にシフト
    NUW_day += 1

    # 次の日のデータがあるかを確認
    if len(NUW) < 7*(NUW_day+1):  # 最終行まで来たら繰り返す
        NUW_day = 0
        ICYCL += 1

    return ICYCL,WD,ID,ISTAT,NUW_day