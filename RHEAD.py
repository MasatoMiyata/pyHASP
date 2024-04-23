def RHEAD(QA, IWFLG, RWFLG):
    """
    気象データのヘッダー部分の読み込み
    """

    # IWFLG[2] 日射・放射の単位 =0:10kJ/m2h, =1:kcal/m2h, =2:kJ/m2h、
    if (QA[15:19] == '10kJ'):
        IWFLG[2] = 0
    elif (QA[15:19] == 'kcal'):
        IWFLG[2] = 1
    elif (QA[15:19] == 'kJ  '):
        IWFLG[2] = 2
    else:
        raise Exception("ERROR: RHEAD")

    # IWFLG[3] 雲量モード =0:雲量, =1:夜間放射、
    if (QA[20:23] ==  'CA '):
        IWFLG[3] = 0
    elif (QA[20:23] == 'LNR'):
        IWFLG[3] = 1
    else:
        raise Exception("ERROR: RHEAD")

    # IWFLG[4] 気象データのカラム数(3以上9以下)
    IWFLG[4] = QA[24]
    if IWFLG[4] <= 3 or IWFLG[4] >= 9:
        raise Exception("ERROR: RHEAD")

    # RWFLG[1] 緯度[deg]（南緯の場合は負値）、
    IWK1 = float(QA[60:62])
    IWK2 = float(QA[62:65])

    if QA[65] == "N":
        RWFLG[1] = IWK1 + IWK2/600.0
    elif QA[65] == "S":
        RWFLG[1] = -IWK1 - IWK2/600.0
    else:
        raise Exception("ERROR: RHEAD")

    # RWFLG[2] 経度[deg]（西経の場合は負値）、
    IWK1 = float(QA[67:70])
    IWK2 = float(QA[70:73])

    if QA[73] == "E":
        RWFLG[2] = IWK1 + IWK2/600.0
    elif QA[65] == "W":
        RWFLG[2] = -IWK1 - IWK2/600.0
    else:
        raise Exception("ERROR: RHEAD")

    # RWFLG[3] 世界時と地方標準時の時差
    RWFLG[3] = float(QA[77:83])

    return IWFLG, RWFLG
