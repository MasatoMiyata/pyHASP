def NDATE(MON,IDY):
    """ANNUAL BASE DATIN
    MON MONTH
    IDY DAY OF THE MONTH
    y : DAY OF THE YEAR

    """
    if MON-3 >= 0:
        y = int(30.57*MON - 31.06 - 1 + IDY)
    else:
        y = int(30.57*MON - 31.06 + 1 + IDY)

    return y