def NDATE(MON,IDY):
    """
    **************** ANNUAL BASE DATING **********************************
    *     ARGUMENTS MON       - INPUT.  MONTH.
    *               IDY       - INPUT.  DAY OF THE MONTH.
    *     FUNCTION  NDATE               DAY OF THE YEAR.
    *     REQ. ROUTINES       - NONE.
    **********************************************************************
    """
    if MON-3 >= 0:
        y = int(30.57*MON - 31.06 - 1 + IDY)
    else:
        y = int(30.57*MON - 31.06 + 1 + IDY)

    return y