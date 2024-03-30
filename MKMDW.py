from NDATE import NDATE

def MKMDW(ID, M):
    """
    **************** DATING (DAY OF THE WEEK) ****************************
    *     LATEST REVISION     - 1996.06.15
    *     ARGUMENTS ID(7,5)   - INPUT   DATE.
    *     FUNCTION  MKMDW               DAY OF THE WEEK.
    *     REQ. ROUTINES       - NDATE
    **********************************************************************
    """

    KDY = NDATE(ID[1,2],ID[1,3])

    mkmdw = ID[1,4]-1

    if (mkmdw == 0):
        mkmdw = 7
    if (mkmdw < 0):
        mkmdw = 8

    if (M[KDY+192] == 1):
        mkmdw = 9   
    
    return mkmdw
