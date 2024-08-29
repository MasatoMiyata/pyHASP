import subprocess
from pyHASP import pyHASP

# 入力データの名称
input_filename    = "./sample/inputdata.txt"
# 気象データ（hasp形式）の名称
cliname_filename  = "./sample/36300110_SI.hasH"
# 窓データファイルの名称
wndwtabl_filename = "./sample/wndwtabl_builelib.xlsx"
# 壁体構造データファイルの名称
wcontabl_filename = "./sample/wcontabl_builelib.xlsx"

#------------------------------------------------------
# pyHASPの実行
#------------------------------------------------------
pyHASP(
    input_filename,
    cliname_filename, 
    wndwtabl_filename, 
    wcontabl_filename, 
)

#------------------------------------------------------
# newHASP (fortran) の実行
#------------------------------------------------------
# file_paths = [
#     folder + input_filename,
#     folder + cliname_filename,
#     folder,
#     folder + wndwtabl_filename + ".dat",
#     folder + wcontabl_filename + ".dat",
#     "NewHASP_forACSS.txt",
#     "NewHASP_forBECS.txt"
# ]

# with open("NewHASP_fname.txt", "w", encoding="utf-8") as f:
#     for path in file_paths:
#         f.write(path + "\n")

# subprocess.call("NewHASP.bat", shell=True)