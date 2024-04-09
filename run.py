import subprocess
from pyHASP import pyHASP

folder = ".\\test\\test_021\\"


## setting
input_filename    = "inputdata.txt"
cliname_filename  = "36300110_SI.hasH"
wndwtabl_filename = "wndwtabl"
wcontabl_filename = "wcontabl"
resultfile_prefix = "pyHASP_"

#------------------------------------------------------
# newHASPの実行
#------------------------------------------------------
file_paths = [
    folder + input_filename,
    folder + cliname_filename,
    folder,
    folder + wndwtabl_filename + ".dat",
    folder + wcontabl_filename + ".dat",
    "NewHASP_forACSS.txt",
    "NewHASP_forBECS.txt"
]

with open("NewHASP_fname.txt", "w", encoding="utf-8") as f:
    for path in file_paths:
        f.write(path + "\n")

subprocess.call("NewHASP.bat", shell=True)


#------------------------------------------------------
# pyHASPの実行
#------------------------------------------------------
pyHASP(
    folder + input_filename,
    folder + cliname_filename, 
    folder + wndwtabl_filename + ".xlsx", 
    folder + wcontabl_filename + ".xlsx", 
    folder + resultfile_prefix
)

