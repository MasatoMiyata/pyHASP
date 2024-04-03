import subprocess
from pyHASP import pyHASP

# folder = ".\\test\\Sample_Input_NewHASP\\"
# input_filename = "Sample_Input_NewHASP.txt"

folder = ".\\test\\test_001_single_room\\"

input_filename    = "inputdata.txt"
cliname_filename  = "36300110_SI.hasH"
wndwtabl_filename = "wndwtabl"
wcontabl_filename = "wcontabl"

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
resultfile_prefix = "pyHASP_"

pyHASP(
    folder + input_filename,
    folder + cliname_filename, 
    folder + wndwtabl_filename + ".xlsx", 
    folder + wcontabl_filename + ".xlsx", 
    folder + resultfile_prefix
)

