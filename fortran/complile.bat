gfortran -o mytHASP NewHASP-ACLD.for LAPACK_dgesv.f coeffp.for extrc1.for postp.for weathout.for coeffs.for extrc2.for rtvadj.for clcmrt.for extrc0.for oahuout.for slvsm.for
pause
mytHASP.exe < fnameNewHASP.txt > mytHASP.err
pause