NEWS
================
Walter Zupa - COISPA
05/06/2024

Fixes 0.1.1
-----
1. "windows" was eliminated from NAMESPACE;
2. The spelling of GSAs, (7:102, 7:427), MEDITS (3:45, 7:80, 7:148, 7:216, 7:320, 7:449), RoME (7:270, 7:388) is verified and its correctness is confirmed;
3. The time for running examples is due to the number of functions involved in the package.

Fixes 0.1.2
-----
1. Each example has been reduced to less than 5 seconds of time to run.

Fixes 0.1.3
-----
1. Eliminated  Namespace with empty importFrom: 'grDevices'
2. Corrected the wrong pintsize in function check_position.
3. Eliminated the folders created in all the RoME functions from the tempdir.

Fixes 0.1.4
-----
1. Corrected the wrong pintsize in function check_temperature.
2. Revised again for eliminating the folders created in all the RoME functions from the tempdir.

Fixes 0.1.5
-----
1. Corrected the wrong pointsize in function check_weight.

Fixes 0.1.6
-----
1. Eliminated detritus from the temporary directories.

Fixes 0.1.7
-----
1. Changed the reference version of R indicated in the file DESCRIPTION.

Fixes 0.1.8
-----
1. Solved the error in check_dm for saving the Logfile.

Fixes 0.1.9
-----
1. Space removed from filenames of the files stored in the temporary directory.

Fixes 0.1.10
-----
1. Addressed the ploblem of nested temporary directories in Linux.

Fixes 0.1.11
-----
1. Error solved in check_dm.Rd example

Fixes 0.1.12
-----
1. Solved the problems of nested temporary directories in Linux.

Fixes 0.1.13
-----
1. Solved other problems of nested temporary directories in Linux.

Fixes 0.1.14
-----
1. Fixed problems of recursive directories creation in Debian with check_dm() function.

Fixes 0.1.15
-----
1. Revision of Documentation Rd files following the .
2. Recision of contributors description in Documentation file

Fixes 0.1.16
-----
1. Inclusion of length-weight dataframe into RoME() parameters

Fixes 0.1.17
-----
1. Modification of RoME function to allow estimation of R sufi files from both TC and TE tables

Fixes 0.1.18
-----
1. Improvement of output plots using functions of ggplot2 package 

Fixes 0.1.19
-----
1. issue in check_area function corrected

Fixes 0.1.20
-----
1. inclusion of new functions (e.g. RoMEcc) to adapt the package to work with the RDBFID database

Fixes 0.1.21
-----
1. inclusion of new function to convert RDBFIS headers to MEDITS format

Fixes 0.1.23
-----
1. check dictionary function allows also NA values

Fixes 0.1.24
-----
1. included country in filtering TB and TC data in check_raising

Fixes 0.1.26
-----
1. improvement of check_raising outputs

Fixes 0.1.27
-----
1. bugs fixed

Fixes 0.1.28
-----
1. included the zip parameter in RoMEcc
2. replaced the zip function with the "zip" library one

Fixes 0.1.29
-----
1. check_haul_species_TCTB function modified to produce Critical_Error file
2. checkHeader function modified to produce Critical_Error file
3. check_class function modified to produce Critical_Error file
4. check_consistencyTA_duration: identify errors if SHOOTING_TIME or HAULING_TIME fields are not integer
5. inclusion of the function RoMEBScc specifically working on Black Sea MEDITS-like data

Fixes 0.1.30
-----
1. New stratification_ _scheme table

Fixes 0.1.31
-----
1. generalization of check_stratum
2. optimization of check_weight

Fixes 0.1.34
-----
1. optimization of zip saving
2. optimization of temporary directory use

Fixes 0.1.35
-----
1. improved management of errors in check_length function

Fixes 0.1.36
-----
1. quasi-identical records' check output changed in warning message instead of error

Fixes 0.1.37
-----
1. warning messages modified in check_bridles_length function

Fixes 0.1.38
-----
1. header.conversion function modified for the measuring_system_salinity field in input file (TA)

Fixes 0.1.39
-----
1. improvement of check_individual_weightTE function

Fixes 0.2.0
-----
0. RDBFIS III Fixes 
1. check_weight function modified to include an updated version of DataTargetSpecies table containing reference values of individual weights and lengths estimated from MEDITS TB table considering the percentile ranges: 10-90th and 25-75th.


