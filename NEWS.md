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
1. check_weight() now logs detailed records of species with mean weights outside reference ranges into a CSV file instead of writing extensive messages in the .dat log, ensuring clearer logs and easier downstream analysis. Plots are now saved exclusively to files and are no longer displayed interactively, and species plots are limited to those with at least 5 observations to reduce unnecessary graph generation. The function was adapted to work with the updated DataTargetSpecies table

2. check_length() now saves all detailed inconsistencies into a CSV file instead of writing verbose messages in the .dat log, improving clarity and data traceability, using the new updated DataTargetSpecies table.

3. check_individual_weightTC changed to save the table for the comparison between the estimated  vs observed weights in TC.

4. the check_smallest_mature function was adapted to work with the updated DataTargetSpecies table

5. check_smallest_mature: Improved handling of non-numeric values in Maturity_parameters and introduced a 10% buffer when checking lengths. Added a CSV output listing all records where mature individuals are smaller than reference sizes, including the threshold size and bibliographic references. The function has also been adapted to the new format of DataTargetSpecies.

6. check_species_TBTC function improved with more robust data filtering checks, introduction of a new CSV output file for clearer results tracking, and streamlined log file content to reduce verbosity. The function has also been adapted to the new format of DataTargetSpecies.

7. check_spawning_period now logs all maturity-stage consistency warnings into a dedicated CSV file rather than filling the .dat log with verbose messages, improving traceability and downstream analyses. The function robustly handles missing (NA) values in both maturity parameters and survey data to avoid false positives. It now separates warnings for immature individuals observed within the spawning period but above size thresholds, and for mature individuals found outside the spawning period, distinguishing whether they fall below bibliographic size limits. CSV outputs include comprehensive details such as spawning months, size thresholds, and the type of inconsistency detected. The function has been adapted to work with the updated DataTargetSpecies table.

8. check_dictionary() now reports the actual invalid value in every log entry. Previously it said "value not allowed for X"; now each line includes the offending string or NA (e.g., "Haul 109: value 'XYZ' not allowed for COURSE in TA"), and messages are batched into a single writeLines() call to eliminate hundreds of small I/O operations. The function was dramatically rebuilt to optimize computational time, achieving a runtime reduction of >90% for this routine (e.g., from 12.91 s down to 0.07 s).

9. check_length(): the internal per-row loop has been replaced with vectorized index computations (which() + match()), and all I/O is consolidated into single write.table() and write() calls. These changes preserve the original logic and output formats (including handling of missing, negative, and out-of-range classes with the updated DataTargetSpecies), while delivering a ≈66% reduction in the routine's execution time (e.g., from 2.77 s down to 0.93 s).

10. Replaced all class(...) == checks with inherits(...) to improve code robustness and avoid R CMD check notes. Fixed documentation mismatchs, ensuring consistency between code default arguments and Rd files. Corrected a misplaced brace in the Rd documentation of check_position_in_Med(), resolving a note from R CMD check.
