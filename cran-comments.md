## Test environments

- Local Windows 10, R 4.5.1
- win-builder (R-devel): `devtools::check_win_devel()`
- R-hub (v2): linux, macos, windows, fedora

## R CMD check results

0 errors | 0 warnings | 0 notes

## Resubmission summary

This version addresses all feedback from the CRAN reviewer:
- Reduced title length to < 65 characters and removed the redundant "R Code to" prefix.
- Removed single quotes around acronyms (MEDITS, TA, TB, TC, TE, TL, TX) in the DESCRIPTION file.
- Changed 'Shiny' to 'shiny' (lowercase) to match package case-sensitivity.
- Fixed examples in `printError.Rd` and `printError_cc.Rd` by uncommenting the code as requested.

## Downstream dependencies

This is a new submission. There are no downstream dependencies to check.
