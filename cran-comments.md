## Resubmission

This is a resubmission of version 0.2.4.

In response to feedback from Uwe Ligges:
- Removed the dismounted MEDITS website URL (`https://www.sibm.it/MEDITS%202011/principaledownload.htm`) from `DESCRIPTION`, `README.md`, vignettes, and function documentation files (`man/*.Rd`). 
- **Note:** The original website URL has been discontinued and is currently unavailable. The formal bibliographic reference (`MEDITS Working Group (2017) MEDITS-Handbook. Version n. 9. MEDITS Working Group. 106 pp.`) has been retained across all documentation files.

## Test environments

- Local Windows 10, R 4.5.1
- win-builder (R-devel): `devtools::check_win_devel()`
- R-hub (v2): linux, macos, windows, fedora

## R CMD check results

0 errors | 0 warnings | 0 notes

## Update summary

- System-wide standardization of validation checks to explicitly prefix log messages with "Error: " or "Warning: ".
- Fixed a bug in `check_weight_tot_nb()` that caused a crash when processing records with an empty or `NA` `FAUNISTIC_CATEGORY`.
- Removed dismounted URL reference (`sibm.it`) from package metadata and documentation.

## Downstream dependencies

There are no downstream dependencies to check.
