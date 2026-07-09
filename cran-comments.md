## Test environments

- Local Windows 10, R 4.5.1
- win-builder (R-devel): `devtools::check_win_devel()`
- R-hub (v2): linux, macos, windows, fedora

## R CMD check results

0 errors | 0 warnings | 0 notes

## Update summary

- System-wide standardization of validation checks to explicitly prefix log messages with "Error: " or "Warning: ".
- Fixed a bug in `check_weight_tot_nb()` that caused a crash when processing records with an empty or `NA` `FAUNISTIC_CATEGORY`.

## Downstream dependencies

There are no downstream dependencies to check.
