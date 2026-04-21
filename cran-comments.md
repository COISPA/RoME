## Test environments

- Local Windows 11, R 4.4.x
- win-builder (R-release, R-devel): `devtools::check_win_devel()`
- R-hub: `rhub::check_for_cran()`

## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

- **NOTE: `import(rnaturalearthdata)`** — `rnaturalearthdata` is a data-only
  package. A full `import()` is the only practical way to make its spatial
  datasets available at runtime. This is a known and accepted pattern for
  data packages on CRAN.

- **NOTE: examples with `\donttest{}`** — Several check functions write output
  files to disk and require more than 5 seconds to run. These are wrapped in
  `\donttest{}` as recommended in the Writing R Extensions manual.

- **NOTE: `svDialogs` in Suggests** — `svDialogs` is only used inside
  `inst/shiny/RoMEApp/app.r` to open a native OS directory picker within the
  Shiny interface. It is listed in `Suggests` because it is only needed when
  the Shiny app is launched interactively by the user.

## Downstream dependencies

This is a new submission. There are no downstream dependencies to check.
