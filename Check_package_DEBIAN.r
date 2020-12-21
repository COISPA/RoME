install.packages("rhub")
library(rhub)
validate_email()
# rhub::check(platform = "debian-gcc-devel", show_status = TRUE)

rhub::check(show_status = FALSE)

# rhub::check_for_cran(show_status = FALSE)


devtools::run_examples()
