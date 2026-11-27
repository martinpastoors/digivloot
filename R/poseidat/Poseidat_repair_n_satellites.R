# One-off repair: n_satellites -> integer, -Inf -> NA in every thinned file.
library(dplyr)
thinned_dir <- file.path(.onedrive(), "processed", "thinned")
bestanden   <- list.files(thinned_dir, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)

for (f in seq_along(bestanden)) {
  tmp <- tempfile(fileext = ".parquet")
  file.copy(bestanden[f], tmp)
  x <- arrow::read_parquet(tmp)
  unlink(tmp)
  if (!"n_satellites" %in% names(x)) { cat(sprintf("%-55s no n_satellites\n", basename(bestanden[f]))); next }
  oud  <- class(x$n_satellites)[1]
  n_inf <- sum(is.infinite(x$n_satellites), na.rm = TRUE)
  x <- x |>
    mutate(n_satellites = as.integer(if_else(is.finite(n_satellites), as.numeric(n_satellites), NA_real_)))
  arrow::write_parquet(x, bestanden[f])
  cat(sprintf("%-55s %s -> integer (%d Inf set to NA)\n", basename(bestanden[f]), oud, n_inf))
}
