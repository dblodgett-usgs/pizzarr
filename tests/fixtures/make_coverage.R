# used to investigate coverage locally
cov <- covr::package_coverage()
missed_lines <- covr::zero_coverage(cov)

covr::report(cov)

readr::write_csv(as.data.frame(missed_lines), "missed_lines.csv")
