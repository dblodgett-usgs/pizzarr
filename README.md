# pizzarr <a href="https://zarr-developers.github.io/pizzarr/"><img src="man/figures/logo.png" align="right" height="139" alt="pizzarr website" /></a>

[![codecov](https://codecov.io/gh/zarr-developers/pizzarr/graph/badge.svg?token=vhidertN9l)](https://codecov.io/gh/zarr-developers/pizzarr)
[![R-CMD-check](https://github.com/zarr-developers/pizzarr/actions/workflows/R-CMD-Check.yml/badge.svg)](https://github.com/zarr-developers/pizzarr/actions/workflows/R-CMD-Check.yml) 
[![logs](https://cranlogs.r-pkg.org/badges/pizzarr)](https://cran.r-project.org/package=pizzarr)
[![CRAN status](https://www.r-pkg.org/badges/version/pizzarr)](https://CRAN.R-project.org/package=pizzarr)

A Zarr implementation for R.

## Installation

Installation requires R 4.0.0 or greater.

```r
install.packages("devtools")
devtools::install_github("zarr-developers/pizzarr")
```

## Usage

```r
library(pizzarr)

a <- array(data=1:20, dim=c(2, 10))
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]    1    3    5    7    9   11   13   15   17    19
# [2,]    2    4    6    8   10   12   14   16   18    20
z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)

z$set_item("...", a)

selection <- z$get_item(list(slice(1, 2), slice(1, 5)))

print(selection$data)
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    3    5    7    9
# [2,]    2    4    6    8   10
```

## Features

- **Zarr V2 and V3** read and write (format auto-detected on open)
- **Stores:** MemoryStore, DirectoryStore (read/write); HttpStore (read-only)
- **Data types:** boolean, int8--int64, uint8--uint64, float16/32/64, string, Unicode, VLenUTF8
- **Compression:** zlib/gzip, bzip2, blosc, LZMA, LZ4, Zstd
- **Blosc** requires the optional [`blosc`](https://cran.r-project.org/package=blosc) package (`install.packages("blosc")`)


## Development


```r
setwd("path/to/pizzarr")
install.packages("devtools")
devtools::install()
devtools::load_all()
```

## Testing

Set `TESTTHAT_CPUS=#` in .Renviron to run tests in parallel

```r
devtools::check()
devtools::test()
```

## Documentation

```r
install.packages("devtools")
install.packages("pkgdown")
devtools::document()
pkgdown::build_site()
```

## Resources

- [Discussion of Zarr in R](https://github.com/zarr-developers/community/issues/18)
- [blosc](https://cran.r-project.org/web/packages/blosc/index.html)
  - Note: `pizzarr` has an optional dependency on `blosc` for Blosc (de)compression.
- R package development
  - [R packages](https://r-pkgs.org/)
  - [roxygen2 syntax](https://roxygen2.r-lib.org/articles/rd-formatting.html)
  - [R6](https://r6.r-lib.org/index.html)
  - [R6 roxygen2 syntax](https://www.tidyverse.org/blog/2019/11/roxygen2-7-0-0/#r6-documentation)
  - [pkgdown](https://pkgdown.r-lib.org/)
- Zarr implementation
  - [zarr_implementations](https://github.com/zarr-developers/zarr_implementations)
  - [zarr-python](https://github.com/zarr-developers/zarr-python)
  - [ZSTD compression in R](https://github.com/qsbase/qs2)
  - [LZ4 and ZSTD compression in R - archived](https://github.com/qsbase/qs)
  - [zarr.js](https://github.com/gzuidhof/zarr.js)
  - [zarrita.js](https://github.com/manzt/zarrita.js)
  - [v2 spec](https://zarr.readthedocs.io/en/stable/spec/v2.html)
