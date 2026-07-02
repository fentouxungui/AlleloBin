#' Example Data
#'
#' Example SSR allele dataset (`data.txt`) and repeat length file (`info.txt`)
#' included with the package.
#' 
#' References
#' Idury, R.M. & Cardon, L.R. (1997). A Simple Method for Automated Allele Binning in Microsatellite Markers. Genome Research, 11:1104–1109.
#'
#' @examples
#' datafile <- system.file("extdata", "data.txt", package = "AlleloBin")
#' infofile <- system.file("extdata", "info.txt", package = "AlleloBin")
#' data <- read.table(datafile, header = FALSE)
#' info <- scan(infofile)
#' allele_binning(data, info)
#'
#' @name example_data
NULL
