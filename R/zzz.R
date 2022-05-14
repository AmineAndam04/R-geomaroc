.pkgenv <- new.env(parent=emptyenv())

.onLoad  <- function(libname, pkgname) {
  has_data <- requireNamespace("geomarocdata", quietly = TRUE)
  .pkgenv[["has_data"]] <- has_data
}

.onAttach <- function(libname, pkgname) {
  if (!.pkgenv$has_data) {
    msg <- paste("To use this package, you must install the",
                 "geomarocdata package. To install that ",
                 "package, run `install.packages('geomarocdata',",
                 "repos='https://amineandam04.github.io/drat/', type='source')`.")
    msg <- paste(strwrap(msg), collapse="\n")
    packageStartupMessage(msg)
  }
}

hasData <- function(has_data = .pkgenv$has_data) {
  if (!has_data) {
    msg <- paste("To use this function, you must install the",
                 "geomarocdata package. To install that ",
                 "package, run `install.packages('geomarocdata',",
                 "repos='https://amineandam04.github.io/drat/', type='source')`.")
    msg <- paste(strwrap(msg), collapse="\n")
    stop(msg)
  }
}
