
#' @import checkmate
#' @import data.table
#' @import mlr3misc
#' @import paradox
#' @importFrom R6 R6Class

.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
  warnIfPHLoaded()
}

leanify_package()
