
#' @import checkmate
#' @import data.table
#' @import mlr3misc
#' @import paradox
#' @importFrom R6 R6Class
"_PACKAGE"


dummy_import = function() {
  # nocov start
  # this function is required to avoid spurious warnings in R CMD check
  bbotk::mlr_optimizers
  mlr3tuning::mlr_tuners
  lhs::randomLHS
} # nocov end


.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
  warnIfPHLoaded()
  utils::getFromNamespace("mlr_optimizers", ns = "bbotk")$add("intermbo", OptimizerInterMBO)
  utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")$add("intermbo", TunerInterMBO)
}

utils::globalVariables(c("opt.state", "self", "captureSpecials"))

leanify_package()
