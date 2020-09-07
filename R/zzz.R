
#' @import checkmate
#' @import data.table
#' @import mlr3misc
#' @import paradox
#' @importFrom R6 R6Class

.onLoad = function(libname, pkgname) {
  backports::import(pkgname)
  warnIfPHLoaded()
  utils::getFromNamespace("mlr_optimizers", ns = "bbotk")$add("intermbo", OptimizerInterMBO)
  utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")$add("intermbo", TunerInterMBO)
}

utils::globalVariables(c("opt.state", "self", "captureSpecials"))

leanify_package()
