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

utils::globalVariables(c("opt.state", "self", "captureSpecials"))

# make these point to whatever bbotk happens to name its classes today
Optimizer_internal = NULL
OptimInstanceSingleCrit_internal = NULL
OptimInstanceMultiCrit_internal = NULL

#' @export
#' @title Optimizer Class
#'
#' @description
#' `bbotk`'s `Optimizer` class.
#' Re-exported since `bbotk` will change the name.
Optimizer = R6::R6Class("Optimizer", inherit = Optimizer_internal)

#' @export
#' @title OptimInstanceSingleCrit Class
#'
#' @description
#' `bbotk`'s `OptimInstanceSingleCrit` class.
#' Re-exported since `bbotk` will change the name.
OptimInstanceSingleCrit = R6::R6Class("OptimInstanceSingleCrit", inherit = OptimInstanceSingleCrit_internal)

#' @export
#' @title OptimInstanceMultiCrit Class
#'
#' @description
#' `bbotk`'s `OptimInstanceMultiCrit` class.
#' Re-exported since `bbotk` will change the name.
OptimInstanceMultiCrit = R6::R6Class("OptimInstanceMultiCrit", inherit = OptimInstanceMultiCrit_internal)

#' @export
#' @title TuningInstanceSingleCrit Class
#'
#' @description
#' `mlr3tuning`'s `TuningInstanceSingleCrit` class.
#' Re-exported since `mlr3tuning` will change the name.
TuningInstanceSingleCrit = R6::R6Class("TuningInstanceSingleCrit", inherit = TuningInstanceSingleCrit_internal)

#' @export
#' @title TuningInstanceMultiCrit Class
#'
#' @description
#' `mlr3tuning`'s `TuningInstanceMultiCrit` class.
#' Re-exported since `mlr3tuning` will change the name.
TuningInstanceMultiCrit = R6::R6Class("TuningInstanceMultiCrit", inherit = TuningInstanceMultiCrit_internal)


.onLoad = function(libname, pkgname) {
  # nocov start
  backports::import(pkgname)
  warnIfPHLoaded()
  utils::getFromNamespace("mlr_optimizers", ns = "bbotk")$add("intermbo", OptimizerInterMBO)
  utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")$add("intermbo", TunerInterMBO)

  ## backward compatibility with bbotk
  replacing = c("Optimizer%s", "OptimInstance%sSingleCrit", "OptimInstance%sMultiCrit")
  localnames = paste0(sprintf(replacing, ""), "_internal")
  if (exists("OptimizerBatch", envir = asNamespace("bbotk"))) {
    bbotknames = sprintf(replacing, "Batch")
  } else {
    bbotknames = sprintf(replacing, "")
  }
  for (i in seq_along(replacing)) {
    assign(localnames[[i]], get(bbotknames[[i]], envir = asNamespace("bbotk")), envir = parent.env(environment()))
  }

  replacing = c("Tuner%sFromOptimizer", "TuningInstance%sSingleCrit", "TuningInstance%sMultiCrit")
  oldnames = sprintf(replacing, "") 
  newnames = sprintf(replacing, "Batch")
  newnames[[1]] = paste0(newnames[[1]], "Batch")
  localnames = paste0(oldnames, c("", "_internal", "_internal"))
  for (i in seq_along(replacing)) {
    oldname = oldnames[[i]]
    newname = newnames[[i]]
    lname = localnames[[i]]
    makeActiveBinding(lname, env = parent.env(environment()), fun = crate(function() {
      if (!requireNamespace("mlr3tuning", quietly = TRUE)) return(NULL)
      if (exists(newname, envir = asNamespace("mlr3tuning"))) {
        get(newname, envir = asNamespace("mlr3tuning"))
      } else {
        get(oldname, envir = asNamespace("mlr3tuning"))
      }
    }, oldname, newname))
  }
}  # nocov end

if (!Sys.getenv("DEVTOOLS_LOAD") == "true") {
  leanify_package()
}

