#' @title Tuner using mlrMBO
#'
#' @aliases mlr_optimizers_intermbo
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3tuning::Tuner].
#'
#' @description
#' Subclass for mlrMBO tuning.
#'
#' mlrMBO must not be loaded directly into R when using mlr3, for various reasons.
#' TunerInterMBO takes care that this does not happen.
#'
#' As they say (to the tune of the "Calgon" jingle): ♫♪ Tuning sessions they live longer with mlrintermbo ♫♪.
#' @include paramset.R
#' @include ParamHelpersParamSet.R
#' @export
OptimizerInterMBO <- R6Class("OptimizerInterMBO",
  inherit = bbotk::Optimizer,
  public = list(
    n.objectives = NULL,
    opt.state = NULL,
    initialize = function(n.objectives = 1) {
      self$n.objectives <- n.objectives
      warnIfPHLoaded
      super$initialize(
        param_set = mboParamSet(n.objectives),
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = "dependencies",
        packages = "mlr3",
        properties= c("single-crit", "multi-crit", "noisy", "deterministic")
      )
    }
  ),
  private = list(
    .optimize = optimize,
    .assign_result = assignResult
  )
)

bbotk::mlr_optimizers$add("intermbo", OptimizerInterMBO)

#' @rdname OptimizerInterMBO
#' @export
TunerInterMBO <- R6Class("TunerInterMBO",
  inherit = mlr3tuning::TunerFromOptimizer,
  public = list(
    initialize = function(n.objectives = 1) {
      super$initialize(OptimizerInterMBO$new(n.objectives))
    }
  )
)

mlr3tuning::mlr_tuners$add("intermbo", TunerInterMBO)
