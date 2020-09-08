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
#' @include paramset.R
#' @include optimize.R
#' @examples
#' library("paradox")
#' library("bbotk")
#'
#' # silly example function: minimize x^2 for -1 < x < 1
#' domain <- ParamSet$new(list(ParamDbl$new("x", lower = -1, upper = 1)))
#' codomain <- ParamSet$new(list(ParamDbl$new("y", tags = "minimize")))
#' objective <- ObjectiveRFun$new(function(xs) list(y = xs$x^2), domain, codomain)
#'
#' # initialize instance
#' instance <- OptimInstanceSingleCrit$new(objective, domain, trm("evals", n_evals = 6))
#'
#' # use intermbo optimizer
#' optser <- opt("intermbo")
#'
#' # optimizer has hyperparameters from mlrMBO
#' optser$param_set$values$final.method <- "best.predicted"
#'
#' # optimization happens here.
#' optser$optimize(instance)
#'
#' instance$result
#' @export
OptimizerInterMBO <- R6Class("OptimizerInterMBO",
  inherit = bbotk::Optimizer,
  public = list(
    n.objectives = NULL,
    on.surrogate.error = NULL,
    r.session = NULL,
    initialize = function(n.objectives = 1, on.surrogate.error = "stop") {
      self$n.objectives <- assertCount(n.objectives, positive = TRUE)
      self$on.surrogate.error <- assertChoice(on.surrogate.error, c("stop", "warn", "quiet"))
      self$r.session <- initSession(callr::r_session$new(wait_timeout = 10000))
      warnIfPHLoaded()
      super$initialize(
        param_set = mboParamSet(n.objectives),
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        packages = "mlr3",
        properties= c("single-crit", "multi-crit", "dependencies")
      )
    }
  ),
  private = list(
    .optimize = optimize,
    .assign_result = assignResult
  )
)

#' @rdname OptimizerInterMBO
#' @export
TunerInterMBO <- R6Class("TunerInterMBO",
  inherit = mlr3tuning::TunerFromOptimizer,
  public = list(
    initialize = function(n.objectives = 1, on.surrogate.error = "stop") {
      super$initialize(OptimizerInterMBO$new(n.objectives, on.surrogate.error))
    }
  )
)
