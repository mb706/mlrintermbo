
#' @title Tuner using mlrMBO
#'
#' @aliases mlr_tuners_intermbo
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
#'
#' @export
TunerInterMBO <- R6Class("TunerInterMBO",
  inherit = Tuner,
  public = list(
    initialize = function(n.objectives) {
      ps = ParamSet$new(c(list(
        ParamInt$new("propose.points", lower = 1, default = 1),
        ParamFct$new("final.method", levels = c("best.true.y", "last.proposed", "best.predicted"), default = "best.true.y"),
        ParamInt$new("final.evals", lower = 0, default = 0),

        ParamFct$new("infill.crit", levels = c("MeanResponse", "StandardError", "EI", "CB", "AEI", "EQI", "DIB", "AdaCB"), default = "CB"),
        ParamDbl$new("infill.crit.se.threshold", lower = 0, default = 1e-6),  #requires infill.crit == StandardError or AEI
        ParamDbl$new("infill.crit.cb.lambda", special_vals = list(NULL), default = NULL),  # requires infill.crit == CB || DIB
        ParamFct$new("infill.crit.aei.use.nugget", default = FALSE),  # requires infill.crit == aei
        ParamDbl$new("infill.crit.eqi.beta", lower = 0, upper = 1, default = 0.75),  # TODO not sure about bounds   # requires infill.crit == EQI
        ParamDbl$new("infill.crit.dib.sms.eps", lower = 0, special_vals = list(NULL), default = NULL),  # TODO not sure about bound  # requires infill.crit == dib
        ParamDbl$new("infill.crit.cb.lambda.start", special_vals = list(NULL), default = NULL),  # TODO requires infill.crit == AdaCB
        ParamDbl$new("infill.crit.cb.lambda.end", special_vals = list(NULL), default = NULL),  # TODO requiresi nfill.crit == AdaCB
        ParamInt$new("infill.interleave.random.points", lower = 0, default = 0),
        ParamLgl$new("infill.filter.proposed.points", default = FALSE),
        ParamDbl$new("infill.filter.propposed.points.tol", lower = 0, default = 1e-4),  # TODO: requires filter.proposed.points == TRUE
        ParamFct$new("infill.opt", levels = c("focussearch", "cmaes", "ea", "nsga2"), default = "focussearch"),
        ParamInt$new("infill.opt.restarts", lower = 1, default = 3),
        ParamInt$new("infill.opt.focussearch.maxit", lower = 1, default = 5), # TODO: requires infill.opt == focussearch
        ParamInt$new("infill.opt.focussearch.points", lower = 1, default = 1000),  # TODO: requires infill.opt == focussearch
        ParamUty$new("infill.opt.cmaes.control"),  # TODO: requires infill.opt = cmaes
        ParamInt$new("infill.opt.ea.maxit", lower = 1, default = 500),  # TODO: requires infill.opt = ea
        ParamInt$new("infill.opt.ea.mu", default = 1),  # TODO: requires infill.opt = ea
        ParamDbl$new("infill.opt.ea.sbx.eta", lower = 0, default = 15),  # TODO not sure about bounds # TODO: requires infill.opt = ea
        ParamDbl$new("infill.opt.ea.sbx.p", lower = 0, upper = 1, default = 0.5),  # TODO not sure about bounds; is the default correct?   # TODO: requires infill.opt = ea
        ParamDbl$new("infill.opt.ea.pm.eta", lower = 0, default = 15),  # TODO not sure about bounds   # TODO: requires infill.opt = ea
        ParamDbl$new("infill.opt.ea.pm.p", lower = 0, upper = 1, default = 0.5),  # TODO not sure about bounds; is the default correct?  # TODO: requires infill.opt = ea
        ParamInt$new("infill.opt.ea.lambda", lower = 1, default = 1),  # TODO: requires infill.opt = ea
        ParamInt$new("infill.opt.nsga2.popsize", lower = 1, default = 100),  # TODO: requires infill.opt = nsga2
        ParamInt$new("infill.opt.nsga2.generations", lower = 1, default = 50),  # TODO: requires infill.opt = nsga2
        ParamDbl$new("infill.opt.nsga2.cprob", lower = 0, upper = 1, default = 0.7),  # TODO: requires infill.opt = nsga2
        ParamDbl$new("infill.opt.nsga2.cdist", lower = 0, default = 5), # TODO not sure about bound # TODO: requires infill.opt = nsga2
        ParamDbl$new("infill.opt.nsga2.mprob", lower = 0, upper = 1, default = 0.2),  # TODO: requires infill.opt = nsga2
        ParamDbl$new("infill.opt.nsga2.mdist", lower = 0, default = 10),  # TODO: requires infill.opt = nsga2

        ParamFct$new("multipoint.method", levels = c("cb", "moimbo", "cl")),
          # multipoint.method == cb --> do not define infill.opt.cb.lambda
          #                   == "moimbo" --> mboinfillcrit ignored
          #                   == cl --> infill.crit == cb
        ParamUty$new("multipoint.cl.lie", custom_check = checkFunction, default = min),  # requires multipoint.method == cl
        ParamFct$new("multipoint.moimbo.objective", levels = c("mean.dist", "ei.dist", "mean.se", "mean.se.dist"), default = "ei.dist"),  # requires multipoint.method == moimbo
        ParamFct$new("multipoint.moimbo.dist", levels = c("nearest.neighbor", "nearest.better"), default = "nearest.better"),  # requires multipoint.method == moimbo
        ParamFct$new("multipoint.moimbo.selection", levels = c("hypervolume", "crowdingdist", "first", "last"), default = "hypervolume"),  # requires multipoint.method == moimbo
        ParamInt$new("multipoint.moimbo.maxit", lower = 1, default = 100),  # requires multipoint.method == moimbo
        ParamDbl$new("multipoint.moimbo.sbx.eta", lower = 0, default = 15),# TODO not sure about bounds  # requires multipoint.method == moimbo
        ParamDbl$new("multipoint.moimbo.sbx.p", lower = 0, upper = 1, default = 1),# TODO not sure about bounds; is the default correct?     # requires multipoint.method == moimbo
        ParamDbl$new("multipoint.moimbo.pm.eta", lower = 0, default = 15),# TODO not sure about bound  # requires multipoint.method == moimbo
        ParamDbl$new("multipoint.moimbo.pm.p", lower = 0, upper = 1, default = 1),# TODO not sure about bounds; is the default correct?     # requires multipoint.method == moimbo
      ), if (n.objectives > 1) list(
        ParamFct$new("multiobj.method", levels = c("parego", "dib", "mspot"), default= "dib"),
        ParamFct$new("multiobj.ref.point.method", levels = c("all", "front", "const"), default = "all"),  # TODO: requires mspot or dib(sms)
        ParamDbl$new("multiobj.ref.point.offset", default = 1),  # TODO: requires method %in% all, front
        ParamUty$new("multiobj.ref.point.val", custom_check = detachEnv(function(x) checkNumeric(x, any.missing = FALSE, len = n.objectives), "n.objectives")),  # TODO requires method == const
        ParamInt$new("multiobj.parego.s", lower = 1),  # TODO requires method parego
        ParamDbl$new("multiobj.parego.rho", default = 0.05),  # TODO requires parego
        ParamLgl$new("multiobj.parego.use.margin.points", default = FALSE),  # TODO requires parego
        ParamInt$new("multiobj.parego.sample.more.weights", default = lower = 1),  # TODO requires parego
        ParamFct$new("multiobj.parego.normalize", levels = c("standard", "front"), default = "standard"),  # TODO requires parego
        ParamFct$new("multiobj.dib.indicator", default = c("sms", "eps")),  # TODO requires dib
        ParamFct$new("multiobj.mspot.select.crit", c("MeanResponse", "CB"), default = "MeanResponse"),  # TODO requires mspot
        ParamDbl$new("multiobj.mspot.select.crit.cb.lambda", special_vals = list(NULL), default = NULL)  # requires multiboj.mspot.select.crit == CB
      )))$
        add_dep("infill.crit.se.threshold", "infill.crit", CondAnyOf$new(c("StandardError", "AEI")))$
        add_dep("infill.crit.cb.lambda", "infill.crit", CondAnyOf$new(c("CB", "DIB")))$
        add_dep("infill.crit.aei.use.nugget", "infill.crit", CondEqual$new("AEI"))$
        add_dep("infill.crit.eqi.beta", "infill.crit", CondEqual$new("EQI"))$
        add_dep("infill.crit.dib.sms.eps", "infill.crit", CondEqual$new("DIB"))$
        add_dep("infill.crit.cb.lambda.start", "infill.crit", CondEqual$new("AdaCB"))$
        add_dep("infill.crit.cb.lambda.end", "infill.crit", CondEqual$new("AdaCB"))$
        add_dep("infill.filter.proposed.points.tol", "filter.proposed.points", CondEqual$new(TRUE))$
        add_dep("infill.opt.focussearch.maxit", "infill.opt", CondEqual$new("focussearch"))$
        add_dep("infill.opt.focussearch.points", "infill.opt", CondEqual$new("focussearch"))$
        add_dep("infill.opt.cmaes.control", "infill.opt", CondEqual$new("cmaes"))$
        add_dep("infill.opt.ea.maxit", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.mu", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.sbx.eta", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.sbx.p", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.pm.eta", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.pm.p", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.pm.lambda", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.nsga2.popsize", "infill.opt", CondEqual$new("nsga2"))$
        add_dep("infill.opt.nsga2.generations", "infill.opt", CondEqual$new("nsga2"))$
        add_dep("infill.opt.nsga2.cprob", "infill.opt", CondEqual$new("nsga2"))$
        add_dep("infill.opt.nsga2.cdist", "infill.opt", CondEqual$new("nsga2"))$
        add_dep("infill.opt.nsga2.mdist", "infill.opt", CondEqual$new("nsga2"))$

        add_dep("infill.opt.", "infill.opt", CondEqual$new(""))$


      super$initialize(
        param_set = ps,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = "dependencies"
      )
    }
  ),
  private = list(
    tune_internal = function(instance) {


    }
  )
)

# removes the environment from fun and potentially wraps it in a new environment
detachEnv <- function(fun, keep = character(0), basis = topenv(parent.frame())) {
  assertEnvironment(basis)
  assertCharacter(keep, any.missing = FALSE)
  assertFunction(fun)
  if (length(keep)) {
    keepvals <- mget(keep, parent.frame(), inherits = TRUE)
    basis <- new.env(parent = basis, size = length(keepvals))
    mapply(assign, names(keepvals), keepvals, MoreArgs = list(envir = basis))
  }
  environment(fun) <- basis
  fun
}

# constructs a learner in an R-session if `check` is TRUE, otherwise
# just stores the parameters
LearnerDescription <- function(learnername, params, check = TRUE) {
  assertCharacter(learnername)
  assertList(params, names = "unique")
  object <- structure(list(learnername = learnername, params = params), class = "LearnerDescription")
  if (check) {
    GetLearnerFromDesc(object)
  } else {
    object
  }
}

GetLearnerFromDesc <- function(learnerDescription) {
  UseMethod("GetLearnerFromDesc")
}

GetLearnerFromDesc.LearnerDesc <- function(learnerDescription) {
  object <- learnerDescription
  object$object <- encapsulate("callr", function(ld) {
    mlr::makeLearner(ld$learnername, par.vals = ld$params)
  }, .args = list(learnerDescription))
  class(object) <- c("LearnerInst", "LearnerDesc")
  object
}

GetLearnerFromDesc.LearnerInst <- function(learnerDescription) {
  learnerDescription
}

# translate a paradox::Condition object into an expression
# condition: the Condition object
# on: the variable the condition depends on
conditionAsExpression <- function(condition, on) {
  UseMethod("ConditionAsExpression")
}

conditionAsExpression.CondAnyOf <- function(condition, on) {
  # the single '&' is taken from paradox::CondAnyOf$test, which may be a bug there.
  substitute(!is.na(on) & x %in% rhs, list(on = as.symbol(on), rhs = condition$rhs))
}

conditionAsExpression.CondEqual <- function(condition, on) {
  # the single '&' is taken from paradox::CondEqual$test, which may be a bug there.
  substitute(!is.na(on) & x == rhs, list(on = as.symbol(on), rhs = condition$rhs))
}

# ParamHelpers::ParamSet from paradox::ParamSet
ParamHelpersParamSet <- function(paramset) {
  getRequires <- function(depname) {
    conds <- paramset$deps[get("id") == depname]
    cond.expressions <- mapply(conditionAsExpression, conds$cond, conds$on)
    Reduce(function(x, y) substitute(x && y, list(x = x, y = y)), cond.expressions)
  }
  data <- imap(paramset$params, function(param, pname) {
    switch(param$class,
      ParamLgl = list("makeLogicalParam", list(id = pname, requires = getRequires(pname))),
      ParamInt = list("makeIntegerParam", list(id = pname, lower = param$lower, upper = param$upper, requires = getRequires(pname))),
      ParamDbl = list("makeNumericParam", list(id = pname, lower = param$lower, upper = param$upper, requires = getRequires(pname))),
      ParamFct = list("makeDiscreteParam", list(id = pname, values = param$levels, requires = getRequires(pname)))
    )
  })
  encapsulate("callr", function(d) {
    ParamHelpers$makeParamSet(params = lapply(d, function(pcon) {
      do.call(get(pcon[[1]], getNamespace("ParamHelpers"), mode = "function"), pcon[[2]])
    }))
  }, .args = list(d = data))
}
