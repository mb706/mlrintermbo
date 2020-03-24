
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
  inherit = mlr3tuning::Tuner,
  public = list(
    n.objectives = NULL,
    opt.state = NULL,
    initialize = function(n.objectives = 1) {
      self$n.objectives <- n.objectives
      ps <- ParamSet$new(c(list(
        # setMBOControl
        ParamInt$new("propose.points", lower = 1, default = 1),
        ParamFct$new("final.method", levels = c("best.true.y", "last.proposed", "best.predicted"), default = "best.true.y"),
        ParamInt$new("final.evals", lower = 0, default = 0),
        # setMBOControlInfill
        ParamFct$new("infill.crit", levels = c("MeanResponse", "StandardError", "EI", "CB", "AEI", "EQI", "DIB", "AdaCB"), default = "CB"),
        ParamDbl$new("infill.crit.se.threshold", lower = 0, default = 1e-6),
        ParamDbl$new("infill.crit.cb.lambda", special_vals = list(NULL), default = NULL),
        ParamLgl$new("infill.crit.aei.use.nugget", default = FALSE),
        ParamDbl$new("infill.crit.eqi.beta", lower = 0, upper = 1, default = 0.75),  # TODO not sure about bounds
        ParamDbl$new("infill.crit.sms.eps", lower = 0, special_vals = list(NULL), default = NULL),
        ParamDbl$new("infill.crit.cb.lambda.start", special_vals = list(NULL), default = NULL),
        ParamDbl$new("infill.crit.cb.lambda.end", special_vals = list(NULL), default = NULL),
        ParamInt$new("infill.interleave.random.points", lower = 0, default = 0),
        ParamLgl$new("infill.filter.proposed.points", default = FALSE),
        ParamDbl$new("infill.filter.proposed.points.tol", lower = 0, default = 1e-4),
        ParamFct$new("infill.opt", levels = c("focussearch", "cmaes", "ea", "nsga2"), default = "focussearch"),
        ParamInt$new("infill.opt.restarts", lower = 1, default = 3),
        ParamInt$new("infill.opt.focussearch.maxit", lower = 1, default = 5),
        ParamInt$new("infill.opt.focussearch.points", lower = 1, default = 1000),
        ParamUty$new("infill.opt.cmaes.control"),
        ParamInt$new("infill.opt.ea.maxit", lower = 1, default = 500),
        ParamInt$new("infill.opt.ea.mu", default = 1),
        ParamDbl$new("infill.opt.ea.sbx.eta", lower = 0, default = 15),  # TODO not sure about bounds
        ParamDbl$new("infill.opt.ea.sbx.p", lower = 0, upper = 1, default = 0.5),  # TODO not sure about bounds; is the default correct?
        ParamDbl$new("infill.opt.ea.pm.eta", lower = 0, default = 15),  # TODO not sure about bounds
        ParamDbl$new("infill.opt.ea.pm.p", lower = 0, upper = 1, default = 0.5),  # TODO not sure about bounds; is the default correct?
        ParamInt$new("infill.opt.ea.lambda", lower = 1, default = 1),
        ParamInt$new("infill.opt.nsga2.popsize", lower = 1, default = 100),
        ParamInt$new("infill.opt.nsga2.generations", lower = 1, default = 50),
        ParamDbl$new("infill.opt.nsga2.cprob", lower = 0, upper = 1, default = 0.7),
        ParamDbl$new("infill.opt.nsga2.cdist", lower = 0, default = 5), # TODO not sure about bound
        ParamDbl$new("infill.opt.nsga2.mprob", lower = 0, upper = 1, default = 0.2),
        ParamDbl$new("infill.opt.nsga2.mdist", lower = 0, default = 10),
        # setMBOControlMultiPoint
        ParamFct$new("multipoint.method", levels = c("cb", "moimbo", "cl"), default = "cb"),
          # multipoint.method == cb --> do not define infill.opt.cb.lambda
          #                   == "moimbo" --> mboinfillcrit ignored
          #                   == cl --> infill.crit == cb
        ParamUty$new("multipoint.cl.lie", custom_check = checkFunction, default = min),
        ParamFct$new("multipoint.moimbo.objective", levels = c("mean.dist", "ei.dist", "mean.se", "mean.se.dist"), default = "ei.dist"),
        ParamFct$new("multipoint.moimbo.dist", levels = c("nearest.neighbor", "nearest.better"), default = "nearest.better"),
        ParamFct$new("multipoint.moimbo.selection", levels = c("hypervolume", "crowdingdist", "first", "last"), default = "hypervolume"),
        ParamInt$new("multipoint.moimbo.maxit", lower = 1, default = 100),
        ParamDbl$new("multipoint.moimbo.sbx.eta", lower = 0, default = 15),  # TODO not sure about bounds
        ParamDbl$new("multipoint.moimbo.sbx.p", lower = 0, upper = 1, default = 1),  # TODO not sure about bounds; is the default correct?
        ParamDbl$new("multipoint.moimbo.pm.eta", lower = 0, default = 15),  # TODO not sure about bound
        ParamDbl$new("multipoint.moimbo.pm.p", lower = 0, upper = 1, default = 1),  # TODO not sure about bounds; is the default correct?moimbo
        # others
        ParamInt$new("initial.design.size", lower = 0),
        ParamUty$new("surrogate.learner", custom_check = detachEnv(function(x) checkClass(x, "LearnerDesc")))
      ), if (n.objectives > 1) list(
        # setMBOControlMultiObj
        ParamFct$new("multiobj.method", levels = c("parego", "dib", "mspot"), default= "dib"),
        ParamFct$new("multiobj.ref.point.method", levels = c("all", "front", "const"), default = "all"),
        ParamDbl$new("multiobj.ref.point.offset", default = 1),
        ParamUty$new("multiobj.ref.point.val", custom_check = detachEnv(function(x) checkNumeric(x, any.missing = FALSE, len = n.objectives), "n.objectives")),
        ParamInt$new("multiobj.parego.s", lower = 1),
        ParamDbl$new("multiobj.parego.rho", default = 0.05),
        ParamLgl$new("multiobj.parego.use.margin.points", default = FALSE),
        ParamInt$new("multiobj.parego.sample.more.weights", lower = 1),
        ParamFct$new("multiobj.parego.normalize", levels = c("standard", "front"), default = "standard"),
        ParamFct$new("multiobj.dib.indicator", levels = c("sms", "eps"), default = "sms"),
        ParamFct$new("multiobj.mspot.select.crit", c("MeanResponse", "CB"), default = "MeanResponse"),
        ParamDbl$new("multiobj.mspot.select.crit.cb.lambda", special_vals = list(NULL), default = NULL)
      )))$
        add_dep("infill.crit.se.threshold", "infill.crit", CondAnyOf$new(c("EI", "AEI", "EQI")))$
        add_dep("infill.crit.cb.lambda", "infill.crit", CondAnyOf$new(c("CB", "DIB")))$
        add_dep("infill.crit.aei.use.nugget", "infill.crit", CondEqual$new("AEI"))$
        add_dep("infill.crit.eqi.beta", "infill.crit", CondEqual$new("EQI"))$
        add_dep("infill.crit.sms.eps", "infill.crit", CondEqual$new("DIB"))$
        add_dep("infill.crit.cb.lambda.start", "infill.crit", CondEqual$new("AdaCB"))$
        add_dep("infill.crit.cb.lambda.end", "infill.crit", CondEqual$new("AdaCB"))$
        add_dep("infill.filter.proposed.points.tol", "infill.filter.proposed.points", CondEqual$new(TRUE))$
        add_dep("infill.opt.focussearch.maxit", "infill.opt", CondEqual$new("focussearch"))$
        add_dep("infill.opt.focussearch.points", "infill.opt", CondEqual$new("focussearch"))$
        add_dep("infill.opt.cmaes.control", "infill.opt", CondEqual$new("cmaes"))$
        add_dep("infill.opt.ea.maxit", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.mu", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.sbx.eta", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.sbx.p", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.pm.eta", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.pm.p", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.ea.lambda", "infill.opt", CondEqual$new("ea"))$
        add_dep("infill.opt.nsga2.popsize", "infill.opt", CondEqual$new("nsga2"))$
        add_dep("infill.opt.nsga2.generations", "infill.opt", CondEqual$new("nsga2"))$
        add_dep("infill.opt.nsga2.cprob", "infill.opt", CondEqual$new("nsga2"))$
        add_dep("infill.opt.nsga2.cdist", "infill.opt", CondEqual$new("nsga2"))$
        add_dep("infill.opt.nsga2.mprob", "infill.opt", CondEqual$new("nsga2"))$
        add_dep("infill.opt.nsga2.mdist", "infill.opt", CondEqual$new("nsga2"))$
        add_dep("multipoint.cl.lie", "multipoint.method", CondEqual$new("cl"))$
        add_dep("multipoint.moimbo.objective", "multipoint.method", CondEqual$new("moimbo"))$
        add_dep("multipoint.moimbo.dist", "multipoint.method", CondEqual$new("moimbo"))$
        add_dep("multipoint.moimbo.selection", "multipoint.method", CondEqual$new("moimbo"))$
        add_dep("multipoint.moimbo.maxit", "multipoint.method", CondEqual$new("moimbo"))$
        add_dep("multipoint.moimbo.sbx.eta", "multipoint.method", CondEqual$new("moimbo"))$
        add_dep("multipoint.moimbo.sbx.p", "multipoint.method", CondEqual$new("moimbo"))$
        add_dep("multipoint.moimbo.pm.eta", "multipoint.method", CondEqual$new("moimbo"))$
        add_dep("multipoint.moimbo.pm.p", "multipoint.method", CondEqual$new("moimbo"))

      if (n.objectives > 1) {
        ps$
          add_dep("multiobj.ref.point.method", "multiobj.method", CondAnyOf$new(c("mspot", "dib")))$
          add_dep("multiobj.ref.point.offset", "multiobj.ref.point.method", CondAnyOf$new(c("all", "front")))$
          add_dep("multiobj.ref.point.val", "multiobj.ref.point.method", CondEqual$new("const"))$
          add_dep("multiobj.parego.s", "multiobj.method", CondEqual$new("parego"))$
          add_dep("multiobj.parego.rho", "multiobj.method", CondEqual$new("parego"))$
          add_dep("multiobj.parego.use.margin.points", "multiobj.method", CondEqual$new("parego"))$
          add_dep("multiobj.parego.sample.more.weights", "multiobj.method", CondEqual$new("parego"))$
          add_dep("multiobj.parego.normalize", "multiobj.method", CondEqual$new("parego"))$
          add_dep("multiobj.dib.indicator", "multiobj.method", CondEqual$new("dib"))$
          add_dep("multiobj.mspot.select.crit", "multiobj.method", CondEqual$new("mspot"))$
          add_dep("multiobj.mspot.select.crit.cb.lambda", "multiobj.mspot.select.crit", CondEqual$new("CB"))
      }

      super$initialize(
        param_set = ps,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = "dependencies"
      )
    }
  ),
  private = list(
    .tune = function(instance) {
      if (length(instance$measures) < self$n.objectives) {
        stopf("instance needs to have at least n.objectives (= %s) measures", self$n.objectives)
      }
      vals <- self$param_set$values
      par.set <- ParamHelpersParamSet(instance$param_set)
      learner <- if (!is.null(vals$surrogate.learner)) GetLearnerFromDesc(vals$surrogate.learner)
      minimize <- sapply(instance$measures, `[[`, "minimize")

      init.size <- vals$initial.design.size %??% length(par.set$pars) * 4

      if (init.size != 0 && instance$n_evals != 0) {
        warning("Both 'initial.design.size' and TuningInstance's n_evals are nonzero, so the initial may be larger than you expect.")
      }

      if (init.size > 0) {
        instance$eval_batch(generate_design_lhs(instance$param_set, init.size)$data)
        # so *in principle* we could stop here if the budget is exhausted. However, we
        # do need to construct the `opt.state` so `assign_result` can do its thing.
        # Therefore we still need to get to the `constructMBOControl` part.
      }

      archive <- as.data.frame(instance$bmr$aggregate(instance$measures[seq_len(self$n.objectives)], ids = FALSE))
      design <- archive[sapply(instance$measures, `[[`, "id")]
      colnames(design) <- sprintf(".PERFORMANCE.%s", seq_len(self$n.objectives))
      design <- cbind(do.call(rbind.data.frame, archive$tune_x), design)
      evalres <- encall(function(constructMBOControl, repairParamDF, vals, n.objectives, still_needs_proposition, ...) {
        suppressMessages(library("mlr"))  # this is necessary because dum-dum mlr does things in .onAttach that should be done in .onLoad
        control <- constructMBOControl(vals = vals, n.objectives = n.objectives)
        opt.state <- mlrMBO::initSMBO(control = control, ...)
        proposition <- NULL
        if (still_needs_proposition) {
          proposition <- mlrMBO::proposePoints(opt.state)
          proposition$prop.points <- repairParamDF(ParamHelpers::getParamSet(opt.state$opt.problem$fun), proposition$prop.points)
        }

        list(
          proposition = proposition,
          opt.state = serialize(opt.state, NULL)  # the opt.state contains references to namespaces which we don't want to load in the main session; therefore we serialize
        )
      }, .args = list(constructMBOControl = detachEnv(constructMBOControl, basis = baseenv()), repairParamDF = detachEnv(repairParamDF, basis = baseenv()),
        vals = vals, n.objectives = self$n.objectives,
        still_needs_proposition = !instance$terminator$is_terminated(instance),  # this is ugly unfortunately; if the initial design exhausts the budget we won't need proposal.
        par.set = par.set, design = design, learner = learner, minimize = minimize))

      self$opt.state <- evalres$opt.state
      proposition <- evalres$proposition

      if (is.null(proposition)) {
        # budget exhausted after initial design
        return(invisible(NULL))
      }

      repeat {
        design <- proposition$prop.points
        proposition$prop.points <- NULL
        # TODO: the following are overwritten because they appear to be broken in mlrMBO. maybe repair this
        proposition$crit.components <- NULL
        proposition$prop.type <- NULL
        extra <- as.data.frame(proposition, stringsAsFactors = FALSE)
        evals <- instance$eval_batch(as.data.table(design))
        extra$uhash <- evals$uhashes
        uinsert(instance$bmr$rr_data, extra, "uhash")

        if (instance$terminator$is_terminated(instance)) {
          return(invisible(NULL))
        }

        perfs <- as.data.frame(evals$perf)[seq_len(self$n.objectives)]

        evalres <- encall(function(repairParamDF, opt.state, ...) {
          suppressMessages(library("mlr"))  # this is necessary because dum-dum mlr does things in .onAttach that should be done in .onLoad
          opt.state <- mlrMBO::updateSMBO(opt.state = unserialize(opt.state), ...)
          proposition <- mlrMBO::proposePoints(opt.state)
          proposition$prop.points <- repairParamDF(ParamHelpers::getParamSet(opt.state$opt.problem$fun), proposition$prop.points)
          list(proposition = proposition, opt.state = serialize(opt.state, NULL))
        }, .args = list(repairParamDF = detachEnv(repairParamDF, basis = baseenv()), opt.state = self$opt.state, x = design, y = as.list(as.data.frame(t(perfs)))))

        self$opt.state <- evalres$opt.state
        proposition <- evalres$proposition
      }
    },
    assign_result = function(instance) {
      final <- encall(function(opt.state) {
        suppressMessages(library("mlr"))  # this is necessary because dum-dum mlr does things in .onAttach that should be done in .onLoad
        resobj <- mlrMBO::finalizeSMBO(unserialize(opt.state))
        resobj[intersect(names(resobj), c("pareto.set", "x", "y", "best.ind"))]
      }, .args = list(opt.state = self$opt.state))
      if (self$n.objectives > 1) {
        final$pareto.set # (list of named lists in pre-trafo space)
        # TODO: multicrit is not handled by mlr3tuning, so we don't know what to do here...
        super$assign_result(instance)
      } else {
        rr <- instance$bmr$resample_result(final$best.ind)
        perf <- rr$aggregate(instance$measures)
        pv <- instance$bmr$rr_data$tune_x[[final$best.ind]]
        instance$assign_result(pv, perf)
      }
      invisible(self)
    }
  )
)

uinsert = function(x, y, key) {
  cn = setdiff(names(y), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s", cn), collapse = ","), ")"))
  x[y, eval(expr), on = key]
}



# create mlrMBO control object from parameter values
constructMBOControl <- function(vals, n.objectives) {
  getVals <- function(delete.prefix, vn = "", vn.is.prefix = TRUE) {
    checkmate::assertCharacter(vn)
    checkmate::assertString(delete.prefix)
    checkmate::assertFlag(vn.is.prefix)
    vn <- paste0(delete.prefix, vn)
    unlist(unname(lapply(vn, function(v) {
      if (vn.is.prefix) {
        take <- names(vals)[startsWith(names(vals), v)]
      } else {
        take <- intersect(names(vals), v)
      }
      li <- vals[take]
      if (nchar(delete.prefix)) {
        names(li) <- sub(delete.prefix, "", names(li), fixed = TRUE)
      }
      li
    })), recursive = FALSE)
  }

  control <- mlr3misc::invoke(mlrMBO::makeMBOControl,
    n.objectives = n.objectives, y.name = sprintf(".PERFORMANCE.%s", seq_len(n.objectives)),
    .args = getVals("", c("propose.points", "final.method", "final.evals"), FALSE))

  if (!is.null(vals$infill.crit)) {
    vals$infill.crit <- switch(vals$infill.crit,
      MeanResponse = mlrMBO::makeMBOInfillCritMeanResponse(),
      StandardError = mlrMBO::makeMBOInfillCritStandardError(),
      EI = do.call(mlrMBO::makeMBOInfillCritEI, getVals("infill.crit.", "se")),
      CB = do.call(mlrMBO::makeMBOInfillCritCB, getVals("infill.crit.", "cb.lambda", FALSE)),
      AEI = do.call(mlrMBO::makeMBOInfillCritAEI, getVals("infill.crit.", c("aei", "se"))),
      EQI = do.call(mlrMBO::makeMBOInfillCritEQI, getVals("infill.crit.", c("eqi", "se"))),
      DIB = do.call(mlrMBO::makeMBOInfillCritDIB, getVals("infill.crit.", c("cb.lambda", "sms.eps"), FALSE)),
      AdaCB = do.call(mlrMBO::makeMBOInfillCritAdaCB, getVals("infill.crit.", "cb.lambda.")),
      stop("bad infill.crit parameter")
    )
  }


  control <- mlr3misc::invoke(mlrMBO::setMBOControlInfill, control = control,
    .args = c(getVals("infill.", "crit", FALSE),
      getVals("infill.", c("interleave", "filter", "opt"))))
  if (checkmate::`%??%`(vals$propose.points, 1) != 1) {
    control <- mlr3misc::invoke(mlrMBO::setMBOControlMultiPoint, control = control,
      .args = getVals("multipoint."))
  }
  if (n.objectives > 1) {
    if (!is.null(vals$multiobj.mspot.select.crit)) {
      vals$multiobj.mspot.select.crit <- switch(vals$multiobj.mspot.select.crit,
        MeanResponse = mlrMBO::makeMBOInfillCritMeanResponse(),
        CB = do.call(mlrMBO::makeMBOInfillCritCB, getVals("multiobj.mspot.select.crit.", "cb.lambda")),
        stop("bad multiobj.mspot.select.crit parameter")
      )
      vals$multiobj.mspot.select.crit.cb.lambda <- NULL
    }
    control <- mlr3misc::invoke(mlrMBO::setMBOControlMultiObj, control = control,
      .args = getVals("multiobj."))
  }
  # need to overwrite the default isters '10'
  mlrMBO::setMBOControlTermination(control, iters = .Machine$integer.max)
}

repairParamDF <- function(par.set, df) {
  do.call(rbind.data.frame, lapply(ParamHelpers::dfRowsToList(df, par.set), ParamHelpers::repairPoint, par.set = par.set))
}




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

#' @title Learner Description without loading mlr
#'
#' @description
#' constructs a learner in an R-session if `check` is TRUE, otherwise
#' just stores the parameters
#'
#' @param learnername name of the learner
#' @param params named hyperparameter value list
#' @param check whether to construct the learner
#' @return LearnerDescription object.
#' @export
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

#' @title Construct a Learner without loading mlr
#'
#' @description
#' constructs a learner in an R-session if the learner description was not constructed with 'check = TRUE'.
#'
#' @param learnerDescription a LearnerDescription() constructed object.
#' @return LearnerInnst object.
#' @export
GetLearnerFromDesc <- function(learnerDescription) {
  UseMethod("GetLearnerFromDesc")
}

GetLearnerFromDesc.LearnerDesc <- function(learnerDescription) {
  object <- learnerDescription
  object$object <- encall(function(ld) {
    suppressMessages(library("mlr"))  # this is necessary because dum-dum mlr does things in .onAttach that should be done in .onLoad
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
  encall(function(d) {
    ParamHelpers::makeParamSet(params = lapply(d, function(pcon) {
      do.call(get(pcon[[1]], getNamespace("ParamHelpers"), mode = "function"), pcon[[2]])
    }))
  }, .args = list(d = data))
}

encall <- function(.f, ...) {
  x <- encapsulate("callr", .f = detachEnv(.f, basis = baseenv()), ...)
  for (line in transpose_list(x$log)) {
    switch(as.character(line$class),
      output = catf("%s", line$msg),
      warning = warning(line$msg),
      error = stop(line$msg)
    )
  }
  x$result
}
