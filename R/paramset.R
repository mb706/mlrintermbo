

mboParamSet <- function(n.objectives) {
  ps <- ParamSet$new(c(list(
    # setMBOControl
    propose.points = p_int(lower = 1, default = 1),
    final.method = p_fct(levels = c("best.true.y", "last.proposed", "best.predicted"), default = "best.true.y"),
    # setMBOControlInfill
    infill.crit = p_fct(levels = c("MeanResponse", "StandardError", "EI", "CB", "AEI", "EQI", if (n.objectives > 1) "DIB", "AdaCB"), default = "CB"),
    infill.crit.se.threshold = p_dbl(lower = 0, default = 1e-6),
    infill.crit.cb.lambda = p_dbl(special_vals = list(NULL), default = NULL),
    infill.crit.aei.use.nugget = p_lgl(default = FALSE),
    infill.crit.eqi.beta = p_dbl(lower = 0.5, upper = 1, default = 0.75),  # TODO not sure about bounds
    infill.crit.sms.eps = p_dbl(lower = 0, special_vals = list(NULL), default = NULL),
    infill.crit.cb.lambda.start = p_dbl(special_vals = list(NULL), default = NULL),
    infill.crit.cb.lambda.end = p_dbl(special_vals = list(NULL), default = NULL),
    infill.interleave.random.points = p_int(lower = 0, default = 0),
    infill.filter.proposed.points = p_lgl(default = FALSE),
    infill.filter.proposed.points.tol = p_dbl(lower = 0, default = 1e-4),
    infill.opt = p_fct(levels = c("focussearch", "cmaes", "ea", if (n.objectives > 1) "nsga2"), default = "focussearch"),
    infill.opt.restarts = p_int(lower = 1, default = 3),
    infill.opt.focussearch.maxit = p_int(lower = 1, default = 5),
    infill.opt.focussearch.points = p_int(lower = 1, default = 1000),
    infill.opt.cmaes.control = p_uty(),
    infill.opt.ea.maxit = p_int(lower = 1, default = 500),
    infill.opt.ea.mu = p_int(lower = 2),
    infill.opt.ea.sbx.eta = p_dbl(lower = 0, default = 15),  # TODO not sure about bounds
    infill.opt.ea.sbx.p = p_dbl(lower = 0, upper = 1, default = 0.5),  # TODO not sure about bounds; is the default correct?
    infill.opt.ea.pm.eta = p_dbl(lower = 0, default = 15),  # TODO not sure about bounds
    infill.opt.ea.pm.p = p_dbl(lower = 0, upper = 1, default = 0.5),  # TODO not sure about bounds; is the default correct?
    infill.opt.ea.lambda = p_int(lower = 1, default = 1)
  ), if (n.objectives > 1) list(
    infill.opt.nsga2.popsize = p_int(lower = 1, default = 100),
    infill.opt.nsga2.generations = p_int(lower = 1, default = 50),
    infill.opt.nsga2.cprob = p_dbl(lower = 0, upper = 1, default = 0.7),
    infill.opt.nsga2.cdist = p_dbl(lower = 0, default = 5), # TODO not sure about bound
    infill.opt.nsga2.mprob = p_dbl(lower = 0, upper = 1, default = 0.2),
    infill.opt.nsga2.mdist = p_dbl(lower = 0, default = 10)
  ), list(
    # setMBOControlMultiPoint
    multipoint.method = p_fct(levels = c("cb", "moimbo", "cl"), default = "cb"),
      # multipoint.method == cb --> do not define infill.opt.cb.lambda
      #                   == "moimbo" --> mboinfillcrit ignored
      #                   == cl --> infill.crit == cb
    multipoint.cl.lie = p_uty(custom_check = checkFunction, default = min),
    multipoint.moimbo.objective = p_fct(levels = c("mean.dist", "ei.dist", "mean.se", "mean.se.dist"), default = "ei.dist"),
    multipoint.moimbo.dist = p_fct(levels = c("nearest.neighbor", "nearest.better"), default = "nearest.better"),
    multipoint.moimbo.selection = p_fct(levels = c("hypervolume", "crowdingdist", "first", "last"), default = "hypervolume"),
    multipoint.moimbo.maxit = p_int(lower = 1, default = 100),
    multipoint.moimbo.sbx.eta = p_dbl(lower = 0, default = 15),  # TODO not sure about bounds
    multipoint.moimbo.sbx.p = p_dbl(lower = 0, upper = 1, default = 1),  # TODO not sure about bounds; is the default correct?
    multipoint.moimbo.pm.eta = p_dbl(lower = 0, default = 15),  # TODO not sure about bound
    multipoint.moimbo.pm.p = p_dbl(lower = 0, upper = 1, default = 1),  # TODO not sure about bounds; is the default correct?moimbo
    # others
    initial.design.size = p_int(lower = 0),
    surrogate.learner = p_uty(custom_check = detachEnv(function(x) checkClass(x, "Learner", null.ok = TRUE)))
  ), if (n.objectives > 1) list(
    # setMBOControlMultiObj
    multiobj.method = p_fct(levels = c("parego", "dib", "mspot"), default = "dib"),
    multiobj.ref.point.method = p_fct(levels = c("all", "front", "const"), default = "all"),
    multiobj.ref.point.offset = p_dbl(default = 1),
    multiobj.ref.point.val = p_uty(custom_check = detachEnv(function(x) checkNumeric(x, any.missing = FALSE, len = n.objectives), "n.objectives")),
    multiobj.parego.s = p_int(lower = 1),
    multiobj.parego.rho = p_dbl(default = 0.05),
    multiobj.parego.use.margin.points = p_lgl(default = FALSE),
    multiobj.parego.sample.more.weights = p_int(lower = 1),
    multiobj.parego.normalize = p_fct(levels = c("standard", "front"), default = "standard"),
    multiobj.dib.indicator = p_fct(levels = c("sms", "eps"), default = "sms"),
    multiobj.mspot.select.crit = p_fct(c("MeanResponse", "CB"), default = "MeanResponse"),
    multiobj.mspot.select.crit.cb.lambda = p_dbl(special_vals = list(NULL), default = NULL)
  )))$
    add_dep("infill.crit.se.threshold", "infill.crit", CondAnyOf$new(c("EI", "AEI", "EQI")))$
    add_dep("infill.crit.cb.lambda", "infill.crit", CondAnyOf$new(c("CB", if (n.objectives > 1) "DIB")))$
    add_dep("infill.crit.aei.use.nugget", "infill.crit", CondEqual$new("AEI"))$
    add_dep("infill.crit.eqi.beta", "infill.crit", CondEqual$new("EQI"))$
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
      add_dep("infill.opt.nsga2.popsize", "infill.opt", CondEqual$new("nsga2"))$
      add_dep("infill.opt.nsga2.generations", "infill.opt", CondEqual$new("nsga2"))$
      add_dep("infill.opt.nsga2.cprob", "infill.opt", CondEqual$new("nsga2"))$
      add_dep("infill.opt.nsga2.cdist", "infill.opt", CondEqual$new("nsga2"))$
      add_dep("infill.opt.nsga2.mprob", "infill.opt", CondEqual$new("nsga2"))$
      add_dep("infill.opt.nsga2.mdist", "infill.opt", CondEqual$new("nsga2"))$
      add_dep("multiobj.ref.point.method", "multiobj.method", CondAnyOf$new(c("mspot", "dib")))$
      add_dep("multiobj.ref.point.offset", "multiobj.ref.point.method", CondAnyOf$new(c("all", "front")))$
      add_dep("multiobj.ref.point.val", "multiobj.ref.point.method", CondEqual$new("const"))$
      add_dep("multiobj.parego.s", "multiobj.method", CondEqual$new("parego"))$
      add_dep("multiobj.parego.rho", "multiobj.method", CondEqual$new("parego"))$
      add_dep("multiobj.parego.use.margin.points", "multiobj.method", CondEqual$new("parego"))$
      add_dep("multiobj.parego.sample.more.weights", "multiobj.method", CondEqual$new("parego"))$
      add_dep("multiobj.parego.normalize", "multiobj.method", CondEqual$new("parego"))$
#      add_dep("infill.crit.sms.eps", "infill.crit", CondEqual$new("DIB"))$
      add_dep("multiobj.dib.indicator", "multiobj.method", CondEqual$new("dib"))$
      add_dep("infill.crit.sms.eps", "multiobj.dib.indicator", CondEqual$new("sms"))$
      add_dep("multiobj.mspot.select.crit", "multiobj.method", CondEqual$new("mspot"))$
      add_dep("multiobj.mspot.select.crit.cb.lambda", "multiobj.mspot.select.crit", CondEqual$new("CB"))
  }
  ps

  ## Removed:
  ## final.evals = p_int(lower = 0, default = 0) -- would probably not make sense, see https://github.com/mlr-org/mlrMBO/issues/498

}
