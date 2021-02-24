

mboParamSet <- function(n.objectives) {
  ps <- ParamSet$new(c(list(
    # setMBOControl
    ParamInt$new("propose.points", lower = 1, default = 1),
    ParamFct$new("final.method", levels = c("best.true.y", "last.proposed", "best.predicted"), default = "best.true.y"),
    # setMBOControlInfill
    ParamFct$new("infill.crit", levels = c("MeanResponse", "StandardError", "EI", "CB", "AEI", "EQI", if (n.objectives > 1) "DIB", "AdaCB"), default = "CB"),
    ParamDbl$new("infill.crit.se.threshold", lower = 0, default = 1e-6),
    ParamDbl$new("infill.crit.cb.lambda", special_vals = list(NULL), default = NULL),
    ParamLgl$new("infill.crit.aei.use.nugget", default = FALSE),
    ParamDbl$new("infill.crit.eqi.beta", lower = 0.5, upper = 1, default = 0.75),  # TODO not sure about bounds
    ParamDbl$new("infill.crit.sms.eps", lower = 0, special_vals = list(NULL), default = NULL),
    ParamDbl$new("infill.crit.cb.lambda.start", special_vals = list(NULL), default = NULL),
    ParamDbl$new("infill.crit.cb.lambda.end", special_vals = list(NULL), default = NULL),
    ParamInt$new("infill.interleave.random.points", lower = 0, default = 0),
    ParamLgl$new("infill.filter.proposed.points", default = FALSE),
    ParamDbl$new("infill.filter.proposed.points.tol", lower = 0, default = 1e-4),
    ParamFct$new("infill.opt", levels = c("focussearch", "cmaes", "ea", if (n.objectives > 1) "nsga2"), default = "focussearch"),
    ParamInt$new("infill.opt.restarts", lower = 1, default = 3),
    ParamInt$new("infill.opt.focussearch.maxit", lower = 1, default = 5),
    ParamInt$new("infill.opt.focussearch.points", lower = 1, default = 1000),
    ParamUty$new("infill.opt.cmaes.control"),
    ParamInt$new("infill.opt.ea.maxit", lower = 1, default = 500),
    ParamInt$new("infill.opt.ea.mu", lower = 2),
    ParamDbl$new("infill.opt.ea.sbx.eta", lower = 0, default = 15),  # TODO not sure about bounds
    ParamDbl$new("infill.opt.ea.sbx.p", lower = 0, upper = 1, default = 0.5),  # TODO not sure about bounds; is the default correct?
    ParamDbl$new("infill.opt.ea.pm.eta", lower = 0, default = 15),  # TODO not sure about bounds
    ParamDbl$new("infill.opt.ea.pm.p", lower = 0, upper = 1, default = 0.5),  # TODO not sure about bounds; is the default correct?
    ParamInt$new("infill.opt.ea.lambda", lower = 1, default = 1)
  ), if (n.objectives > 1) list(
    ParamInt$new("infill.opt.nsga2.popsize", lower = 1, default = 100),
    ParamInt$new("infill.opt.nsga2.generations", lower = 1, default = 50),
    ParamDbl$new("infill.opt.nsga2.cprob", lower = 0, upper = 1, default = 0.7),
    ParamDbl$new("infill.opt.nsga2.cdist", lower = 0, default = 5), # TODO not sure about bound
    ParamDbl$new("infill.opt.nsga2.mprob", lower = 0, upper = 1, default = 0.2),
    ParamDbl$new("infill.opt.nsga2.mdist", lower = 0, default = 10)
  ), list(
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
    ParamUty$new("surrogate.learner", custom_check = detachEnv(function(x) checkClass(x, "Learner", null.ok = TRUE)))
  ), if (n.objectives > 1) list(
    # setMBOControlMultiObj
    ParamFct$new("multiobj.method", levels = c("parego", "dib", "mspot"), default = "dib"),
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
  ## ParamInt$new("final.evals", lower = 0, default = 0) -- would probably not make sense, see https://github.com/mlr-org/mlrMBO/issues/498

}
