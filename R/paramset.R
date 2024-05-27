

mboParamSet <- function(n.objectives) {
  ps.list <- c(list(
    # setMBOControl
    propose.points = p_int(lower = 1, default = 1),
    final.method = p_fct(levels = c("best.true.y", "last.proposed", "best.predicted"), default = "best.true.y"),
    # setMBOControlInfill
    infill.crit = p_fct(levels = c("MeanResponse", "StandardError", "EI", "CB", "AEI", "EQI", if (n.objectives > 1) "DIB", "AdaCB"), default = "CB"),
    infill.crit.se.threshold = p_dbl(lower = 0, default = 1e-6, depends = quote(infill.crit %in% c("EI", "AEI", "EQI")))
  ), if (n.objectives > 1) list(
    infill.crit.cb.lambda = p_dbl(special_vals = list(NULL), default = NULL, depends = quote(infill.crit %in% c("CB", "DIB")))
  ) else list(
    infill.crit.cb.lambda = p_dbl(special_vals = list(NULL), default = NULL, depends = quote(infill.crit == "CB"))
  ), list(
    infill.crit.aei.use.nugget = p_lgl(default = FALSE, depends = quote(infill.crit == "AEI")),
    infill.crit.eqi.beta = p_dbl(lower = 0.5, upper = 1, default = 0.75, depends = quote(infill.crit == "EQI"))  # TODO not sure about bounds
  ), if (n.objectives > 1) list(
    infill.crit.sms.eps = p_dbl(lower = 0, special_vals = list(NULL), default = NULL, depends = quote(multiobj.dib.indicator == "sms"))
  ) else list(
    infill.crit.sms.eps = p_dbl(lower = 0, special_vals = list(NULL), default = NULL)
  ), list(
    infill.crit.cb.lambda.start = p_dbl(special_vals = list(NULL), default = NULL, depends = quote(infill.crit == "AdaCB")),
    infill.crit.cb.lambda.end = p_dbl(special_vals = list(NULL), default = NULL, depends = quote(infill.crit == "AdaCB")),
    infill.interleave.random.points = p_int(lower = 0, default = 0),
    infill.filter.proposed.points = p_lgl(default = FALSE),
    infill.filter.proposed.points.tol = p_dbl(lower = 0, default = 1e-4, depends = quote(infill.filter.proposed.points == TRUE)),
    infill.opt = p_fct(levels = c("focussearch", "cmaes", "ea", if (n.objectives > 1) "nsga2"), default = "focussearch"),
    infill.opt.restarts = p_int(lower = 1, default = 3),
    infill.opt.focussearch.maxit = p_int(lower = 1, default = 5, depends = quote(infill.opt == "focussearch")),
    infill.opt.focussearch.points = p_int(lower = 1, default = 1000, depends = quote(infill.opt == "focussearch")),
    infill.opt.cmaes.control = p_uty(depends = quote(infill.opt == "cmaes")),
    infill.opt.ea.maxit = p_int(lower = 1, default = 500, depends = quote(infill.opt == "ea")),
    infill.opt.ea.mu = p_int(lower = 2, depends = quote(infill.opt == "ea")),
    infill.opt.ea.sbx.eta = p_dbl(lower = 0, default = 15, depends = quote(infill.opt == "ea")),  # TODO not sure about bounds
    infill.opt.ea.sbx.p = p_dbl(lower = 0, upper = 1, default = 0.5, depends = quote(infill.opt == "ea")),  # TODO not sure about bounds; is the default correct?
    infill.opt.ea.pm.eta = p_dbl(lower = 0, default = 15, depends = quote(infill.opt == "ea")),  # TODO not sure about bounds
    infill.opt.ea.pm.p = p_dbl(lower = 0, upper = 1, default = 0.5, depends = quote(infill.opt == "ea")),  # TODO not sure about bounds; is the default correct?
    infill.opt.ea.lambda = p_int(lower = 1, default = 1, depends = quote(infill.opt == "ea"))
  ), if (n.objectives > 1) list(
    infill.opt.nsga2.popsize = p_int(lower = 1, default = 100, depends = quote(infill.opt == "nsga2")),
    infill.opt.nsga2.generations = p_int(lower = 1, default = 50, depends = quote(infill.opt == "nsga2")),
    infill.opt.nsga2.cprob = p_dbl(lower = 0, upper = 1, default = 0.7, depends = quote(infill.opt == "nsga2")),
    infill.opt.nsga2.cdist = p_dbl(lower = 0, default = 5, depends = quote(infill.opt == "nsga2")), # TODO not sure about bound
    infill.opt.nsga2.mprob = p_dbl(lower = 0, upper = 1, default = 0.2, depends = quote(infill.opt == "nsga2")),
    infill.opt.nsga2.mdist = p_dbl(lower = 0, default = 10, depends = quote(infill.opt == "nsga2"))
  ), list(
    # setMBOControlMultiPoint
    multipoint.method = p_fct(levels = c("cb", "moimbo", "cl"), default = "cb"),
      # multipoint.method == cb --> do not define infill.opt.cb.lambda
      #                   == "moimbo" --> mboinfillcrit ignored
      #                   == cl --> infill.crit == cb
    multipoint.cl.lie = p_uty(custom_check = checkFunction, default = min, depends = quote(multipoint.method == "cl")),
    multipoint.moimbo.objective = p_fct(levels = c("mean.dist", "ei.dist", "mean.se", "mean.se.dist"), default = "ei.dist", depends = quote(multipoint.method == "moimbo")),
    multipoint.moimbo.dist = p_fct(levels = c("nearest.neighbor", "nearest.better"), default = "nearest.better", depends = quote(multipoint.method == "moimbo")),
    multipoint.moimbo.selection = p_fct(levels = c("hypervolume", "crowdingdist", "first", "last"), default = "hypervolume", depends = quote(multipoint.method == "moimbo")),
    multipoint.moimbo.maxit = p_int(lower = 1, default = 100, depends = quote(multipoint.method == "moimbo")),
    multipoint.moimbo.sbx.eta = p_dbl(lower = 0, default = 15, depends = quote(multipoint.method == "moimbo")),  # TODO not sure about bounds
    multipoint.moimbo.sbx.p = p_dbl(lower = 0, upper = 1, default = 1, depends = quote(multipoint.method == "moimbo")),  # TODO not sure about bounds; is the default correct?
    multipoint.moimbo.pm.eta = p_dbl(lower = 0, default = 15, depends = quote(multipoint.method == "moimbo")),  # TODO not sure about bound
    multipoint.moimbo.pm.p = p_dbl(lower = 0, upper = 1, default = 1, depends = quote(multipoint.method == "moimbo")),  # TODO not sure about bounds; is the default correct?moimbo
    # others
    initial.design.size = p_int(lower = 0),
    surrogate.learner = p_uty(custom_check = detachEnv(function(x) checkClass(x, "Learner", null.ok = TRUE)))
  ), if (n.objectives > 1) list(
    # setMBOControlMultiObj
    multiobj.method = p_fct(levels = c("parego", "dib", "mspot"), default = "dib"),
    multiobj.ref.point.method = p_fct(levels = c("all", "front", "const"), default = "all", depends = quote(multiobj.method %in% c("mspot", "dib"))),
    multiobj.ref.point.offset = p_dbl(default = 1, depends = quote(multiobj.ref.point.method %in% c("all", "front"))),
    multiobj.ref.point.val = p_uty(custom_check = detachEnv(function(x) checkNumeric(x, any.missing = FALSE, len = n.objectives), "n.objectives"), depends = quote(multiobj.ref.point.method == "const")),
    multiobj.parego.s = p_int(lower = 1, depends = quote(multiobj.method == "parego")),
    multiobj.parego.rho = p_dbl(default = 0.05, depends = quote(multiobj.method == "parego")),
    multiobj.parego.use.margin.points = p_lgl(default = FALSE, depends = quote(multiobj.method == "parego")),
    multiobj.parego.sample.more.weights = p_int(lower = 1, depends = quote(multiobj.method == "parego")),
    multiobj.parego.normalize = p_fct(levels = c("standard", "front"), default = "standard", depends = quote(multiobj.method == "parego")),
    multiobj.dib.indicator = p_fct(levels = c("sms", "eps"), default = "sms", depends = quote(multiobj.method == "dib")),
    multiobj.mspot.select.crit = p_fct(c("MeanResponse", "CB"), default = "MeanResponse", depends = quote(multiobj.method == "mspot")),
    multiobj.mspot.select.crit.cb.lambda = p_dbl(special_vals = list(NULL), default = NULL, depends = quote(multiobj.mspot.select.crit == "CB"))
  ))

  do.call(paradox::ps, ps.list)

  ## Removed:
  ## final.evals = p_int(lower = 0, default = 0) -- would probably not make sense, see https://github.com/mlr-org/mlrMBO/issues/498

}
