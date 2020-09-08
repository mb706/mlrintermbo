#' @include utils.R
#' @include ParamHelpersParamSet.R
#' @include CapsuledMlr3Learner.R


persistent <- new.env(parent = baseenv())  # persistent storage for opt.state and proposition in background R session

optimize <- function(instance) {
  warnIfPHLoaded()

  if (instance$objective$codomain$length != self$n.objectives) {
    stopf("instance needs to have n.objectives (= %s) objectives", self$n.objectives)
  }

  vals <- self$param_set$values

  if (identical(vals$multiobj.method, "mspot") || identical(vals$multiobj.method, "parego")) {
    warning("When this function was written, mlrMBO had a bug that prevented mspot / parego multiobjective optimization from working.
See https://github.com/mlr-org/mlrMBO/issues/474")
  }

  n.objectives <- self$n.objectives
  on.surrogate.error <- self$on.surrogate.error
  par.set <- ParamHelpersParamSet(self$r.session, instance$search_space)
  learner <- vals$surrogate.learner

  init.size <- vals$initial.design.size %??% length(par.set$pars) * 4

  if (init.size != 0 && instance$archive$n_evals != 0) {
    warning("Both 'initial.design.size' and number of instance's previously evaluated points are nonzero, so the initial may be larger than you expect.")
  }
  if (init.size == 0 && instance$archive$n_evals == 0) {
    stop("Initial design must not be 0. Either use an Optimization / Tuning Instance with present evaluations, or set 'initial.design.size' > 0.")
  }

  if (vals$propose.points %??% 1 > 1 &&
      vals$multipoint.method %??% "cb" == "cb" &&
      vals$infill.crit %??% "CB" != "CB") {
    stop("for multipoint.method 'cb', infill.crit must be 'CB'.")
  }

  # don't eval_batch if the instance is already terminated. Will need to check this again *after* the initial design
  if (init.size > 0 && !instance$terminator$is_terminated(instance$archive)) {
    ## TODO: This is in contradiction to what mlrMBO usually does. depends on https://github.com/mlr-org/paradox/issues/307
    instance$eval_batch(generate_design_random(instance$search_space, init.size)$data)
  }
  # so *in principle* we could stop here if the budget is exhausted. However, we
  # do need to construct the `opt.state` so `assign_result` can do its thing.
  # Therefore we still need to get to the `constructMBOControl` part.
  #
  # The following therefore tells the encall() further down whether to generate a proposal.
  still.needs.proposition <- !instance$terminator$is_terminated(instance$archive)

  design <- as.data.frame(instance$archive$data()[, instance$objective$codomain$ids(), with = FALSE])
  colnames(design) <- sprintf(".PERFORMANCE.%s", seq_len(self$n.objectives))
  design <- cbind(do.call(rbind.data.frame, instance$archive$data()$x_domain), design)
  minimize <- unname(map_lgl(instance$objective$codomain$tags, function(x) "minimize" %in% x))  # important to unname, mlrMBO fails otherwise

  proposition <- encall(self$r.session, vals, n.objectives, still.needs.proposition, par.set, minimize, design, learner, on.surrogate.error, expr = {

    control <- constructMBOControl(vals = vals, n.objectives = n.objectives, on.surrogate.error = on.surrogate.error)

    # that's right, we save the opt.state inside the background R session. It contains lots of stuff, none of which we need
    # in the main R session.
    persistent$opt.state <- mlrMBO::initSMBO(control = control, par.set = par.set, minimize = minimize, design = design, learner = if (!is.null(learner)) makeCapsuledLearner(learner))

    proposition <- NULL
    if (still.needs.proposition) {
      proposition <- mlrMBO::proposePoints(persistent$opt.state)
      proposition$prop.points <- repairParamDF(ParamHelpers::getParamSet(persistent$opt.state$opt.problem$fun), proposition$prop.points)
    }

    # also save the design inside the background R session, this saves us from having to send data over the process gap
    persistent$design <- proposition$prop.points

    proposition
  })
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
    evals <- instance$eval_batch(as.data.table(cbind(design, extra)))

    if (instance$terminator$is_terminated(instance$archive)) {
      return(invisible(NULL))
    }
    perfs <- as.data.frame(evals)[seq_len(n.objectives)]

    proposition <- encall(self$r.session, perfs, expr = {
      # using the saved design & opt.state here; save the new opt.state righ taway
      persistent$opt.state <- mlrMBO::updateSMBO(opt.state = persistent$opt.state, x = persistent$design, y = as.list(as.data.frame(t(perfs))))
      proposition <- mlrMBO::proposePoints(persistent$opt.state)
      proposition$prop.points <- repairParamDF(ParamHelpers::getParamSet(persistent$opt.state$opt.problem$fun), proposition$prop.points)
      persistent$design <- proposition$prop.points  # save the design again
      proposition
    })
  }
}

assignResult <- function(instance) {
  best <- encall(self$r.session, expr = {
    resobj <- mlrMBO::finalizeSMBO(persistent$opt.state)
    if (!is.null(resobj$pareto.inds)) resobj$pareto.inds else resobj$best.ind
  })
  xdt <- instance$archive$data()[best, instance$search_space$ids(), with = FALSE]
  ydt <- instance$archive$data()[best, instance$objective$codomain$ids(), with = FALSE]
  if (inherits(instance, "OptimInstanceMultiCrit")) {
    instance$assign_result(xdt, ydt)
  } else {
    instance$assign_result(xdt, unlist(ydt))
  }
  invisible(self)
}

# create mlrMBO control object from parameter values
# used from within background R session
constructMBOControl <- function(vals, n.objectives, on.surrogate.error) {
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
    n.objectives = n.objectives, on.surrogate.error = on.surrogate.error,
    y.name = sprintf(".PERFORMANCE.%s", seq_len(n.objectives)),
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

# call ParamHelpers::repairPoint on each data frame row
# used from within background R session
repairParamDF <- function(par.set, df) {
  do.call(rbind.data.frame, lapply(ParamHelpers::dfRowsToList(df, par.set), ParamHelpers::repairPoint, par.set = par.set))
}
