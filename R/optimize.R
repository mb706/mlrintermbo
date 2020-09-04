#' @include utils.R

optimize <- function(instance) {

  warnIfPHLoaded()

  if (instance$objective$codomain$length != self$n.objectives) {
    stopf("instance needs to have n.objectives (= %s) objectives", self$n.objectives)
  }

  vals <- self$param_set$values
  n.objectives <- self$n.objectives
  par.set <- ParamHelpersParamSet(instance$search_space)
  learner <- if (!is.null(vals$surrogate.learner)) GetLearnerFromDesc(vals$surrogate.learner)  # TODO

  init.size <- vals$initial.design.size %??% length(par.set$pars) * 4

  if (init.size != 0 && instance$archive$n_evals != 0) {
    warning("Both 'initial.design.size' and number of instance's previously evaluated points are nonzero, so the initial may be larger than you expect.")
  }

  if (init.size > 0) {
    instance$eval_batch(generate_design_lhs(instance$param_set, init.size)$data)
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

  evalres <- encall(constructMBOControl, repairParamDF, vals, n.objectives, still.needs.proposition, par.set, design, learner, expr = {
    suppressMessages(library("mlr"))  # this is necessary because mlr does things in .onAttach that should be done in .onLoad
    control <- constructMBOControl(vals = vals, n.objectives = n.objectives)
    opt.state <- mlrMBO::initSMBO(control = control, par.set = par.set, design = design, learner = learner)
    proposition <- NULL
    if (still.needs.proposition) {
      proposition <- mlrMBO::proposePoints(opt.state)
      proposition$prop.points <- repairParamDF(ParamHelpers::getParamSet(opt.state$opt.problem$fun), proposition$prop.points)
    }

    list(
      proposition = proposition,
      # the opt.state contains references to namespaces which we don't want to load in the main session; therefore we serialize
      opt.state = serialize(opt.state, NULL)
    )
  })

  repeat {
    opt.state <- self$opt.state <- evalres$opt.state
    proposition <- evalres$proposition
    if (is.null(proposition)) {
      # budget exhausted after initial design
      return(invisible(NULL))
    }

    design <- proposition$prop.points
    proposition$prop.points <- NULL

    # TODO: the following are overwritten because they appear to be broken in mlrMBO. maybe repair this
    proposition$crit.components <- NULL
    proposition$prop.type <- NULL
    extra <- as.data.frame(proposition, stringsAsFactors = FALSE)
    evals <- instance$eval_batch(as.data.table(cbind(design, extra)))

    if (instance$terminator$is_terminated(instance)) {
      return(invisible(NULL))
    }
    perfs <- as.data.frame(evals)[seq_len(n.objectives)]

    evalres <- encall(repairParamDF, opt.state, design, perfs, expr = {
      suppressMessages(library("mlr"))  # this is necessary because dum-dum mlr does things in .onAttach that should be done in .onLoad
      opt.state <- mlrMBO::updateSMBO(opt.state = unserialize(opt.state), x = design, y = as.list(as.data.frame(t(perfs))))
      proposition <- mlrMBO::proposePoints(opt.state)
      proposition$prop.points <- repairParamDF(ParamHelpers::getParamSet(opt.state$opt.problem$fun), proposition$prop.points)
      list(proposition = proposition, opt.state = serialize(opt.state, NULL))
    })
  }
}

assignResult <- function(instance) {
  opt.state <- self$opt.state

  final <- encall(opt.state, expr = {
    suppressMessages(library("mlr"))  # this is necessary because dum-dum mlr does things in .onAttach that should be done in .onLoad
    resobj <- mlrMBO::finalizeSMBO(unserialize(opt.state))
    resobj[intersect(names(resobj), c("pareto.set", "x", "y", "best.ind"))]
  })
  browser()
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

# call ParamHelpers::repairPoint on each data frame row
repairParamDF <- function(par.set, df) {
  do.call(rbind.data.frame, lapply(ParamHelpers::dfRowsToList(df, par.set), ParamHelpers::repairPoint, par.set = par.set))
}
