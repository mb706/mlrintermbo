context("fuzzing")

test_that("mbo with autotuner", {

  ll <- lrn("classif.rpart", predict_type = "prob")
  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamDbl$new("minsplit", lower = 1, upper = 20), ParamDbl$new("minbucket", lower = 1, upper = 20)))
  ps$trafo <- function(x, param_set) {
    x$minsplit <- round(x$minsplit)
    x$minbucket <- round(x$minbucket)
    x
  }

  surr <- mlr3::LearnerRegrFeatureless$new()
  surr$predict_type = "se"

  at <- AutoTuner$new(ll, rsmp("holdout"), msr("classif.auc"), ps, trm("evals", n_evals = 11), tnr("intermbo", surrogate.learner = surr))

  rres <- mlr3::resample(tsk("pima"), at, rsmp("cv", folds = 5))

  expect_number(rres$aggregate())

})

test_that("fuzzing intermbo", {

  surr <- mlr3::LearnerRegrFeatureless$new()
  surr$predict_type = "se"


  tups <- tnr("intermbo")$param_set$clone(deep = TRUE)
  psnew <- ParamSet$new(discard(tups$params, function(x) "ParamUty" %in% class(x)))

  for (row in seq_len(nrow(tups$deps))) {
    if (tups$deps[row, id] %in% psnew$ids()) {
      psnew$add_dep(tups$deps[row, id], tups$deps[row, on], tups$deps$cond[[row]])
    }
  }

  for (pari in which(psnew$is_number)) {
    par <- psnew$params[[pari]]
    if (par$is_bounded) next
    if (is.finite(par$lower) && is.numeric(par$default) && is.finite(par$default)) {
      if (par$default > par$lower) {
        par$upper = (par$lower - par$default) + par$default * 2
      } else {
        par$upper = par$lower + 2
      }
    } else {
      par$lower = 0
      par$upper = 1
    }
  }
  psnew$params$initial.design.size$lower = 1
  psnew$params$initial.design.size$upper = 2

  ll <- lrn("classif.rpart", predict_type = "prob")
  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamDbl$new("minsplit", lower = 1, upper = 20), ParamDbl$new("minbucket", lower = 1, upper = 20)))
  ps$trafo <- function(x, param_set) {
    x$minsplit <- round(x$minsplit)
    x$minbucket <- round(x$minbucket)
    x
  }

  tuner <- TunerInterMBO$new(on.surrogate.error = "quiet")

  set.seed(1)
  for (setting in seq_len(20)) {
    repeat {
      setting <- generate_design_random(psnew, 1)$transpose()[[1]]
      ##> Multi-point proposal using constant liar needs the infill criterion 'ei' or 'aei', but you used '___'!
      if (setting$multipoint.method == "cl" && !setting$infill.crit %in% c("EI", "AEI", "CB")) next

      ##> for multipoint.method 'cb', infill.crit must be 'CB'.
      if (setting$multipoint.method == "cb" && setting$infill.crit != "CB") next

      # this only works with original learner
      if (setting$infill.crit == "AEI" && isTRUE(setting$infill.crit.aei.use.nugget)) next

      break
    }
    setting$surrogate.learner <- surr

    ti <- TuningInstanceSingleCrit$new(tsk("pima"), ll, rsmp("holdout"), msr("classif.auc"), ps, trm("evals", n_evals = 3))
    tuner$param_set$values <- setting
    suppressWarnings(tuner$optimize(ti))
  }


  psnew$params$initial.design.size$lower = 10
  psnew$params$initial.design.size$upper = 12

  tuner <- TunerInterMBO$new(on.surrogate.error = "stop")
  set.seed(2)
  for (setting in seq_len(20)) {

    repeat {
      setting <- generate_design_random(psnew, 1)$transpose()[[1]]
      ##> Multi-point proposal using constant liar needs the infill criterion 'ei' or 'aei', but you used '___'!
      if (setting$multipoint.method == "cl" && !setting$infill.crit %in% c("EI", "AEI", "CB")) next

      ##> for multipoint.method 'cb', infill.crit must be 'CB'.
      if (setting$multipoint.method == "cb" && setting$infill.crit != "CB") next

      # this only works with original learner
      if (setting$infill.crit == "AEI" && isTRUE(setting$infill.crit.aei.use.nugget)) next

      break
    }
    setting$surrogate.learner <- surr

    ti <- TuningInstanceSingleCrit$new(tsk("pima"), ll, rsmp("holdout"), msr("classif.auc"), ps, trm("evals", n_evals = 3))
    tuner$param_set$values <- setting
    suppressWarnings(tuner$optimize(ti))
  }

})