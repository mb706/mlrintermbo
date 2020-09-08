context("manual tests")

test_that("mbo default", {
  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))
  ti <- TuningInstanceSingleCrit$new(tsk("pima"), lrn("classif.rpart", predict_type = "prob"), rsmp("cv"), msr("classif.auc"), ps, trm("evals", n_evals = 11))

  tuner <- TunerInterMBO$new()

  tuner$optimize(ti)

  archivenames <- c("cp", "minsplit", "classif.auc", "resample_result", "timestamp", "batch_nr", "x_domain", "propose.time", "crit.vals", "errors.model")

  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))

  expect_data_table(ti$archive$data(), nrows = 11)
})

test_that("mbo different settings", {

  set.seed(2)
  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))
  archivenames <- c("cp", "minsplit", "classif.auc", "resample_result", "timestamp", "batch_nr", "x_domain", "propose.time", "errors.model")

  ll <- lrn("classif.rpart", predict_type = "prob")
  surr <- mlr3::LearnerRegrFeatureless$new()
  surr$predict_type = "se"
  eval_mbo <- function(params, multimsr = FALSE, n.objectives = 1, surrogate = surr) {
    ti <- (if (n.objectives == 1) TuningInstanceSingleCrit else TuningInstanceMultiCrit)$new(tsk("pima"),
      ll, rsmp("cv"), (if (n.objectives == 1) msr else msrs)(c("classif.auc", if (multimsr) "classif.tpr")),
      ps, trm("evals", n_evals = 11))
    tuner <- TunerInterMBO$new(n.objectives)
    params$surrogate.learner <- surrogate
    if (is.null(params$infill.opt)) params$infill.opt <- "focussearch"  # TODO: only here because of https://github.com/mlr-org/paradox/issues/265
    tuner$param_set$values <- params
    tuner$optimize(ti)
    ti
  }

  ti <- eval_mbo(list())
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.opt = "ea", infill.opt.ea.maxit = 10, infill.opt.ea.mu = 2, infill.opt.ea.sbx.eta = 13, infill.opt.ea.sbx.p = 0.7, infill.opt.ea.pm.eta = 11, infill.opt.ea.pm.p = 0.4, infill.opt.ea.lambda = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.interleave.random.points = 1, infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 12)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE, TRUE, FALSE, TRUE), c(8, 1, 1, 1, 1)))
  expect_equal(ti$archive$data()$batch_nr, rep(1:3, c(8, 2, 2)))

  ti <- eval_mbo(list(propose.points = 2, infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals", "multipoint.cb.lambdas"))
  expect_data_table(ti$archive$data(), nrows = 12)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive$data()$batch_nr, rep(1:3, c(8, 2, 2)))

  ti <- eval_mbo(list(propose.points = 2, multipoint.method = "cb", infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals", "multipoint.cb.lambdas"))
  expect_data_table(ti$archive$data(), nrows = 12)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive$data()$batch_nr, rep(1:3, c(8, 2, 2)))

  # cmaes only works with numeric params, but we can append a trafo that rounds
  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamDbl$new("minsplit", lower = 1, upper = 20)))
  ps$trafo <- function(x, param_set) { x$minsplit <- round(x$minsplit) ; x }
  ti <- eval_mbo(list(propose.points = 2, infill.opt = "cmaes"))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals", "multipoint.cb.lambdas"))
  expect_data_table(ti$archive$data(), nrows = 12)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive$data()$batch_nr, rep(1:3, c(8, 2, 2)))

  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))
  ti <- eval_mbo(list(propose.points = 2, multipoint.method = "cl", infill.crit = "EI", infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 12)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive$data()$batch_nr, rep(1:3, c(8, 2, 2)))

  ti <- eval_mbo(list(propose.points = 2, multipoint.method = "cl", infill.crit = "EI", multipoint.cl.lie = max, infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 12)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive$data()$batch_nr, rep(1:3, c(8, 2, 2)))

  ti <- eval_mbo(list(propose.points = 2, multipoint.method = "moimbo", infill.crit = "EI"))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals.1", "crit.vals.2"))
  expect_data_table(ti$archive$data(), nrows = 12)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive$data()$batch_nr, rep(1:3, c(8, 2, 2)))

  ti <- eval_mbo(list(propose.points = 2, multipoint.method = "moimbo", infill.crit = "EI", multipoint.moimbo.objective = "mean.dist", multipoint.moimbo.dist = "nearest.neighbor", multipoint.moimbo.selection = "crowdingdist", multipoint.moimbo.maxit = 10, multipoint.moimbo.sbx.eta = 17, multipoint.moimbo.sbx.p = 0.8, multipoint.moimbo.pm.eta = 12, multipoint.moimbo.pm.p = 0.7))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals.1", "crit.vals.2"))
  expect_data_table(ti$archive$data(), nrows = 12)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive$data()$batch_nr, rep(1:3, c(8, 2, 2)))

  ti <- eval_mbo(list(infill.crit = "MeanResponse", infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "StandardError", infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "CB", infill.crit.cb.lambda = 4, infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "AEI", infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  set.seed(1)
  # need to explicitly turn on nugget in kriging model
  ti <- eval_mbo(list(infill.crit = "AEI", infill.crit.aei.use.nugget = TRUE, infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2), surrogate = NULL)
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "EQI", infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "EQI", infill.crit.eqi.beta = 0.6, infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "AdaCB", infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2))
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "DIB", multiobj.method = "dib", infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2), multimsr = TRUE, n.objectives = 2)
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "classif.tpr", "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "DIB", multiobj.method = "dib", infill.crit.sms.eps = 0.01, infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2), multimsr = TRUE, n.objectives = 2)
  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "classif.tpr", "crit.vals"))
  expect_data_table(ti$archive$data(), nrows = 11)
  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))


## TODO: breaks because of https://github.com/mlr-org/mlrMBO/issues/474
#  ti <- eval_mbo(list(infill.crit = "EI", multiobj.method = "parego", infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2), multimsr = TRUE, n.objectives = 2)
#  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "classif.auc", "classif.tpr", "propose.time", "crit.vals", "errors.model"))
#  expect_data_table(ti$archive$data(), nrows = 11)
#  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

## TODO: breaks because of https://github.com/mlr-org/mlrMBO/issues/474
#  ti <- eval_mbo(list(infill.crit = "EI", multiobj.method = "mspot", infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2), multimsr = TRUE, n.objectives = 2)
#  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "classif.auc", "classif.tpr", "propose.time", "crit.vals", "errors.model"))
#  expect_data_table(ti$archive$data(), nrows = 11)
#  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

## TODO: breaks because of https://github.com/mlr-org/mlrMBO/issues/474
#  ti <- eval_mbo(list(propose.points = 2, infill.crit = "EI", multiobj.method = "mspot", infill.opt = "nsga2", infill.opt.nsga2.popsize = 80, infill.opt.nsga2.generations = 30, infill.opt.nsga2.cprob = 0.6, infill.opt.nsga2.cdist = 4, infill.opt.nsga2.mprob = 0.3, infill.opt.nsga2.mdist = 11), multimsr = TRUE, n.objectives = 2)
#  expect_names(names(ti$archive$data()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model", "multipoint.cb.lambdas"))
#  expect_data_table(ti$archive$data(), nrows = 12)
#  expect_equal(is.na(ti$archive$data()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
#  expect_equal(ti$archive$data()$batch_nr, rep(1:3, c(8, 2, 2)))


})

