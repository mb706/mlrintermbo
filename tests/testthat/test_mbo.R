
context("TunerInterMBO")

test_that("mbo default", {
  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))
  ti <- TuningInstance$new(tsk("pima"), lrn("classif.rpart", predict_type = "prob"), rsmp("cv"), msrs(c("classif.auc")), ps, term("evals", n_evals = 11))

  tuner <- TunerInterMBO$new()

  tuner$tune(ti)

  names(ti)

  archivenames <- c("nr", "batch_nr", "resample_result", "task_id", "learner_id", "resampling_id", "iters", "params", "tune_x", "warnings", "errors")

  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))

  expect_data_table(ti$archive(), nrows = 11)
})

test_that("mbo different settings", {

  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))
  archivenames <- c("nr", "batch_nr", "resample_result", "task_id", "learner_id", "resampling_id", "iters", "params", "tune_x", "warnings", "errors")
  ll <- lrn("classif.rpart", predict_type = "prob")
  ll$param_set$.__enclos_env__$private$.params$maxdepth <- ParamDbl$new("maxdepth", lower = 1, upper = 30, default = 30)

  eval_mbo <- function(params, multimsr = FALSE, n.objectives = 1) {
    ti <- TuningInstance$new(tsk("pima"),
      ll, rsmp("cv"), msrs(c("classif.auc", if (multimsr) "classif.tpr")),
      ps, term("evals", n_evals = 11))
    tuner <- TunerInterMBO$new(n.objectives)
    tuner$param_set$values <- params
    tuner$tune(ti)
    ti
  }

  ti <- eval_mbo(list())
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 11)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.opt = "ea", infill.opt.ea.maxit = 200, infill.opt.ea.mu = 2, infill.opt.ea.sbx.eta = 13, infill.opt.ea.sbx.p = 0.7, infill.opt.ea.pm.eta = 11, infill.opt.ea.pm.p = 0.4, infill.opt.ea.lambda = 2))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 11)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))


  ti <- eval_mbo(list(infill.interleave.random.points = 1))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 12)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE, TRUE, FALSE, TRUE), c(8, 1, 1, 1, 1)))
  expect_equal(ti$archive()$batch_nr, rep(1:3, c(8, 2, 2)))

  ti <- eval_mbo(list(propose.points = 2))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model", "multipoint.cb.lambdas"))
  expect_data_table(ti$archive(), nrows = 12)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive()$batch_nr, rep(1:3, c(8, 2, 2)))

  ti <- eval_mbo(list(propose.points = 2, multipoint.method = "cb"))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model", "multipoint.cb.lambdas"))
  expect_data_table(ti$archive(), nrows = 12)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive()$batch_nr, rep(1:3, c(8, 2, 2)))


  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamDbl$new("maxdepth", lower = 1, upper = 20)))
  ti <- eval_mbo(list(propose.points = 2, infill.opt = "cmaes"))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model", "multipoint.cb.lambdas"))
  expect_data_table(ti$archive(), nrows = 12)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive()$batch_nr, rep(1:3, c(8, 2, 2)))

  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))
#  ti <- eval_mbo(list(propose.points = 2, infill.opt = "nsga2", infill.opt.nsga2.popsize = 80, infill.opt.nsga2.generations = 30, infill.opt.nsga2.cprob = 0.6, infill.opt.nsga2.cdist = 4, infill.opt.nsga2.mprob = 0.3, infill.opt.nsga2.mdist = 11))
#  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model", "multipoint.cb.lambdas"))
#  expect_data_table(ti$archive(), nrows = 12)
#  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
#  expect_equal(ti$archive()$batch_nr, rep(1:3, c(8, 2, 2)))

  ti <- eval_mbo(list(propose.points = 2, multipoint.method = "cl", infill.crit = "EI"))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 12)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive()$batch_nr, rep(1:3, c(8, 2, 2)))

  ti <- eval_mbo(list(propose.points = 2, multipoint.method = "cl", infill.crit = "EI", multipoint.cl.lie = max))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 12)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive()$batch_nr, rep(1:3, c(8, 2, 2)))


  ti <- eval_mbo(list(propose.points = 2, multipoint.method = "moimbo", infill.crit = "EI"))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals.1", "crit.vals.2","errors.model"))
  expect_data_table(ti$archive(), nrows = 12)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive()$batch_nr, rep(1:3, c(8, 2, 2)))

  ti <- eval_mbo(list(propose.points = 2, multipoint.method = "moimbo", infill.crit = "EI", multipoint.moimbo.objective = "mean.dist", multipoint.moimbo.dist = "nearest.neighbor", multipoint.moimbo.selection = "crowdingdist", multipoint.moimbo.maxit = 80, multipoint.moimbo.sbx.eta = 17, multipoint.moimbo.sbx.p = 0.8, multipoint.moimbo.pm.eta = 12, multipoint.moimbo.pm.p = 0.7))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals.1", "crit.vals.2","errors.model"))
  expect_data_table(ti$archive(), nrows = 12)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 4)))
  expect_equal(ti$archive()$batch_nr, rep(1:3, c(8, 2, 2)))


  ti <- eval_mbo(list(infill.crit = "MeanResponse"))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 11)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "StandardError"))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 11)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "CB", infill.crit.cb.lambda = 4))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 11)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "AEI"))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 11)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

## need to explicitly turn on nugget in kriging model
#  ti <- eval_mbo(list(infill.crit = "AEI", infill.crit.aei.use.nugget = TRUE))
#  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
#  expect_data_table(ti$archive(), nrows = 11)
#  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "EQI"))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 11)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "EQI", infill.crit.eqi.beta = 0.6))
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 11)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

## TODO: breaks because of https://github.com/mlr-org/mlrMBO/issues/473
#  ti <- eval_mbo(list(infill.crit = "AdaCB"))
#  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "propose.time", "crit.vals", "errors.model"))
#  expect_data_table(ti$archive(), nrows = 11)
#  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "DIB", multiobj.method = "dib"), multimsr = TRUE, n.objectives = 2)
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "classif.tpr", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 11)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

  ti <- eval_mbo(list(infill.crit = "DIB", multiobj.method = "dib", infill.crit.sms.eps = 0.01), multimsr = TRUE, n.objectives = 2)
  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "classif.tpr", "propose.time", "crit.vals", "errors.model"))
  expect_data_table(ti$archive(), nrows = 11)
  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))


## TODO: breaks because of https://github.com/mlr-org/mlrMBO/issues/474
#  ti <- eval_mbo(list(infill.crit = "EI", multiobj.method = "parego"), multimsr = TRUE, n.objectives = 2)
#  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "classif.tpr", "propose.time", "crit.vals", "errors.model"))
#  expect_data_table(ti$archive(), nrows = 11)
#  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))

## TODO: breaks because of https://github.com/mlr-org/mlrMBO/issues/474
#  ti <- eval_mbo(list(infill.crit = "EI", multiobj.method = "mspot"), multimsr = TRUE, n.objectives = 2)
#  expect_names(names(ti$archive()), permutation.of = c(archivenames, "classif.auc", "classif.tpr", "propose.time", "crit.vals", "errors.model"))
#  expect_data_table(ti$archive(), nrows = 11)
#  expect_equal(is.na(ti$archive()$propose.time), rep(c(TRUE, FALSE), c(8, 3)))



})
