context("custom_learner")

test_that("makeMlr3Surrogate", {
  skip_on_cran()

  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))

  objective <- ObjectiveRFun$new(function(xs) {
    list(y = (xs$cp - .5) ^ 2 + (assertInt(xs$minsplit) - 10) ^ 2)
  }, ps, ParamSet$new(list(ParamDbl$new("y", tags = "minimize"))))


  tuner <- OptimizerInterMBO$new()
  testSurrogate <- function(s) {
    ti <- OptimInstanceSingleCrit$new(objective, ps, trm("evals", n_evals = 11))
    values <- list(surrogate.learner = s, infill.opt.focussearch.points = 5, infill.opt.focussearch.maxit = 2)
    values$infill.opt <- "focussearch"  # TODO: only here because of https://github.com/mlr-org/paradox/issues/265
    tuner$param_set$values <- values
    set.seed(1)
    tuner$optimize(ti)
  }

  testSurrogate(makeMlr3Surrogate(FALSE, TRUE, FALSE))
  skip_on_cran()
  testSurrogate(makeMlr3Surrogate(TRUE, TRUE, FALSE))
  testSurrogate(makeMlr3Surrogate(TRUE, TRUE, TRUE))
  testSurrogate(makeMlr3Surrogate(TRUE, FALSE, TRUE))
  testSurrogate(makeMlr3Surrogate(FALSE, TRUE, TRUE))

})

test_that("custom surrogate", {

  # we create a custom surrogate model that consists of a pipeline that replaces the measured performance value with a (cp - 0.2)^2 performance value
  custompo <- R6::R6Class("mypo", inherit = mlr3pipelines::PipeOpTaskPreproc,
    private = list(
      .train_task  = function(task) {
        target <- task$data(cols = task$target_names)
        target[, `:=`(task$target_names, (task$data(cols = "cp")[[1]] - 0.2) ^ 2)]
        task$cbind(target)
      },
      predict_task = function(task) {
        task
      }
    )
  )$new(id = "custom")

  cus <- as_learner(mlr3pipelines::`%>>%`(custompo, mlr3learners::LearnerRegrRanger$new()))

  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))

  objective <- ObjectiveRFun$new(function(xs) {
    list(y = (xs$cp - .5) ^ 2 + (assertInt(xs$minsplit) - 10) ^ 2)
  }, ps, ParamSet$new(list(ParamDbl$new("y", tags = "minimize"))))
  tuner <- OptimizerInterMBO$new()
  tuner$param_set$values$surrogate.learner <- cus
  tuner$param_set$values$infill.crit <- "MeanResponse"
  tuner$param_set$values$final.method <- "last.proposed"

  ti <- OptimInstanceSingleCrit$new(objective, ps, trm("evals", n_evals = 10))
  tuner$optimize(ti)

  skip_on_cran()
  ti <- OptimInstanceSingleCrit$new(objective, ps, trm("evals", n_evals = 20))
  tuner$optimize(ti)
  expect_equal(ti$result_x_domain$cp, 0.2, tolerance = 0.01)

})
