context("custom_learner")

test_that("makeMlr3Surrogate", {
  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))

  testSurrogate <- function(s) {
    ti <- TuningInstanceSingleCrit$new(tsk("pima"), lrn("classif.rpart", predict_type = "prob"), rsmp("cv"), msr("classif.auc"), ps, trm("evals", n_evals = 11))
    tuner <- TunerInterMBO$new()
    tuner$param_set$values$surrogate.learner <- s
    tuner$optimize(ti)
  }

  testSurrogate(makeMlr3Surrogate(TRUE, TRUE, FALSE))
  testSurrogate(makeMlr3Surrogate(TRUE, TRUE, TRUE))
  testSurrogate(makeMlr3Surrogate(TRUE, FALSE, TRUE))
  testSurrogate(makeMlr3Surrogate(FALSE, TRUE, TRUE))
  testSurrogate(makeMlr3Surrogate(FALSE, TRUE, FALSE))


})

test_that("custom surrogate", {

  custompo <- R6::R6Class("mypo", inherit = mlr3pipelines::PipeOpTaskPreproc,
    private = list(
      .train_task  = function(task) {
        target <- task$data(cols = task$target_names)
        target[, `:=`(task$target_names, -(task$data(cols = "cp")[[1]] - 0.5) ^ 2)]
        task$cbind(target)
      },
      predict_task = function(task) {
        task
      }
    )
  )$new(id = "custom")

  cus <- as_learner(mlr3pipelines::`%>>%`(custompo, mlr3learners::LearnerRegrRanger$new()))

  ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))

  ti <- TuningInstanceSingleCrit$new(tsk("pima"), lrn("classif.rpart", predict_type = "prob"), rsmp("cv"), msr("classif.auc"), ps, trm("evals", n_evals = 20))
  tuner <- TunerInterMBO$new()
  tuner$param_set$values$surrogate.learner <- cus
  tuner$param_set$values$infill.crit <- "MeanResponse"
  tuner$param_set$values$final.method <- "last.proposed"
  tuner$optimize(ti)

  expect_lte(abs(0.5 - ti$result_x_domain$cp), .01)


})
