
devtools::document()
devtools::load_all()
library("mlr3")
library("mlr3tuning")
library("bbotk")
library("paradox")
library("data.table")

options(error=recover)

Sys.setenv(NOT_CRAN = "true")
testthat::test_package("mlrintermbo")

devtools::load_all()

ti <- TuningInstanceMultiCrit$new(tsk("german_credit"), lrn("classif.rpart"), rsmp("cv"), msrs(c("classif.acc", "classif.tpr")), paradox::ParamSet$new(list(paradox::ParamDbl$new("cp", 0, 1))), trm("evals", n_evals = 5))

tun <- tnr("intermbo", n.objectives = 2, infill.crit = "DIB")

tun$optimize(ti)

devtools::test(filter = "fuzzing")
devtools::test(filter = "learner")
devtools::test(filter = "mbo")

# ------------------

ti <- TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("cv"), msr("classif.acc"), paradox::ParamSet$new(list(paradox::ParamDbl$new("cp", 0, 1))), trm("evals", n_evals = 5))

tun <- tnr("intermbo")

tun$optimize(ti)

des <- generate_design_lhs(ti$search_space, 3)

ev <- ti$eval_batch(des$data)

ti$archive$data()[, ti$objective$codomain$ids(), with = FALSE]

# ------------------

ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))
ti <- TuningInstanceSingleCrit$new(tsk("pima"), lrn("classif.rpart", predict_type = "prob"), rsmp("cv"), msr("classif.auc"), ps, trm("evals", n_evals = 11))

tuner <- tnr("intermbo")

# debug(tuner$.__enclos_env__$private$tune_internal)

tuner$optimize(ti)

ti$archive$data()

ti$result_y
ti$result_x_search_space

mlr3misc::unnest

mlr3misc::rbindlist2

# ------------------

library("mlr3")
library("mlr3tuning")
library("paradox")
library("data.table")
lrn("classif.rpart")$param_set

ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))

ti <- TuningInstance$new(tsk("pima"), lrn("classif.rpart", predict_type = "prob"), rsmp("cv"), msrs(c("classif.auc")), ps, term("evals", n_evals = 11))





perfs <- ti$eval_batch(as.data.table(readRDS("des.rds")))

saveRDS(perfs$perf, "perf.rds")



ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))
ti <- TuningInstance$new(tsk("pima"), lrn("classif.rpart", predict_type = "prob"), rsmp("cv"), msrs(c("classif.auc", "classif.logloss")), ps, term("evals", n_evals = 1))
ti$eval_batch(data.table(cp = 0, minsplit = 1))




ti$bmr$resample_result(1)



