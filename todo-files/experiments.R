


devtools::document()
devtools::load_all()
library("mlr3")
library("mlr3tuning")
library("paradox")
library("data.table")


ps <- ParamSet$new(list(ParamDbl$new("cp", lower = 0, upper = 1), ParamInt$new("minsplit", lower = 1, upper = 20)))
ti <- TuningInstance$new(tsk("pima"), lrn("classif.rpart", predict_type = "prob"), rsmp("cv"), msrs(c("classif.auc")), ps, term("evals", n_evals = 11))

tuner = TunerInterMBO$new()

debug(tuner$.__enclos_env__$private$tune_internal)

tuner$tune(ti)





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
