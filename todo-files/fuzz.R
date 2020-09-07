lg = lgr::get_logger("mlr3")
lg$set_threshold("warn")

lg_bb = lgr::get_logger("bbotk")
old_threshold_bb = lg_bb$threshold
lg_bb$set_threshold("warn")



options(warnPartialMatchArgs = TRUE)
options(warnPartialMatchAttr = TRUE)
options(warnPartialMatchDollar = TRUE)

options(warning = 1)

id <- round(runif(1) * 10000000)

devtools::load_all()

library("mlr3tuning")
library("mlr3")
library("bbotk")


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

    tuner <- TunerInterMBO$new(on.surrogate.error = "warn")
  for (setting in generate_design_random(psnew, 10000)$transpose()) {

    ##> Multi-point proposal using constant liar needs the infill criterion 'ei' or 'aei', but you used 'adacb'!
    if (setting$multipoint.method == "cl" && !setting$infill.crit %in% c("EI", "AEI", "CB")) next

    ##> for multipoint.method 'cb', infill.crit must be 'CB'.
    if (setting$multipoint.method == "cb" && setting$infill.crit != "CB") next

    ti <- TuningInstanceSingleCrit$new(tsk("pima"), ll, rsmp("holdout"), msr("classif.auc"), ps, trm("evals", n_evals = 3))
    tuner$param_set$values <- setting
    tryCatch(tuner$optimize(ti), error = function(e) saveRDS(setting, sprintf("fuzz_%s.rds", round(runif(1) * 10000000))))
  }


