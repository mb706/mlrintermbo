lg = lgr::get_logger("mlr3")
old_threshold = lg$threshold
lg$set_threshold("warn")

lg_bb = lgr::get_logger("bbotk")
old_threshold_bb = lg_bb$threshold
lg_bb$set_threshold("warn")


options(warnPartialMatchArgs = TRUE)
options(warnPartialMatchAttr = TRUE)
options(warnPartialMatchDollar = TRUE)

library("mlr3")
library("mlr3learners")
library("mlr3tuning")
library("bbotk")
