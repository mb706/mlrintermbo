
library("mlr")
library("mlrMBO")
library("mlrCPO")
library("ParamHelpers")

ps <- pSS(q: numeric [-1, 2], v: integer [-2, 3])

des <- generateDesign(n = 7, par.set = ps)


ctrl <- makeMBOControl(n.objectives = 2, y.name = sprintf(".PERFORMANCE.%s", seq_len(2)), propose.points = 3)

des$.PERFORMANCE <- c(1.20, 0.97, 0.91, 3.15, 0.58, 1.12, 0.50)

optstate <- initSMBO(par.set = ps, design = des, control = ctrl, minimize = TRUE)

proposition <- proposePoints(optstate)

proposition$prop.points <- NULL

as.data.frame(proposition, stringsAsFactors = FALSE)
