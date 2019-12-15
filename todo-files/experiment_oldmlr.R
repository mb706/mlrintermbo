
library("mlr")
library("mlrMBO")
library("mlrCPO")
library("ParamHelpers")

ps <- pSS(cp: numeric [0, 1], minsplit: integer [1, 20])

des <- generateDesign(n = 7, par.set = ps)
saveRDS(des, "des.rds")

ctrl <- makeMBOControl(n.objectives = 1, y.name = sprintf(".PERFORMANCE.%s", seq_len(1)), propose.points = 3)
ctrl <- setMBOControlInfill(ctrl, makeMBOInfillCritCB())

perf <- readRDS("perf.rds")

colnames(perf) <- sprintf(".PERFORMANCE.%s", seq_len(1))


des <- cbind(des, perf)
optstate <- initSMBO(par.set = ps, design = des, control = ctrl, minimize = c(FALSE))

proposition <- proposePoints(optstate)

saveRDS(proposition$prop.points, "des.rds")

proposition$prop.points <- NULL

        proposition$crit.components <- NULL
        proposition$prop.type <- NULL

proposition

as.data.frame(proposition, stringsAsFactors = FALSE)


perf <- readRDS("perf.rds")


updateSMBO(opt.state = optstate, x = readRDS("des.rds"), y = as.list(as.data.frame(t(perf))))

options(width=180)
as.data.frame(optstate$opt.path)


get

optstatem

finalizeSMBO(optstate)$best.ind
