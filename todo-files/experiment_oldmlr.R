
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



# ---


obj.fun <- smoof::makeSingleObjectiveFunction(
  fn = function(x) checkmate::assertIntegerish(x$ypar),
  par.set = mlrCPO::pSS(xpar: numeric[0, 10], ypar: integer[0, 10]),
  has.simple.signature = FALSE)
ctrl <- makeMBOControl()
# ctrl <- setMBOControlMultiPoint(ctrl, method = "moimbo")
ctrl <- setMBOControlInfill(ctrl, makeMBOInfillCritAdaCB(), opt = "nsga2")
mbo(obj.fun, control = ctrl)


obj.fun <- smoof::makeMultiObjectiveFunction(
  fn = function(...) unlist(list(...)),
  par.set = mlrCPO::pSS(xpar: numeric[0, 10], ypar: integer[0, 10]),
  has.simple.signature = FALSE)
ctrl <- makeMBOControl(n.objectives = 2)
# ctrl <- setMBOControlMultiPoint(ctrl, method = "moimbo")
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritDIB(), opt = "nsga2")
ctrl <- setMBOControlMultiObj(ctrl, method = "dib")
mbo(obj.fun, control = ctrl)




ps = makeParamSet(
  makeNumericParam("q", lower = -1, upper = 2),
  makeIntegerParam("v", lower = -2, upper = 3)
)
des = generateDesign(n = 7, par.set = ps)
des$y_1 = c(1.20, 0.97, 0.91, 3.15, 0.58, 1.12, 0.50)
des$y_2 = c(1.20, 0.97, 0.91, 3.15, 0.58, 1.12, 0.50)
ctrl = makeMBOControl(n.objectives = 2)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(), opt = "nsga2")
ctrl = setMBOControlMultiObj(ctrl, method = "mspot")
opt.state = initSMBO(par.set = ps, design = des, control = ctrl, minimize = c(TRUE, TRUE), noisy = FALSE)
proposePoints(opt.state)
x = data.frame(q = 1.7, v = 1)
updateSMBO(opt.state, x = x, y = c(2.19, 2.19))


ps = makeParamSet(
  makeNumericParam("q", lower = -1, upper = 2),
  makeIntegerParam("v", lower = -2, upper = 3)
)
des = generateDesign(n = 7, par.set = ps)
des$y = c(1.20, 0.97, 0.91, 3.15, 0.58, 1.12, 0.50)
ctrl = makeMBOControl()
ctrl = setMBOControlInfill(ctrl, opt = "cmaes")
opt.state = initSMBO(par.set = ps, design = des, control = ctrl, minimize = TRUE, noisy = FALSE)
proposePoints(opt.state)
x = data.frame(q = 1.7, v = 1)
updateSMBO(opt.state, x = x, y = c(2.19, 2.19))

ps = makeParamSet(
  makeNumericParam("q", lower = -1, upper = 2),
  makeIntegerParam("v", lower = -2, upper = 3)
)
des = generateDesign(n = 7, par.set = ps)
des$y = c(1.20, 0.97, 0.91, 3.15, 0.58, 1.12, 0.50)
ctrl = makeMBOControl(propose.points = 2)
ctrl = setMBOControlMultiPoint(ctrl, method = "moimbo")
opt.state = initSMBO(par.set = ps, design = des, control = ctrl, minimize = TRUE, noisy = FALSE)
proposition <- proposePoints(opt.state)

proposition$prop.points
