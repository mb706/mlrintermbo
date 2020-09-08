#' @include utils.R

# ParamHelpers::ParamSet from paradox::ParamSet
ParamHelpersParamSet <- function(session, paramset) {
  getRequires <- function(depname) {
    conds <- paramset$deps[get("id") == depname]
    cond.expressions <- mapply(conditionAsExpression, conds$cond, conds$on)
    Reduce(function(x, y) substitute(x && y, list(x = x, y = y)), cond.expressions)
  }
  data <- imap(paramset$params, function(param, pname) {
    switch(param$class,
      ParamLgl = list("makeLogicalParam", list(id = pname, requires = getRequires(pname))),
      ParamInt = list("makeIntegerParam", list(id = pname, lower = param$lower, upper = param$upper, requires = getRequires(pname))),
      ParamDbl = list("makeNumericParam", list(id = pname, lower = param$lower, upper = param$upper, requires = getRequires(pname))),
      ParamFct = list("makeDiscreteParam", list(id = pname, values = param$levels, requires = getRequires(pname)))
    )
  })
  encall(session, data, expr = {
    ParamHelpers::makeParamSet(params = lapply(data, function(pcon) {
      do.call(get(pcon[[1]], getNamespace("ParamHelpers"), mode = "function"), pcon[[2]])
    }))
  })
}

# translate a paradox::Condition object into an expression
# condition: the Condition object
# on: the variable the condition depends on
conditionAsExpression <- function(condition, on) {
  UseMethod("ConditionAsExpression")
}

conditionAsExpression.CondAnyOf <- function(condition, on) {
  # the single '&' is taken from paradox::CondAnyOf$test, which may be a bug there.
  substitute(!is.na(on) & x %in% rhs, list(on = as.symbol(on), rhs = condition$rhs))
}

conditionAsExpression.CondEqual <- function(condition, on) {
  # the single '&' is taken from paradox::CondEqual$test, which may be a bug there.
  substitute(!is.na(on) & x == rhs, list(on = as.symbol(on), rhs = condition$rhs))
}
