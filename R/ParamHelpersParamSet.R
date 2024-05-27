#' @include utils.R

# ParamHelpers::ParamSet from paradox::ParamSet
ParamHelpersParamSet <- function(session, paramset) {
  getRequires <- function(depname) {
    conds <- paramset$deps[get("id") == depname]
    cond.expressions <- mapply(function(...) conditionAsExpression(...), conds$cond, conds$on)
    Reduce(function(x, y) substitute(x && y, list(x = x, y = y)), cond.expressions)
  }
  data <- imap(paramset$ids(), function(pname, pindex) {
    switch(paramset$class[[pindex]],
      ParamLgl = list("makeLogicalParam", list(id = pname, requires = getRequires(pname))),
      ParamInt = list("makeIntegerParam", list(id = pname, lower = paramset$lower[[pindex]], upper = paramset$upper[[pindex]], requires = getRequires(pname))),
      ParamDbl = list("makeNumericParam", list(id = pname, lower = paramset$lower[[pindex]], upper = paramset$upper[[pindex]], requires = getRequires(pname))),
      ParamFct = list("makeDiscreteParam", list(id = pname, values = paramset$levels[[pname]], requires = getRequires(pname)))
    )
  })
  encall(session, data, expr = {
    # need to patch out https://github.com/mlr-org/ParamHelpers/pull/233
    ns <- asNamespace("ParamHelpers")
    patching <- get("determineReqVectorized", ns)
    if (isNamespace(environment(patching))) {
      p.env <- new.env(parent = environment(patching))
      p.env$deparse <- function(expr, width.cutoff = 500, ...) base::deparse(expr = expr, width.cutoff = width.cutoff, ...)
      environment(patching) <- p.env
      suppressWarnings({ get("unlockBinding", mode = "function")("determineReqVectorized", ns) ; assign("determineReqVectorized", patching, ns) ; lockBinding("determineReqVectorized", ns) })
    }
    ParamHelpers::makeParamSet(params = lapply(data, function(pcon) {
      do.call(get(pcon[[1]], getNamespace("ParamHelpers"), mode = "function"), pcon[[2]], quote = TRUE)
    }))
  })
}

# translate a paradox::Condition object into an expression
# condition: the Condition object
# on: the variable the condition depends on
conditionAsExpression <- function(condition, on) {
  UseMethod("conditionAsExpression")
}

conditionAsExpression.CondAnyOf <- function(condition, on) {
  # the single '&' is taken from paradox::CondAnyOf$test, which may be a bug there.
  substitute(!is.na(on) & on %in% rhs, list(on = as.symbol(on), rhs = condition$rhs))
}

conditionAsExpression.CondEqual <- function(condition, on) {
  # the single '&' is taken from paradox::CondEqual$test, which may be a bug there.
  substitute(!is.na(on) & on == rhs, list(on = as.symbol(on), rhs = condition$rhs))
}
