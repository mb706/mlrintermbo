
# removes the environment from fun and potentially wraps it in a new environment
detachEnv <- function(fun, keep = character(0), basis = topenv(parent.frame())) {
  assertEnvironment(basis)
  assertCharacter(keep, any.missing = FALSE)
  assertFunction(fun)
  if (length(keep)) {
    keepvals <- mget(keep, parent.frame(), inherits = TRUE)
    basis <- new.env(parent = basis, size = length(keepvals))
    mapply(assign, names(keepvals), keepvals, MoreArgs = list(envir = basis))
  }
  environment(fun) <- basis
  fun
}

warnIfPHLoaded <- function() {
  if (environmentName(environment(as.environment("package:base")$.__S3MethodsTable__.$print.ParamSet)) == "ParamHelpers") {
    warning("ParamHelpers package was loaded for some reason.
Did you accidentally load something from ParamHelpers::, mlr::, or mlrMBO::?
It is strongly recommended that you restart this R session.")
  }
}

# evaluate the content of expr in another R session. The variables
# that are mentioned in '...' are given over. They must be symbols
# representing variables, not expressions.
#
# Automatically detachEnv()s the environment from wich encall is called.
#
# Example:
# x <- 1
# y <- 2
# encall({
#   z <- x + y
#   exp(z)
# }, x, y)
encall <- function(expr, ..., .detach.env = topenv(parent.frame())) {
  args <- structure(list(...), names = map_chr(substitute(list(...)), as.character)[-1])
  args <- map(args, function(x) {
    if (is.function(x) && identical(topenv(environment(x)), .detach.env)) {
      detachEnv(environment(x), basis = baseenv())
    }
    x
  })
  funexp <- call("function", map(args, function(x) substitute()), substitute(expr))
  x <- encapsulate("callr", .f = eval(funexp, envir = baseenv()), .args = args)
  for (line in transpose_list(x$log)) {
    switch(as.character(line$class),
      output = catf("%s", line$msg),
      warning = warning(line$msg),
      error = stop(line$msg)
    )
  }
  x$result
}
