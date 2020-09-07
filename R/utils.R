
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
encall <- function(session, expr, ..., .detach.env = topenv(parent.frame())) {
  args <- structure(list(...), names = map_chr(substitute(list(...)), as.character)[-1])

  # If the 'args' contain any functions, we detachEnv() them
  # TODO: this may be unnecessary since we have `registerEncallFunction()` now.
  args <- map(args, function(x) {
    if (is.function(x) && identical(topenv(environment(x)), .detach.env)) {
      detachEnv(environment(x), basis = .GlobalEnv)
    }
    x
  })
  funexp <- call("function", as.pairlist(map(args, function(x) substitute())), substitute(expr))
  fun <- eval(funexp, envir = .GlobalEnv)

  if (session$get_state() == "busy") {
    # The following we do because initSession() loads mlr, which may take a while, so it is run asynchronously.
    # Therefore here we need to wait for the background process. We are being generous with timing, because it
    # *could* just take long, but it could also mean the session is hung for some reason so we message after a while.
    if (session$poll_process(10000) == "timeout") {  # message after 10 seconds
      message("mlrintermbo is waiting for background R session which appears to take longer for startup than anticipated. Maybe it is hanging?")
      while(session$poll_process(60000) == "timeout") message("... still waiting ...")
      message("mlrintermbo background R process startup finally done")
    }
    session$read()  # need this to reset session's internal readiness indicator after startup.
  }

  output <- session$run_with_output(args = list(fun = fun, args = args, seed = runif(1) * 2^31), function(fun, args, seed) {
    set.seed(seed)
    captureSpecials(do.call(fun, args))
  })

  output.text <- output$stdout
  output.pieces <- gregexpr("<<!(WARNING|ERROR)!>>.*?<</!(WARNING|ERROR)!>>\n", output.text)[[1]]
  output.piece.indices <- c(1, if (output.pieces[[1]] != -1) rbind(output.pieces, output.pieces + attr(output.pieces, "match.length")), nchar(output.text) + 1)
  lg <- lgr::get_logger("bbotk")
  for (i in seq_len(length(output.piece.indices) - 1)) {
    piece <- substr(output.text, output.piece.indices[[i]], output.piece.indices[[i + 1]] - 1)
    if (grepl("^<<!WARNING!>>.*<</!WARNING!>>\n$", piece)) {
      warning(substr(piece, 14, nchar(piece) - 15), call. = FALSE)
      next
    }
    if (grepl("^<<!ERROR!>>.*<</!ERROR!>>\n$", piece)) {
      stop(substr(piece, 12, nchar(piece) - 13), call. = FALSE)
      next
    }
    lg$debug(piece)
  }

  if (!is.null(output$error)) {
    # this should not be reached if there is an `<<!ERROR!>>` somewhere in the output.
    stop(output$error$message)
  }
  output$result
}

encall.function.registry <- list()
registerEncallFunction <- function(f) {
  encall.function.registry <<- c(encall.function.registry, structure(list(detachEnv(f, basis = .GlobalEnv)), names = as.character(substitute(f))))
}

# make a callr-session ready for `encall`
initSession <- function(session) {
  session$call(args = list(encall.function.registry), function(encall.function.registry) {
    options(warn = 1)
    assign(envir = .GlobalEnv, "captureSpecials", function(expr) {
      sink(stdout(), type = "message")
      withCallingHandlers(
        expr,
        error = function(e) {
          suppressWarnings(sink())
          cat("ERROR TRACEBACK:\n")
          traceback(4)
          # do some sprintf'ing because we don't want to have a string in the code that matches '<<!....!>>'.
          cat(sprintf("<<!%s!>>%s<</!%s!>>\n", "ERROR", conditionMessage(e), "ERROR"))
        },
        warning = function(e) {
          suppressWarnings(sink())
          cat(sprintf("<<!%s!>>%s<</!%s!>>\n", "WARNING", conditionMessage(e), "WARNING"))
          invokeRestart("muffleWarning")
        }
      )
    })

    for (n in names(encall.function.registry)) {
      assign(n, envir = .GlobalEnv, encall.function.registry[[n]])
    }
    suppressMessages(attachNamespace("mlr"))  # this is necessary because mlr does things in .onAttach that should be done in .onLoad
    NULL
  })
  invisible(session)
}
