
#' @title Create Surrogate Learner
#'
#' @description Creates the default mlrMBO surrogate learner as an [`mlr3::Learner`].
#'
#' @export
makeMlr3Surrogate <- function(is.numeric = TRUE, is.noisy = TRUE, has.dependencies = !is.numeric) {
  if (is.numeric) {
    base <- mlr3learners::LearnerRegrKM$new()
    base$param_set$values <- list(covtype = "matern3_2", optim.method = "gen", jitter = 0)
    if (!is.noisy) {
      base$param_set$values$nugget.stability <- 1e-8
    } else {
      base$param_set$values$jitter <- 1e-12
      base$param_set$values$nugget.estim <- TRUE
    }
  } else {
    base <- mlr3learners::LearnerRegrRanger$new()
    base$param_set$values <- list(se.method = "jack", keep.inbag = TRUE)  # TODO: is this the richt correspondence?
  }
  if (has.dependencies) {
    base <- mlr3::as_learner(mlr3pipelines::`%>>%`(mlr3pipelines::po("imputeoor"), base))
  }
  base$predict_type = "se"
  base
}
