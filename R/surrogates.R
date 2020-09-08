#' @title Create Surrogate Learner
#'
#' @description
#' Creates the default mlrMBO surrogate learners as an [`mlr3::Learner`].
#'
#' This imitates the behaviour of mlrCPO when no `learner` argument is given to `mbo()` / `initSMBO()`.
#'
#' @param is.numeric (`logical(1)`)\cr
#'   Whether only numeric parameters are present. If so, a `LearnerRegrKM` (\pkg{DiceKriging} package)
#'   is constructed. Otherwise a `LearnerRegrRanger` (random forest from the \pkg{ranger} package) is constructed.
#'   Default is `TRUE`.
#' @param is.noisy (`logical(1)`)\cr
#'   Whether to use nugget estimation. Only considered when `is.numeric` is `TRUE`. Default is `TRUE`.
#' @param has.dependencies (`logical(1)`)\cr
#'   Whether to anticipate missing values in the surrogate model design. This adds out-of-range imputation to the model.
#'   If more elaborate imputation is desired, it may be desirable to set this to `FALSE` and instead perform custom imputation
#'   using \pkg{mlr3pipelines}.
#'   Default is `!numeric`.
#'
#' @examples
#' # DiceKriging Learner:
#' makeMlr3Surrogate()
#'
#' # mlr3pipelines Graph: imputation %>>% 'ranger' (randomForest):
#' makeMlr3Surrogate(is.numeric = FALSE)
#'
#' # just the 'ranger' Learner:
#' makeMlr3Surrogate(is.numeric = FALSE, has.dependencies = FALSE)
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
