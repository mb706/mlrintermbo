#' @include utils.R

# create an mlr learner that encapsulates the mlr3-learner encing
# used from within background R session
makeCapsuledLearner <- function(encing) {  # nocov start
  checkmate::assertClass(encing, "Learner")
  checkmate::assertClass(encing, "R6")

  property.translate <- list(
    missings = "missings",
    weights = "weights",
    importance = "featimp",
    selected_features = character(0),
    numeric = "numerics",
    factor = "factors",
    ordered = "ordered",
    se = "se"
  )

  lr <- mlr::makeRLearnerRegr("CapsuledMlr3Learner",
    package = encing$packages,
    par.set = ParamHelpers::makeParamSet(),
    properties = unlist(unname(property.translate[c(encing$properties, encing$feature_types, encing$predict_types)])),
    name = encing$id, short.name = encing$id
  )
  lr$encing <- encing
  lr$fix.factors.prediction = FALSE
  mlr::setPredictType(lr, encing$predict_type)
}  # nocov end


# used from within background R session
#' @exportS3Method mlr::trainLearner CapsuledMlr3Learner
trainLearner.CapsuledMlr3Learner <- function(.learner, .task, .subset, .weights = NULL, ...) {  # nocov start
  data <- mlr::getTaskData(.task, .subset)
  data.task <- mlr3::TaskRegr$new(mlr::getTaskId(.task), data, mlr::getTaskTargetNames(.task))
  lclone <- .learner$encing$clone(deep = TRUE)
  lclone$predict_type <- mlr::getLearnerPredictType(.learner)
  lclone$train(data.task)
}  # nocov end

# used from within background R session
#' @exportS3Method mlr::predictLearner CapsuledMlr3Learner
predictLearner.CapsuledMlr3Learner <- function(.learner, .model, .newdata, ...) {  # nocov start
  pred <- .model$learner.model$predict_newdata(.newdata)
  if (.learner$predict.type == "se") {
    cbind(response = pred$response, se = pred$se)  # return 2-col numeric matrix
  } else {
    pred$response  # return numeric vector
  }
}  # nocov end
