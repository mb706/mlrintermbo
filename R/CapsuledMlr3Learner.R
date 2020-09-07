#' @include utils.R

# create an mlr learner that encapsulates the mlr3-learner encing
makeCapsuledLearner <- function(encing) {
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
    properties = unlist(unname(property.translate[c(encing$learner_properties, encing$feature_types, encing$predict_types)])),
    name = encing$id, short.name = encing$id
  )
  lr$encing <- encing
  lr$fix.factors.prediction = FALSE
  mlr::setPredictType(lr, encing$predict_type)
}
registerEncallFunction(makeCapsuledLearner)

trainLearner.CapsuledMlr3Learner <- function(.learner, .task, .subset, .weights = NULL, ...) {
  data <- mlr::getTaskData(.task, .subset)
  data.task <- mlr3::TaskRegr$new(mlr::getTaskId(.task), data, mlr::getTaskTargetNames(.task))
  lclone <- .learner$encing$clone(deep = TRUE)
  lclone$predict_type <- mlr::getLearnerPredictType(.learner)
  lclone$train(data.task)
}
registerEncallFunction(trainLearner.CapsuledMlr3Learner)

predictLearner.CapsuledMlr3Learner <- function(.learner, .model, .newdata, ...) {
  pred <- .model$learner.model$predict_newdata(.newdata)
  if (.learner$predict.type == "se") {
    cbind(response = pred$response, se = pred$se)  # return 2-col numeric matrix
  } else {
    pred$response  # return numeric vector
  }
}
registerEncallFunction(predictLearner.CapsuledMlr3Learner)
