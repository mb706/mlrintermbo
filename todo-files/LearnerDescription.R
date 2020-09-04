
#' @title Learner Description without loading mlr
#'
#' @description
#' constructs a learner in an R-session if `check` is TRUE, otherwise
#' just stores the parameters
#'
#' @param learnername name of the learner
#' @param params named hyperparameter value list
#' @param check whether to construct the learner
#' @return LearnerDescription object.
#' @export
LearnerDescription <- function(learnername, params, check = TRUE) {
  assertCharacter(learnername)
  assertList(params, names = "unique")
  object <- structure(list(learnername = learnername, params = params), class = "LearnerDescription")
  if (check) {
    GetLearnerFromDesc(object)
  } else {
    object
  }
}

#' @title Construct a Learner without loading mlr
#'
#' @description
#' constructs a learner in an R-session if the learner description was not constructed with 'check = TRUE'.
#'
#' @param learnerDescription a LearnerDescription() constructed object.
#' @return LearnerInnst object.
#' @export
GetLearnerFromDesc <- function(learnerDescription) {
  UseMethod("GetLearnerFromDesc")
}

GetLearnerFromDesc.LearnerDesc <- function(learnerDescription) {
  object <- learnerDescription
  object$object <- encall(function(ld) {
    suppressMessages(library("mlr"))  # this is necessary because dum-dum mlr does things in .onAttach that should be done in .onLoad
    mlr::makeLearner(ld$learnername, par.vals = ld$params)
  }, .args = list(learnerDescription))
  class(object) <- c("LearnerInst", "LearnerDesc")
  object
}

GetLearnerFromDesc.LearnerInst <- function(learnerDescription) {
  learnerDescription
}
