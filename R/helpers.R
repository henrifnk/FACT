#' @title Create a generic prediction function
#' @description Create the algorithms prediction function.
#' @importFrom stats predict
#' @param model any \cr
#'  An arbitrary trained clustering algorithm.
#' @param task `character(1)` \cr
#'  Should be clustering in this case. To be extended...
#' @param predict.fun `function` \cr
#'  The function to assign newdata. Only needed if `model` is not a model from
#'  `mlr3cluster`. The first argument of `predict.fun` has to be the model, the
#'  second the `newdata`:
#'   ```
#'   function(model, newdata)
#'   ```
#'   To be extended for more methods.
#' @param type `character(1)` \cr
#'  For soft label predictions, `type="prob"`. For hard label predictions,
#'  `type="partition"`. Consult the documentation or definition of the
#'  clustering algorithm you use to find which type options you have.
#' @return A unified cluster assignment function for either hard or soft labels.
create_predict_fun <- function(model, task, predict.fun = NULL, type = NULL) {
  UseMethod("create_predict_fun")
}

# utilities---------------------------------------------------------------------
#' @describeIn create_predict_fun Create a predict function for algorithms from
#' `mlr3cluster`
create_predict_fun.Learner <- function(model, task, predict.fun = NULL, type = NULL) {
  if (!requireNamespace("mlr3")) {
    "Please install the mlr3 package."
  }
  if (task == "clustering") {
    function(newdata) {
      if(model$predict_type == "prob") {
        return(data.table(model$predict_newdata(newdata)$prob))
      }
      data.table(predict(model, newdata = newdata))
    }
  } else {
    stop(sprintf("Task type '%s' not supported", task))
  }
}
