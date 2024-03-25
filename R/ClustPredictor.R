#' @title Clustering Predictor Object
#'
#' @description
#' A `ClustPredictor` object holds any unsupervised clustering algorithm
#' and the data to be used for analyzing the model. The interpretation methods
#' in the `FACT` package need the clustering algorithm to be wrapped in a
#' `ClustPredictor` object.
#'
#' @importFrom R6 R6Class
#' @importFrom iml Predictor
#' @import checkmate
#'
#' @details
#' A Cluster Predictor object is a container for the unsupervised prediction
#' model and the data.
#' This ensures that the clustering algorithm can be analyzed in a robust way.
#' The Model inherits from [iml::Predictor] Object and adjusts this Object to
#'  contain unsupervised Methods.
#' @examples
#' require(factoextra)
#' require(FuzzyDBScan)
#' multishapes <- as.data.frame(multishapes[, 1:2])
#' eps = c(0, 0.2)
#' pts = c(3, 15)
#' res <- FuzzyDBScan$new(multishapes, eps, pts)
#' res$plot("x", "y")
#' # create hard label predictor
#' predict_part = function(model, newdata) model$predict(new_data = newdata, cmatrix = FALSE)$cluster
#' ClustPredictor$new(res, as.data.frame(multishapes), y = res$clusters,
#'                        predict.function = predict_part, type = "partition")
#' # create soft label predictor
#' predict_prob = function(model, newdata) model$predict(new_data = newdata)
#' ClustPredictor$new(res, as.data.frame(multishapes), y = res$results,
#'                                predict.function = predict_prob, type = "prob")
#' @export
ClustPredictor <- R6Class("ClustPredictor",
                          inherit = Predictor,
                          public = list(

                            #' @description Create a ClustPredictor object
                            #' @param model any\cr
                            #'   The trained clustering algorithm. Recommended
                            #'   are models from `mlr3cluster`. For other
                            #'   clustering algorithms predict functions need to
                            #'   be specified.
                            #' @param data [data.frame]\cr
                            #'   The data to be used for analyzing the prediction model. Allowed column
                            #'   classes are: [numeric], [factor], [integer], [ordered] and [character]
                            #' @param predict.function [function]\cr
                            #'   The function to assign newdata. Only needed if
                            #'   `model` is not a model from `mlr3cluster`. The
                            #'   first argument of `predict.fun` has to be the
                            #'   model, the second the `newdata`:
                            #'   ```
                            #'   function(model, newdata)
                            #'   ```
                            #' @param y any\cr
                            #'   A [integer] vector representing the assigned
                            #'   clusters or a [data.frame] representing the
                            #'   soft labels per cluster assigned in columns.
                            #' @param type `character(1)`)\cr
                            #'   This argument is passed to the prediction
                            #'   function of the model. For soft label
                            #'   predictions, use `type="prob"`. For hard label
                            #'   predictions, use `type="partition"`. Consult
                            #'   the documentation or definition of the
                            #'   clustering algorithm you use to find which type
                            #'   options you have.
                            #' @param batch.size `numeric(1)`\cr
                            #' The maximum number of rows to be input the model for prediction at once.
                            #' Currently only respected for [SMART].
                            initialize = function(model = NULL, data = NULL, predict.function = NULL,
                                                  y = NULL, batch.size = 1000, type = NULL) {
                              assert_number(batch.size, lower = 1)
                              if(is.null(y)) stop("Provide a assignment vector
                                                  or an soft label data.frame for y!")
                              if (is.null(model) & is.null(predict.function)) {
                                stop("Provide a model, a predict.fun or both!")
                              }
                              if (is.null(data)) {
                                stop("Can't extract data from model, please provide via data=")
                              }
                              if (inherits(data, "data.table")) {
                                setDF(data)
                              }
                              self$data <- iml:::Data$new(data, y = y)
                              self$model <- model
                              self$type <- type
                              self$task <- "clustering"
                              self$type <- if(is.null(type)){
                                ifelse(ncol(data.table(y)) == 1L, "partition", "prob")
                              } else type
                              if(self$type == "prob") self$cnames <- colnames(y)
                              if(is.null(predict.function)){
                                self$prediction.function <- create_predict_fun(model, "clustering",
                                                                               predict.function)
                              } else{
                                self$prediction.function <- function(newdata){
                                  pred = do.call(predict.function, list(model, newdata = newdata))
                                  data.table(pred)
                                }
                              }
                              self$batch.size <- batch.size
                            },
                            #' @field type `character(1)`\cr
                            #'  Either partition for cluster assignments or prob
                            #'  for soft labels. Can be decided by chosen by the
                            #'  user when initializing the object. If `NULL`,
                            #'  it checks the the dimensions of `y`.
                            type = NULL,
                            #' @field cnames `character`\cr
                            #'  Is `NULL`, if hard labeling is used. If soft
                            #'  labels are used, column names of `y` are being
                            #'  transferred.
                            cnames = NULL
                          )
)
