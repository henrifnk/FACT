#' @title `SMART` - Scoring Metric after Permutation
#'
#' @description
#' `SMART` estimates the importance of a feature to the clustering algorithm
#' by measuring changes in cluster assignments by scoring functions after
#' permuting selected feature. Cluster-specific `SMART` indicates the importance
#' of specific clusters versus the remaining ones, measured by a binary scoring
#' metric. Global `SMART` assigns importance scores across all clusters, measured
#' by a multi-class scoring metric. Currently, `SMART` can only be used for hard
#' label predictors.
#'
#' @details Let \eqn{M \in \mathbb{N}_0^{k \times k}} denote the multi-cluster
#' confusion matrix and \eqn{M_c \in \mathbb{N}_0^{2 \times 2}} the binary
#' confusion matrix for cluster c versus the remaining clusters. `SMART` for
#' feature set S corresponds to:
#' \deqn{
#' \text{Multi-cluster scoring:} \quad \text{SMART}(X, \tilde{X}_S) = h_{\text{multi}}(M) \\
#' \text{Binary scoring:} \quad \text{SMART}(X, \tilde{X}_S) = \text{AVE}(h_{\text{binary}}(M_1), \dots, h_{\text{binary}}(M_k))
#' }
#' where \eqn{\text{AVE}} averages a vector of binary scores, e.g., via micro or
#' macro averaging.
#' In order to reduce variance in the estimate from shuffling the data, one can
#' shuffle t times and evaluate the distribution of scores. Let \eqn{\tilde{X}_S^{(t)}}
#' denote the t-th shuffling iteration for feature set S. The `SMART` point
#' estimate is given by: \cr
#' \deqn{
#' \overline{\text{SMART}}(X, \tilde{X}_S) = \psi\left(\text{SMART}(X, \tilde{X}_S^{(1)}),
#'  \dots, \text{SMART}(X, \tilde{X}_S^{(t)})\right)
#' }
#' where \eqn{\psi} extracts a sample statistic such as the mean or median or quantile.
#'
#' @importFrom data.table rbindlist data.table
#' @examples
#' \donttest{
#' # load data and packages
#' require(factoextra)
#' require(FuzzyDBScan)
#' multishapes = as.data.frame(multishapes[, 1:2])
#' # Set up an train FuzzyDBScan
#' eps = c(0, 0.2)
#' pts = c(3, 15)
#' res = FuzzyDBScan$new(multishapes, eps, pts)
#' res$plot("x", "y")
#' # create hard label predictor
#' predict_part = function(model, newdata) model$predict(new_data = newdata, cmatrix = FALSE)$cluster
#' predictor = ClustPredictor$new(res, as.data.frame(multishapes), y = res$clusters,
#'                                predict.function = predict_part, type = "partition")
#' # Run SMART globally
#' macro_f1 = SMART$new(predictor, n.repetitions = 50, metric = "f1", avg = "macro")
#' macro_f1 # print global SMART
#' macro_f1$plot(log = TRUE) # plot global SMART
#' # Run cluster specific SMART
#' classwise_f1 = SMART$new(predictor, n.repetitions = 50, metric = "f1")
#' macro_f1 # print regional SMART
#' macro_f1$plot(log = TRUE) # plot regional SMART
#' }
#' @seealso [iml::FeatureImp]
#' @export
SMART <- R6Class("SMART",
                 public = list(

                   #' @description Create a [SMART] object
                   #' @param predictor [ClustPredictor]\cr
                   #'   The object (created with `ClustPredictor$new()`) holding
                   #'   the cluster algorithm and the data.
                   #' @param features (`character or list`)\cr
                   #'   For which features do you want importance scores calculated. The default
                   #'   value of `NULL` implies all features. Use a named list of character vectors
                   #'   to define groups of features for which joint importance will be calculated.
                   #' @param metric `character(1)`\cr
                   #'   The binary similarity metric used. Defaults to `f1`,
                   #'   where F1 Score is used. Other possible binary scores are
                   #'   `"precision"`, `"recall"`, `"jaccard"`, `"folkes_mallows"`
                   #'   and `"accuracy"`.
                   #' @param avg (`character(1)` or `NULL`)\cr
                   #' Either `NULL`, `"micro"` or `"macro"`.
                   #' Defaults to `NULL` is calculating cluster-specific (binary)
                   #' metrics. `"micro"` summarizes binary scores to a global
                   #' score that treats each instance in the data set with equal
                   #' importance. `"macro"` summarizes binary scores to a global
                   #' score that treats each cluster with equal importance.
                   #' For unbalanced clusters, `"macro"` is more recommendable.
                   #' @param n.repetitions (`numeric(1)`)\cr
                   #' How often should the shuffling of the feature be repeated?
                   #' The higher the number of repetitions the more stable and
                   #' accurate the results become.
                   #' @return (data.frame)\cr
                   #' data.frame with the results of the feature importance computation.
                   #' One row per feature with the following columns:
                   #' For global scores:
                   #' - importance.05 (5% quantile of importance values from the repetitions)
                   #' - importance (median importance)
                   #' - importance.95 (95% quantile) and the permutation.error (median error
                   #' over all repetitions).
                   #' For cluster specific scores each column indicates for a different cluster.
                   initialize = function(predictor, features = NULL, metric = "f1",
                                         avg = NULL, n.repetitions = 5) {
                     assert_class(predictor, "ClustPredictor")
                     assert_set_equal(predictor$type, "partition")
                     assert_choice(avg, c("micro", "macro"), null.ok = TRUE)
                     assert_choice(metric, c("recall", "precision", "f1",
                                             "jaccard", "folkes_mallows", "accuracy"))
                     assert_number(n.repetitions)
                     self$avg <- avg
                     self$metric <- metric
                     self$predictor <- predictor
                     self$sampler <- predictor$data
                     self$data.sample <- predictor$data$get.xy()
                     self$n.repetitions <- n.repetitions
                     if (is.null(features)) {
                       features <- predictor$data$feature.names
                     }
                     if (!is.list(features)) {
                       features <- as.list(features)
                       names(features) <- unlist(features)
                     }
                     assert_subset(unique(unlist(features)), predictor$data$feature.names, empty.ok = FALSE)
                     self$features <- features
                     # suppressing package startup messages
                     self$results = suppressPackageStartupMessages(private$run(self$predictor$batch.size))
                   },

                   #' @description Print a `SMART` object
                   #' @return `character` \cr
                   #' Information about `predictor`, `data`, `metric`, and `avg`
                   #' and head of the `results`.
                   #'
                   #' @seealso [SMART]
                   print = function() {
                     cat("Interpretation method: ", class(self)[1], "\n")
                     cat("\nAnalysed predictor: \n")
                     self$predictor$print()
                     cat("\nAnalysed data:\n")
                     print(self$sampler)
                     cat("\nMetric:", self$metric, "\n")
                     if(is.null(self$avg)) cat("Classwise", self$metric,  "scores") else cat(self$metric, "scores sumarized by ", self$avg)
                     cat("\n\nHead of results:\n")
                     if (!is.null(self$results)) {
                       print(head(self$results))
                     }
                   },

                   #' @description plots the similarity score results of a `SMART`
                   #' object.
                   #'
                   #' @param log `logical(1)` \cr
                   #'   Indicator weather results should be logged. This can be
                   #'   useful to distinguish the importance if similarity scores
                   #'   are all close to 1.
                   #' @param single_cl `character(1)` \cr
                   #'   Only used for cluster-specific scores (`avg = NULL`).
                   #'   Should match one of the cluster names.
                   #'   In this case, importance scores for a single cluster are
                   #'   plotted.
                   #' @return ggplot2 plot object
                   #' @export
                   #'
                   #' @details
                   #' The plot shows the similarity per feature.
                   #' For global scores:
                   #' When `n.repetitions` in `SMART$new` was larger than 1, then we get
                   #' multiple similarity estimates per feature. The similarity are aggregated and
                   #' the plot shows the median similarity per feature (as dots) and also the
                   #' 90%-quantile, which helps to understand how much variance the computation has
                   #' per feature.
                   #' For cluster-specific scores:
                   #' Stacks the similarity estimates of all clusters per feature.
                   #' Can be used to achieve a global estimate as a sum of
                   #' cluster-wise similarities.
                   #'
                   #' @seealso [SMART]
                   plot = function(log = FALSE, single_cl = NULL) {
                     assert_logical(log, any.missing = FALSE, len = 1L)
                     assert_choice(single_cl, choices = colnames(self$results)[-1], null.ok = TRUE)
                     if(!is.null(self$avg)) assert_true(is.null(single_cl))
                     plot_res = if(log == TRUE) {
                       cbind(self$results[,1], log(self$results[,-1]))
                     } else{self$results}
                     if(!is.null(single_cl)){
                       p = ggplot(data = plot_res, aes(x = !!sym(single_cl), y = reorder(features, !!sym(single_cl)))) +
                         geom_bar(stat = "identity") + ylab("features")
                       return(p)
                     }
                     if(!is.null(self$avg)){
                       colnames(plot_res) = gsub("[[:punct:]]", "", colnames(plot_res))
                       p = ggplot(data = plot_res, aes(x = median, y = reorder(rn, median))) +
                         geom_bar(stat = "identity") +
                         geom_errorbar(aes(xmin = quant5, xmax = quant95), width= 0.2,
                                       position=position_dodge(0.9)) +
                         labs(y = "feature", title = "CFI with certainty quantiles",
                              caption = paste("Metric used:", self$metric, "\n", "Scores sumarized by ", self$avg))
                     } else {
                       cnames = grep("Cluster", colnames(plot_res), value = TRUE)
                       lon_res = reshape(plot_res, idvar = "features", varying = list(cnames),
                                         timevar = "Cluster", v.names = "median", direction = "long",
                                         times = cnames)
                       p = ggplot(lon_res, aes(x = median, y = reorder(features, median), fill = Cluster)) +
                         geom_bar(stat = "identity") +
                         labs(y = "feature", title = "clusterwise aggregated CFI",
                              caption = paste("Metric used:", self$metric))
                     }
                     p
                   },

                   #' @field avg (`character(1)` or `NULL`)\cr
                   #' `NULL` is calculating cluster-specific (binary)
                   #' metrics. `"micro"` summarizes binary scores to a global
                   #' score that treats each instance in the data set with equal
                   #' importance. `"macro"` summarizes binary scores to a global
                   #' score that treats each cluster with equal importance.
                   avg = NULL,

                   #' @field metric `character(1)`\cr
                   #'   The binary similarity metric used.
                   metric = NULL,

                   #' @field predictor [ClustPredictor]\cr
                   #'   The object (created with `ClustPredictor$new()`) holding
                   #'   the cluster algorithm and the data.
                   predictor = NULL,

                   #' @field data.sample [data.frame]\cr
                   #'   The data, including features and cluster soft/ hard labels.
                   data.sample = NULL,

                   #' @field sampler any\cr
                   #'   Sampler from the `predictor` object.
                   sampler = NULL,

                   #' @field features (`character or list`)\cr
                   #'   Features/ feature sets to calculate importance scores.
                   features = NULL,

                   #' @field n.repetitions (`numeric(1)`)\cr
                   #' How often is the shuffling of the feature repeated?
                   n.repetitions = NULL,

                   #' @field results (`data.table`)\cr
                   #'   A [data.table] containing the results from `SMART` procedure.
                   results = NULL
                 ),

                 private = list(
                   run = function(n) {
                     result <- NULL

                     estimate_confusion_fimp <- function(group, features, data.sample, y, metric,
                                                         n.repetitions, y.names, predictor,
                                                         halfspaces) {
                       cnames <- setdiff(colnames(data.sample), y.names)
                       num_rep <- data.table()
                       tables = list()
                       n = nrow(data.sample)
                       class_scores = data.table()
                       cat("feature", group, "\n")
                       pb = txtProgressBar(min = 0, max = self$n.repetitions, initial = 0, style=3)
                       for (repi in 1:n.repetitions) {
                         setTxtProgressBar(pb, repi * which(features == group))
                         mg <- iml:::MarginalGenerator$new(data.sample, data.sample,
                                                           features = features, n.sample.dist = 1,
                                                           y = y, cartesian = FALSE, id.dist = TRUE
                         )
                         while (!mg$finished) {
                           data.design <- mg$next.batch(n, y = TRUE)
                           if(!is.null(halfspaces)) {
                             mass = predict(halfspaces, data.design[, cnames, with = FALSE])
                             data.design = cbind(data.design, mass)
                             rows = round(nrow(data.design) * 0.7, digits = 0)
                             data.design = data.design[order(-mass), .SD[1:rows]]
                           }
                           num_rep <- rbind(num_rep, rep(repi, times = nrow(data.design)))
                           y.vec <- data.design[, y.names, with = FALSE]
                           qResults <- predictor$predict(data.table(data.design[, cnames, with = FALSE]))
                         }
                         class_scores = rbind(class_scores,
                                              t(evaluate_class(y.vec, qResults, metric = metric))
                         )
                       }
                       class_scores$features = group
                       close(pb)
                       class_scores
                     }

                     featurewise_results = mapply(estimate_confusion_fimp, group = names(self$features), features = self$features,
                                                  MoreArgs = list(data.sample = self$data.sample, y = self$sampler$y, metric = self$metric,
                                                                  n.repetitions = self$n.repetitions, y.names = self$sampler$y.names,
                                                                  predictor = self$predictor, halfspaces = NULL),
                                                  SIMPLIFY = FALSE
                     )
                     if(!is.null(self$avg)){
                       n_classes = table(self$data.sample$.y)
                       if(self$avg == "micro"){
                         results = lapply(featurewise_results, function(x) {
                           as.matrix(x[, .SD, .SDcols = names(n_classes)]) %*% as.matrix(n_classes) / sum(n_classes)
                         })}
                       if(self$avg == "macro"){
                         results = lapply(featurewise_results, function(x) {
                           apply(x[, .SD, .SDcols = names(n_classes)], 1, mean)})
                       }
                       results = t(vapply(results, function(x){
                         c("quant" = quantile(x, probs = 0.05),
                           "median" = median(x),
                           "quant" = quantile(x, probs = 0.95)
                         )}, numeric(3)))
                       return(data.table(results, keep.rownames = TRUE))
                     }
                     result <- rbindlist(unname(featurewise_results), use.names = TRUE)
                     clusters = grep(pattern = "\\d", x = colnames(result))
                     setnames(result, old = clusters, new = paste0("Cluster",  colnames(result)[clusters]))
                     clusters = colnames(result)[grep(pattern = "\\d", x = colnames(result))]
                     result[, lapply(.SD, median), .SDcols = clusters, by = list(features)]
                   }
                 )
)
