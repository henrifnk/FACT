#' @title Idea - Isolated Effect on Assignment
#'
#' @description
#' `IDEA` with a soft label predictor (sIDEA) \cr
#' tacks changes the soft label of being assigned to each existing cluster
#' throughout a (multidimensional) feature space
#' `IDEA` with a hard label predictor (hIDEA) \cr
#' tacks changes the soft label of being assigned to each existing cluster
#' throughout a (multidimensional) feature space
#'
#' @details
#' `IDEA` for soft labeling algorithms (sIDEA) indicates the soft label that an
#' observation \eqn{\textbf{x}} with replaced values \eqn{\tilde{\textbf{x}}_S} is assigned to
#' the k-th cluster. `IDEA` for hard labeling algorithms (hIDEA) indicates
#' the cluster assignment of an observation \eqn{\textbf{x}} with replaced values
#' \eqn{\tilde{\textbf{x}}_S}. \cr
#'
#' The global `IDEA` is denoted by the corresponding data set X:
#' \deqn{
#' \text{sIDEA}_X(\tilde{\textbf{x}}_S) = \left(\frac{1}{n} \sum_{i = 1}^n
#' \text{sIDEA}^{(1)}_{\textbf{x}^{(i)}}(\tilde{\textbf{x}}_S), \dots, \frac{1}{n}
#' \sum_{i = 1}^n \text{sIDEA}^{(k)}_{\textbf{x}^{(i)}}(\tilde{\textbf{x}}_S) \right)
#' }
#' where the c-th vector element is the average c-th vector element of local
#' sIDEA functions. The global hIDEA corresponds to:
#' \deqn{
#' \text{hIDEA}_X(\tilde{\textbf{x}}_S) =  \left(\frac{1}{n}\sum_{i = 1}^n
#' \mathbb{1}_{1}(\text{hIDEA}_{\textbf{x}^{(i)}}(\tilde{\textbf{x}}_S)), \dots,
#' \frac{1}{n}\sum_{i = 1}^n \mathbb{1}_{k}(\text{hIDEA}_{\textbf{x}^{(i)}}(\tilde{\textbf{x}}_S))\right)
#' }
#' where the c-th vector element is the fraction of hard label
#' reassignments to the c-th cluster.
#'
#'
#' @import data.table
#' @importFrom gridExtra grid.arrange
#' @import ggplot2
#'
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
#' # create soft label predictor
#' predict_prob = function(model, newdata) model$predict(new_data = newdata)
#' predictor = ClustPredictor$new(res, as.data.frame(multishapes), y = res$results,
#'                                     predict.function = predict_prob, type = "prob")
#' # Calculate `IDEA` global and local for feature "x"
#' idea_x = IDEA$new(predictor = predictor, feature = "x", grid.size = 5)
#' idea_x$plot_globals(0.5) # plot global effect of all clusters with 50 percent of local mass.
#' }
#' @seealso [iml::FeatureEffects], [iml::FeatureEffects]
#' @export
IDEA <- R6Class("IDEA",
                public = list(

                  #' @description Create an [IDEA] object.
                  #' @param predictor [ClustPredictor]\cr
                  #'   The object (created with `ClustPredictor$new()`) holding
                  #'   the cluster algorithm and the data.
                  #' @param feature (`character or list`)\cr
                  #'   For which features do you want importance scores calculated. The default
                  #'   value of `NULL` implies all features. Use a named list of character vectors
                  #'   to define groups of features for which joint importance will be calculated.
                  #' @param method `character(1)`\cr
                  #'   The `IDEA` method to be used. Possible choices for the method are:\cr
                  #'   `"g+l"` (default): store global and local `IDEA` results
                  #'
                  #'   `"local"`: store only local `IDEA` results
                  #'
                  #'   `"global"`: store only global `IDEA` results
                  #'
                  #'   `"init_local"`: store only local `IDEA` results and
                  #'   additional reference for the observations initial
                  #'   assigned cluster.
                  #'
                  #'   `"init_g+l"` store global and local `IDEA` results and
                  #'   additional reference for the observations initial
                  #'   assigned cluster.
                  #' @param grid.size `(numeric(1) or NULL)` \cr
                  #'   size of the grid to replace values. If grid size is
                  #'   given, an equidistant grid is create. If `NULL`, values
                  #'   are calculated at all present combinations of feature values.
                  #' @param noise.out any \cr
                  #'  Indicator for the noise variable. If not NULL, noise will
                  #'  be excluded from the effect estimation.
                  #' @return (data.frame)\cr
                  #'  Values for the effect curves: \cr
                  #'  One row per grid per instance for each local idea
                  #'  estimation. If `method` includes global estimation, one
                  #'  additional row per grid point.
                  initialize = function(predictor, feature, method = "g+l", grid.size = 20L, noise.out = NULL) {
                    assert_choice(method, c("global", "local", "g+l","init_local", "init_g+l"))
                    assert_numeric(grid.size, min.len = 1, null.ok = TRUE)
                    self$predictor = predictor
                    if(method %in% c("init_g+l", "init_local")) assert_true(self$type == "prob")
                    self$method = method
                    self$feature <- private$sanitize.feature(
                      feature, predictor$data$feature.names, method
                    )
                    self$mg = private$create_mg(predictor, feature, grid.size)
                    self$noise.out = noise.out
                    self$results = data.table(
                      switch(self$type, prob = private$run_prob(predictor$data$n.rows),
                             partition = private$run_part(predictor$data$n.rows))
                    )
                  },

                  #' @description Plot an [IDEA] object.
                  #' @param c indicator for the cluster to plot. If `NULL`,
                  #' all clusters are plotted.
                  #' @return (ggplot)\cr
                  #' A ggplot object that depends on the `method` chosen.
                  plot = function(c = NULL) {
                    if(self$method %in% c("init_g+l", "init_local")){
                      classes = grep("Cluster", colnames(self$results), value = TRUE)
                      if(is.null(c)) {
                        class_plots = lapply(classes, private$plot_cpdp)
                        return(do.call("grid.arrange", c(class_plots, ncol = 1)))
                      }
                      return(private$plot_cpdp(classes[c]))
                    }
                    theme_set(theme_minimal())
                    if(length(self$feature) == 1L){
                      if(self$type == "partition") {
                        return(private$plot_part_1D())
                      }
                      classes = grep("Cluster", colnames(self$results), value = TRUE)
                      if(is.null(c)) {
                        class_plots = lapply(classes, private$plot_prob_1D)
                        return(do.call("grid.arrange", c(class_plots, ncol = 1)))
                      }
                      return(private$plot_prob_1D(classes[c]))
                    } else if(length(self$feature) == 2L) {
                      return(private$plot_2D())
                    }
                  },

                  #' @description Plot the global sIDEA curves of all clusters.
                  #' @param mass between 0 and 1. The percentage of local `IDEA`
                  #' curves to plot a certainty interval.
                  #' @return (ggplot)\cr
                  #' A ggplot object.
                  plot_globals = function(mass = NULL) {
                    assert_choice(self$method, c("global", "g+l"))
                    assert_character(self$feature, len = 1L)
                    if(!is.null(mass) && self$method == "g+l"){
                      setkey(self$results, .type)
                      high = self$results[.("local"), lapply(.SD, quantile, probs = 1 - ((1 - mass)/2)),
                                          .SDcols = grep("^Cluster", colnames(self$results)), by  = c(self$feature)]
                      low = self$results[.("local"), lapply(.SD, quantile, probs = (1 - mass)/2),
                                         .SDcols = grep("^Cluster", colnames(self$results)), by  = c(self$feature)]
                      aggr = list(melt(high, id.vars = c(self$feature), value.name = "upper"),
                                  melt(low, id.vars = c(self$feature), value.name = "lower"),
                                  melt(self$results[.("global"), ][, c(".type", ".id"):= NULL],
                                       id.vars = c(self$feature), value.name = "global")
                      )
                      aggr = Reduce(function(...) merge(..., all = TRUE), aggr)
                      ribbon = geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable),
                                           linetype = 0, alpha = .2)
                      labs =labs(title = "IDEA Density Plots per cluster",
                                 subtitle = paste(mass, "% local mass included in certainty boundaries")
                      )

                    } else{
                      setkey(self$results, .type)
                      aggr = melt(self$results[.("global"), ][, c(".type", ".id.dist"):= NULL],
                                  id.vars = c(self$feature), value.name = "global")
                      ribbon = NULL
                      labs = labs(title = "IDEA Density Plots per cluster")
                    }

                    ggplot(aggr, mapping = aes(!!!list(x = sym(self$feature)),
                                               y = global, color = variable)) +
                      geom_line(linewidth = 2) +
                      scale_y_continuous(expression(f^{(k)})) +
                      geom_rug(data = self$predictor$data$get.x(),
                               mapping = aes(!!!list(x = sym(self$feature)), y = NULL, color = NULL)) +
                      ribbon + labs
                  },

                  #' @field predictor [ClustPredictor]\cr
                  #'   The object (created with `ClustPredictor$new()`) holding
                  #'   the cluster algorithm and the data.
                  predictor = NULL,

                  #' @field feature (`character or list`)\cr
                  #'   Features/ feature sets to calculate the effect curves.
                  feature = NULL,

                  #' @field method `character(1)`\cr
                  #'  The `IDEA` method to be used.
                  method = NULL,

                  #' @field mg `DataGenerator`\cr
                  #'  A `MarginalGenerator` object to sample and generate
                  #'  the pseudo instances.
                  mg = NULL,

                  #' @field results `data.table`\cr
                  #'  The `IDEA` results.
                  results = NULL,

                  #' @field noise.out any \cr
                  #'  Indicator for the noise variable.
                  noise.out = NULL
                ),


                active = list(
                  #' @field type function \cr
                  #'  Detect the type in the predictor
                  type = function() self$predictor$type
                ),
                private = list(
                  sanitize.feature = function(feature, feature.names, method) {
                    assert_character(feature, unique = TRUE)
                    stopifnot(all(feature %in% feature.names))
                    feature_idx <- which(feature[1] == feature.names)
                    if (length(feature) == 2) {
                      feature_idx <- c(feature, which(feature[2] == feature.names))
                      assert_false(feature_idx[1] == feature_idx[2])
                      if (method %in% c("local", "g+l")) {
                        stop("local is not implemented for two features.")
                      }
                    }
                    feature
                  },
                  create_mg = function(predictor, feature, grid.size){
                    data.sample = predictor$data$get.xy()
                    dta_filtered = data.sample[, feature, with = FALSE]
                    if(is.null(grid.size)){
                      grid.size = apply(dta_filtered, 2, function(x) length(unique(x)))
                    }
                    grid.dt <- iml:::get.grid(dta_filtered, grid.size, anchor.value = NULL)
                    iml:::MarginalGenerator$new(grid.dt, data.sample, feature,
                                                id.dist = TRUE, cartesian = TRUE)
                  },
                  get_mode = function(x) {
                    ux <- unique(x)
                    ux[which.max(tabulate(match(x, ux)))]
                  },
                  get_certainty = function(x) {
                    tab = table(x)
                    max(tab) / sum(tab)
                  },
                  run_prob = function(n){
                    y = vector()
                    results.local <- data.table()
                    pb = txtProgressBar(min = 0, max = self$mg$n_total, initial = 0, style=3)
                    while (!self$mg$finished) {
                      setTxtProgressBar(pb, self$mg$.__enclos_env__$private$counter)
                      results.local.inter <- self$mg$next.batch(n)
                      predictions <- self$predictor$predict(data.table(results.local.inter[, self$predictor$data$feature.names, with = FALSE]))
                      colnames(predictions) = paste0("Cluster", self$predictor$cnames)
                      y = c(y, paste("Cluster", results.local.inter$.y))
                      results.local.inter <- results.local.inter[, c(self$feature, ".id.dist"), with = FALSE]
                      results.local.inter = cbind(results.local.inter, predictions)
                      results.local <- rbind(results.local, results.local.inter)
                    }
                    results = data.table()
                    cluster_names = colnames(predictions)
                    if(!is.null(self$noise.out)) {
                      noise_c = paste0("Cluster", self$noise.out)
                      results.local = results.local[, !noise_c, with = F]
                      cluster_names = colnames(predictions)[colnames(predictions) != noise_c]
                    }

                    if(self$method %in% c("init_g+l", "init_local")) results.local = cbind(results.local, cluster = y)
                    if (self$method %in% c("global", "g+l", "init_g+l")) {
                      results.aggregated = if(self$method != "init_g+l") {
                        results.local[, lapply(.SD, mean),
                                      .SDcols = cluster_names,
                                      by  = c(self$feature)]
                      } else {
                        results.aggregated <- results.local[, lapply(.SD, mean),
                                                            .SDcols = cluster_names,
                                                            by  = c(self$feature, "cluster")]
                      }
                      results.aggregated$.type <- "global"
                      results.aggregated$.id.dist = NA
                      results <- rbind(results, results.aggregated)
                    }

                    if (self$method %in% c("local", "g+l", "init_g+l", "init_local")) {
                      results.local$.type <- "local"
                      cols = c(self$feature, cluster_names, ".type", ".id.dist")
                      if(self$method %in% c("init_g+l", "init_local")) cols = c(cols, "cluster")
                      results <- rbind(results, results.local[, ..cols], fill = TRUE)
                      results$.id <- results$.id.dist
                      results$.id.dist <- NULL
                      # sort by id
                      setkeyv(results, ".id")
                    }
                    close(pb)
                    data.table(results)
                  },
                  run_part = function(n){
                    results.local <- data.table()
                    pb = txtProgressBar(min = 0, max = self$mg$n_total, initial = 0, style=3)
                    while (!self$mg$finished) {
                      setTxtProgressBar(pb, self$mg$.__enclos_env__$private$counter)
                      results.local.inter <- self$mg$next.batch(n)
                      predictions <- self$predictor$predict(data.table(results.local.inter[, self$predictor$data$feature.names, with = FALSE]))
                      results.local.inter <- results.local.inter[, c(self$feature, ".id.dist"), with = FALSE]
                      results.local.inter$.value <- paste("cluster", unlist(predictions))
                      results.local.inter$.class <- 1
                      results.local <- rbind(results.local, results.local.inter)
                    }
                    if(!is.null(self$noise.out)) {
                      noise_c = paste("cluster", self$noise.out)
                      setkey(results.local, .value)
                      results.local = results.local[! noise_c]
                    }
                    results <- data.table()
                    if (self$method %in% c("global", "g+l")) {
                      results.aggregated <- results.local[, list(.value = private$get_mode(.value),
                                                                 .cert = private$get_certainty(.value)),
                                                          by = c(self$feature)]
                      results.aggregated[, setdiff(colnames(results.local), colnames(results.aggregated)):= NA]
                      results.aggregated[, colnames(results.local), with = FALSE]
                      results.aggregated$.type <- "global"
                      results <- rbind(results, results.aggregated)
                    }

                    if (self$method %in% c("local", "g+l")) {
                      results.local$.type <- "local"
                      results <- rbind(results, results.local, fill = TRUE)
                      results$.id <- results$.id.dist
                      results$.id.dist <- NULL
                      # sort by id
                      setkeyv(results, ".id")
                    }
                    close(pb)
                    data.table(results)
                  },
                  plot_cpdp = function(target){
                    if(self$method == "init_g+l") global <- self$results[self$results$.type != "local", ]
                    local = self$results[self$results$.type == "local", ]
                    ggplot(local, mapping = aes(!!!list(x = sym(self$feature), y = sym(target)), colour = cluster)) +
                      geom_rug(data = self$predictor$data$get.x(),
                               mapping = aes(!!!list(x = sym(self$feature)), y = NULL, colour = NULL)) +
                      geom_line(alpha = 0.2, mapping = aes(group = .id)) +
                      if(self$method == "init_g+l") geom_line(data = global, linewidth = 2) else NULL
                  },
                  plot_part_1D = function(){
                    stopifnot(exprObject = self$method %in% c("global", "g+l"))
                    aggr <- self$results[self$results$.type != "local", ]
                    ggplot(aggr, aes(!!!list(x= sym(self$feature)), y = .cert)) +
                      geom_ribbon(aes(ymin = rep(0, times = nrow(self$results)),
                                      ymax = .cert, fill = as.factor(.value)), linetype = 0) +
                      geom_rug(data = self$predictor$data$get.x(), mapping = aes(!!!list(x = sym(self$feature)), y = NULL)) +
                      scale_fill_discrete(name = "Marginal Cluster")
                  },
                  plot_prob_1D = function(target){
                    curve = ggplot(self$results[self$results$.type == "local", ],
                                   mapping = aes(!!!list(x = sym(self$feature), y = sym(target)))) +
                      geom_rug(data = self$predictor$data$get.x(),
                               mapping = aes(!!!list(x = sym(self$feature)), y = NULL))
                    if(self$method %in% c("local", "g+l")) {
                      curve = curve + geom_line(alpha = 0.2, mapping = aes(group = .id))
                    }
                    if(self$method %in% c("global", "g+l")) {
                      global <- self$results[self$results$.type != "local", ]
                      curve = curve + geom_line(data = global, linewidth = 2, color = "gold")
                    }
                    curve
                  },
                  plot_2D = function(){
                    if(self$type == "prob") {
                      cols = grep("Cluster", names(self$results), value = TRUE)
                      self$results$.value = apply(self$results[, ..cols], 1, which.max)
                      self$results$.cert = apply(self$results[, ..cols], 1, max)
                    }
                    self$results$.value = as.factor(self$results$.value)
                    grid_p = ggplot(self$results, aes(!!!list(x = sym(self$feature[1]), y = sym(self$feature[2])),
                                                      alpha = .cert, fill = .value)) +
                      geom_tile() + geom_rug(data = self$predictor$data$get.x(), mapping = aes(alpha= NULL, fill = NULL))
                    grid_p
                  }
                )
)
