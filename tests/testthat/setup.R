# test one: predict function ---------------------------------------------------
library(FuzzyDBScan)
library(factoextra)
multishapes <- as.data.frame(multishapes[, 1:2])
eps = c(0, 0.2)
pts = c(3, 15)
res <- FuzzyDBScan$new(multishapes, eps, pts)
predict_part = function(model, newdata) model$predict(new_data = newdata, cmatrix = FALSE)$cluster
res_predictor_part = ClustPredictor$new(res, multishapes, predict_part, res$clusters, type = "partition")
predict_prob = function(model, newdata) model$predict(new_data = newdata)
res_predictor_prob = ClustPredictor$new(res, multishapes, predict_prob, res$results, type = "prob")
# test two: mlr3cluster --------------------------------------------------------
library(mlr3cluster)
attributes_scale = attributes(scale(USArrests))
tsk_usa = TaskClust$new(id = "usarest", backend = data.table(scale(USArrests)))
c_lrn = lrn("clust.cmeans", centers = 3, predict_type = "prob")
c_lrn$train(tsk_usa)
clrn_predictor_prob = ClustPredictor$new(c_lrn, tsk_usa$data(), y = c_lrn$model$membership, type = "prob")
clrn_predictor_part = ClustPredictor$new(c_lrn, tsk_usa$data(), y = c_lrn$assignments, type = "part")
