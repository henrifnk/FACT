#' @title Evaluate Class
#' @description Calculation of binary similarity metric based on confusion matrix.
#' @param actual `numeric` \cr
#'  initial cluster assignments
#' @param predicted `numeric` \cr
#'  cluster assignments of permuted data
#' @param metric `character(1)` \cr
#'  binary score metric
#' @return A binary score for each of the clusters and the number of instances.
evaluate_class = function(actual, predicted, metric = "f1"){
  cm = calculate_confusion(actual, predicted)
  n = sum(cm)
  nc = nrow(cm)
  diag = diag(cm)
  rowsums = apply(cm, 1, sum)
  colsums = apply(cm, 2, sum)
  p = rowsums / n
  q = colsums / n
  rec = diag / rowsums
  prec = diag / colsums
  expAccuracy = sum(p*q)
  score = switch(metric,
                 recall = rec,
                 precision = prec,
                 f1 = {a = 2 * prec * rec / (prec + rec)
                   a[is.nan(a)] = 0
                   a},
                 jaccard = diag / rowsums + colsums - diag,
                 folkes_mallows = sqrt((diag / colsums) * (diag / rowsums)),
                 accuracy = {
                   a = rep(sum(diag), times = length(diag)) / n
                   names(a) = 1:length(diag)
                   a}
  )
  c(score, n = n)
}
# utilities---------------------------------------------------------------------
#' @describeIn evaluate_class Calculate confusion matrix
calculate_confusion = function(actual, predicted) {
  naVals = union(which(is.na(actual)), which(is.na(predicted)))
  if(length(naVals) > 0) {
   actual = actual[-naVals]
   predicted = predicted[-naVals]
  }
  predi = factor(unlist(predicted), levels = sort(unique(unlist(actual))))
  as.matrix(table(Actual = unlist(actual), Predicted = predi))
}
