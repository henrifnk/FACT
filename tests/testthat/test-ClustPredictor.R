test_that("equivalence", {
  expect_equal(ignore_attr = TRUE, res$predict(multishapes),
               res_predictor_prob$predict(multishapes))
  expect_equal(ignore_attr = TRUE, res$predict(multishapes, cmatrix = F)$cluster,
               unlist(res_predictor_part$predict(multishapes)))
  c_lrn$predict_type = "partition"
  expect_equal(unlist(clrn_predictor_part$predict(tsk_usa$data()), use.names = F),
               c_lrn$assignments)
  c_lrn$predict_type = "prob"
  expect_equal(clrn_predictor_part$predict(tsk_usa$data()),
               as.data.frame(c_lrn$model$membership))
})
