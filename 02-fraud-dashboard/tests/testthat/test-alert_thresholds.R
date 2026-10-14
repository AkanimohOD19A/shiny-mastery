test_that("threshold edge: score exactly at cutoff is included", {
  df <- data.frame(id="x", risk_score=85, velocity_5m=1,
                   is_international=FALSE)
  th <- list(risk_cutoff=85, velocity_cutoff=5, flag_international=FALSE)
  expect_equal(nrow(apply_thresholds(df, th)), 1)
})

test_that("flag_international toggles inclusion", {
  df <- data.frame(id="x", risk_score=10, velocity_5m=1,
                   is_international=TRUE)
  th_on  <- list(risk_cutoff=85, velocity_cutoff=5, flag_international=TRUE)
  th_off <- list(risk_cutoff=85, velocity_cutoff=5, flag_international=FALSE)
  expect_equal(nrow(apply_thresholds(df, th_on)), 1)
  expect_equal(nrow(apply_thresholds(df, th_off)), 0)
})