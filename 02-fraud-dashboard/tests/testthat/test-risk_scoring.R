test_that("score_one is bounded [0,100]", {
  expect_gte(score_one(0, 0, FALSE), 0)
  expect_lte(score_one(1e6, 1e6, TRUE), 100)
})

test_that("international transactions get +20 boost", {
  base    <- score_one(500, 2, FALSE)
  boosted <- score_one(500, 2, TRUE)
  expect_equal(boosted - base, 20)
})

test_that("velocity_flag triggers at 5 or more", {
  df <- data.frame(
    id = c("a","b"),
    amount = c(100, 100),
    velocity_5m = c(4, 5),
    is_international = c(FALSE, FALSE)
  )
  out <- score_transactions(df)
  expect_false(out$velocity_flag[1])
  expect_true(out$velocity_flag[2])
})

test_that("apply_thresholds keeps only flagged rows", {
  df <- data.frame(
    id = c("a","b","c"),
    risk_score = c(50, 90, 60),
    velocity_5m = c(1, 1, 10),
    is_international = c(FALSE, FALSE, FALSE)
  )
  th <- list(risk_cutoff = 85, velocity_cutoff = 5,
             flag_international = FALSE)
  out <- apply_thresholds(df, th)
  expect_setequal(out$id, c("b","c"))
})