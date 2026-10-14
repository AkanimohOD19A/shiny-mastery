test_that("reviewed IDs accumulate and are marked", {
  ids <- c("TXN001", "TXN002")
  reviewed <- character()
  reviewed <- c(reviewed, ids[1])
  expect_true("TXN001" %in% reviewed)
  expect_false("TXN002" %in% reviewed)
})