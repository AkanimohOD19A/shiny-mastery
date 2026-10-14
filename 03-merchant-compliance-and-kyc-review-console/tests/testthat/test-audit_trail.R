test_that("audit trail length strictly increases on every append", {
  t0 <- audit_trail_init()
  t1 <- audit_append(t0, "u1", "C1", "assign", "new", "assigned", "")
  t2 <- audit_append(t1, "u1", "C1", "decide", "assigned", "decided", "ok")
  expect_equal(nrow(t0), 0)
  expect_equal(nrow(t1), 1)
  expect_equal(nrow(t2), 2)
  # t0 unchanged - pure
  expect_equal(nrow(t0), 0)
})

test_that("audit_append validates schema", {
  bad <- data.frame(foo = 1)
  expect_error(audit_append(bad, "u", "C1", "x", "a", "b", ""),
               "missing required columns")
})