test_that("SLA clock pauses while awaiting merchant", {
  opened <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  due_no_pause <- sla_due_at(opened, "high", "assigned")
  due_paused   <- sla_due_at(opened, "high", "awaiting_merchant",
                             paused_at  = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
                             resumed_at = as.POSIXct("2026-01-04 00:00:00", tz = "UTC"))
  expect_equal(as.numeric(difftime(due_paused, due_no_pause, units = "hours")),
               48)
})

test_that("sla_state flags breached and at_risk correctly", {
  expect_equal(sla_state(-1),  "breached")
  expect_equal(sla_state(4),   "at_risk")
  expect_equal(sla_state(48),  "ok")
})