test_that("a decided case cannot be re-decided without a new review cycle", {
  cr <- list(status = "decided", sanctions_hit = FALSE,
             sla_state = "ok", risk_tier = "low")
  expect_false(validate_decision(cr, "approve", "justification ok")$ok)
  # Reopen moves back to 'new' - then decide is allowed again
  expect_equal(next_status("decided", "reopen"), "new")
})

test_that("illegal transitions raise an error", {
  expect_error(next_status("new", "decide"), "Illegal transition")
  expect_error(next_status("decided", "assign"), "Illegal transition")
})

test_that("request_info moves to awaiting_merchant from new/assigned", {
  expect_equal(next_status("new", "request_info"), "awaiting_merchant")
  expect_equal(next_status("assigned", "request_info"), "awaiting_merchant")
  expect_equal(next_status("awaiting_merchant", "resume"), "assigned")
})

test_that("breached high-risk approvals must escalate", {
  cr <- list(status = "assigned", sanctions_hit = FALSE,
             sla_state = "breached", risk_tier = "critical")
  v <- validate_decision(cr, "approve", "justification ok here")
  expect_false(v$ok)
  expect_match(v$reason, "escalat")
})